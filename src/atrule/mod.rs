use codemap::{Span, Spanned};

use peekmore::{PeekMore, PeekMoreIterator};

use crate::common::{Brackets, ListSeparator};
use crate::error::SassResult;
use crate::scope::Scope;
use crate::selector::Selector;
use crate::utils::{
    devour_whitespace, eat_ident, read_until_closing_curly_brace, read_until_open_curly_brace,
    read_until_semicolon_or_closing_curly_brace,
};
use crate::value::Value;
use crate::{RuleSet, Stmt, Token};

pub(crate) use function::Function;
pub(crate) use if_rule::If;
pub(crate) use kind::AtRuleKind;
pub(crate) use mixin::{eat_include, Mixin};
use parse::{eat_stmts, eat_stmts_at_root};
use unknown::UnknownAtRule;

mod for_rule;
mod function;
mod if_rule;
mod kind;
mod mixin;
mod parse;
mod unknown;

#[derive(Debug, Clone)]
pub(crate) enum AtRule {
    Warn(String),
    Debug(String),
    Mixin(String, Box<Mixin>),
    Function(String, Box<Function>),
    Return(Vec<Token>),
    Charset,
    Content,
    Unknown(UnknownAtRule),
    For(Vec<Spanned<Stmt>>),
    Each(Vec<Spanned<Stmt>>),
    While(Vec<Spanned<Stmt>>),
    Include(Vec<Spanned<Stmt>>),
    If(If),
    AtRoot(Vec<Spanned<Stmt>>),
}

impl AtRule {
    pub fn from_tokens<I: Iterator<Item = Token>>(
        rule: &AtRuleKind,
        kind_span: Span,
        toks: &mut PeekMoreIterator<I>,
        scope: &mut Scope,
        super_selector: &Selector,
    ) -> SassResult<Spanned<AtRule>> {
        devour_whitespace(toks);
        Ok(match rule {
            AtRuleKind::Error => {
                let Spanned {
                    node: message,
                    span,
                } = Value::from_vec(
                    read_until_semicolon_or_closing_curly_brace(toks),
                    scope,
                    super_selector,
                )?;

                return Err((message.to_css_string(span)?, span.merge(kind_span)).into());
            }
            AtRuleKind::Warn => {
                let Spanned {
                    node: message,
                    span,
                } = Value::from_vec(
                    read_until_semicolon_or_closing_curly_brace(toks),
                    scope,
                    super_selector,
                )?;
                span.merge(kind_span);
                if toks.peek().unwrap().kind == ';' {
                    kind_span.merge(toks.next().unwrap().pos());
                }
                devour_whitespace(toks);
                Spanned {
                    node: AtRule::Warn(message.to_css_string(span)?),
                    span,
                }
            }
            AtRuleKind::Debug => {
                let Spanned {
                    node: message,
                    span,
                } = Value::from_vec(
                    read_until_semicolon_or_closing_curly_brace(toks),
                    scope,
                    super_selector,
                )?;
                span.merge(kind_span);
                if toks.peek().unwrap().kind == ';' {
                    kind_span.merge(toks.next().unwrap().pos());
                }
                devour_whitespace(toks);
                Spanned {
                    node: AtRule::Debug(message.inspect(span)?),
                    span,
                }
            }
            AtRuleKind::Mixin => {
                let Spanned {
                    node: (name, mixin),
                    span,
                } = Mixin::decl_from_tokens(toks, scope, super_selector)?;
                Spanned {
                    node: AtRule::Mixin(name, Box::new(mixin)),
                    span,
                }
            }
            AtRuleKind::Function => {
                let (name, func) = Function::decl_from_tokens(toks, scope.clone(), super_selector)?;
                Spanned {
                    node: AtRule::Function(name, Box::new(func)),
                    span: kind_span,
                }
            }
            AtRuleKind::Return => {
                let v = read_until_semicolon_or_closing_curly_brace(toks);
                if toks.peek().unwrap().kind == ';' {
                    toks.next();
                }
                devour_whitespace(toks);
                Spanned {
                    node: AtRule::Return(v),
                    span: kind_span,
                }
            }
            AtRuleKind::Use => todo!("@use not yet implemented"),
            AtRuleKind::AtRoot => {
                let mut selector = &Selector::replace(
                    super_selector,
                    Selector::from_tokens(
                        &mut read_until_open_curly_brace(toks).into_iter().peekmore(),
                        scope,
                        super_selector,
                    )?,
                );
                let mut is_some = true;
                if selector.is_empty() {
                    is_some = false;
                    selector = super_selector;
                }
                toks.next();
                devour_whitespace(toks);
                let mut body = read_until_closing_curly_brace(toks);
                body.push(toks.next().unwrap());
                devour_whitespace(toks);
                let mut styles = Vec::new();
                let raw_stmts = eat_stmts_at_root(
                    &mut body.into_iter().peekmore(),
                    scope,
                    selector,
                    0,
                    is_some,
                )?
                .into_iter()
                .filter_map(|s| match s.node {
                    Stmt::Style(..) => {
                        styles.push(s);
                        None
                    }
                    _ => Some(s),
                })
                .collect::<Vec<Spanned<Stmt>>>();
                let mut stmts = vec![Spanned {
                    node: Stmt::RuleSet(RuleSet {
                        selector: selector.clone(),
                        rules: styles,
                        super_selector: Selector::new(),
                    }),
                    span: kind_span,
                }];
                stmts.extend(raw_stmts);
                Spanned {
                    node: AtRule::AtRoot(stmts),
                    span: kind_span,
                }
            }
            AtRuleKind::Charset => {
                read_until_semicolon_or_closing_curly_brace(toks);
                if toks.peek().unwrap().kind == ';' {
                    toks.next();
                }
                devour_whitespace(toks);
                Spanned {
                    node: AtRule::Charset,
                    span: kind_span,
                }
            }
            AtRuleKind::Each => {
                let mut stmts = Vec::new();
                devour_whitespace(toks);
                let mut vars = Vec::new();
                let mut span = kind_span;
                loop {
                    let next = toks.next().ok_or(("expected \"$\".", span))?;
                    span = next.pos();
                    match next.kind {
                        '$' => vars.push(eat_ident(toks, scope, super_selector)?),
                        _ => return Err(("expected \"$\".", next.pos()).into()),
                    }
                    devour_whitespace(toks);
                    if toks
                        .peek()
                        .ok_or(("expected \"$\".", vars[vars.len() - 1].span))?
                        .kind
                        == ','
                    {
                        toks.next();
                        devour_whitespace(toks);
                    } else {
                        break;
                    }
                }
                if toks.peek().is_none() {
                    todo!()
                }
                let i = eat_ident(toks, scope, super_selector)?;
                if i.node.to_ascii_lowercase() != "in" {
                    return Err(("Expected \"in\".", i.span).into());
                }
                devour_whitespace(toks);
                let iter_val =
                    Value::from_vec(read_until_open_curly_brace(toks), scope, super_selector)?;
                let iterator = match iter_val.node.eval(iter_val.span)?.node {
                    Value::List(v, ..) => v,
                    Value::Map(m) => m
                        .into_iter()
                        .map(|(k, v)| Value::List(vec![k, v], ListSeparator::Space, Brackets::None))
                        .collect(),
                    v => vec![v],
                };
                toks.next();
                devour_whitespace(toks);
                let mut body = read_until_closing_curly_brace(toks);
                body.push(toks.next().unwrap());
                devour_whitespace(toks);

                for row in iterator {
                    let this_iterator = match row {
                        Value::List(v, ..) => v,
                        Value::Map(m) => m
                            .into_iter()
                            .map(|(k, v)| {
                                Value::List(vec![k, v], ListSeparator::Space, Brackets::None)
                            })
                            .collect(),
                        v => vec![v],
                    };

                    if vars.len() == 1 {
                        scope.insert_var(
                            &vars[0],
                            Spanned {
                                node: Value::List(
                                    this_iterator,
                                    ListSeparator::Space,
                                    Brackets::None,
                                ),
                                span: vars[0].span,
                            },
                        )?;
                    } else {
                        for (var, val) in vars.clone().into_iter().zip(
                            this_iterator
                                .into_iter()
                                .chain(std::iter::once(Value::Null).cycle()),
                        ) {
                            scope.insert_var(&var, Spanned { node: val, span })?;
                        }
                    }

                    stmts.extend(eat_stmts(
                        &mut body.clone().into_iter().peekmore(),
                        scope,
                        super_selector,
                    )?);
                }
                Spanned {
                    node: AtRule::Each(stmts),
                    span: kind_span,
                }
            }
            AtRuleKind::Extend => todo!("@extend not yet implemented"),
            AtRuleKind::If => Spanned {
                node: AtRule::If(If::from_tokens(toks)?),
                span: kind_span,
            },
            AtRuleKind::Else => todo!("@else not yet implemented"),
            AtRuleKind::For => Spanned {
                node: for_rule::parse_for(toks, scope, super_selector, kind_span)?,
                span: kind_span,
            },
            AtRuleKind::While => {
                let mut stmts = Vec::new();
                devour_whitespace(toks);
                let cond = read_until_open_curly_brace(toks);

                if cond.is_empty() {
                    return Err(("Expected expression.", kind_span).into());
                }

                toks.next();
                let scope = &mut scope.clone();
                let body = read_until_closing_curly_brace(toks);
                toks.next();

                devour_whitespace(toks);

                let mut val = Value::from_vec(cond.clone(), scope, super_selector)?;
                while val.node.is_true(val.span)? {
                    stmts.extend(eat_stmts(
                        &mut body.clone().into_iter().peekmore(),
                        scope,
                        super_selector,
                    )?);
                    val = Value::from_vec(cond.clone(), scope, super_selector)?;
                }
                Spanned {
                    node: AtRule::While(stmts),
                    span: kind_span,
                }
            }
            AtRuleKind::Keyframes => todo!("@keyframes not yet implemented"),
            AtRuleKind::Unknown(name) => Spanned {
                node: AtRule::Unknown(UnknownAtRule::from_tokens(
                    toks,
                    name,
                    scope,
                    super_selector,
                    kind_span,
                )?),
                span: kind_span,
            },
            AtRuleKind::Content => Spanned {
                node: AtRule::Content,
                span: kind_span,
            },
            AtRuleKind::Include => Spanned {
                node: AtRule::Include(eat_include(toks, scope, super_selector)?),
                span: kind_span,
            },
            _ => todo!("encountered unimplemented at rule"),
        })
    }
}
