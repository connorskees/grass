use codemap::{Span, Spanned};

use peekmore::{PeekMore, PeekMoreIterator};

use crate::error::SassResult;
use crate::scope::Scope;
use crate::selector::Selector;
use crate::utils::{
    devour_whitespace, read_until_closing_curly_brace, read_until_open_curly_brace,
    read_until_semicolon_or_closing_curly_brace,
};
use crate::value::Value;
use crate::{RuleSet, Stmt, Token};

use each_rule::{parse_each, Each};
use for_rule::For;
pub(crate) use function::Function;
pub(crate) use if_rule::If;
pub(crate) use kind::AtRuleKind;
pub(crate) use mixin::{eat_include, Mixin};
use parse::{eat_stmts, eat_stmts_at_root, ruleset_eval};
use unknown::UnknownAtRule;
use while_rule::{parse_while, While};

mod each_rule;
mod for_rule;
mod function;
mod if_rule;
mod kind;
mod mixin;
mod parse;
mod unknown;
mod while_rule;

#[derive(Debug, Clone)]
pub(crate) enum AtRule {
    Warn(Spanned<String>),
    Debug(Spanned<String>),
    Mixin(String, Box<Mixin>),
    Function(String, Box<Function>),
    Return(Vec<Token>),
    Charset,
    Content,
    Unknown(UnknownAtRule),
    For(For),
    Each(Each),
    While(While),
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
                    node: AtRule::Warn(Spanned {
                        node: message.to_css_string(span)?,
                        span,
                    }),
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
                    node: AtRule::Debug(Spanned {
                        node: message.inspect(span)?,
                        span,
                    }),
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
            AtRuleKind::Each => Spanned {
                node: parse_each(toks, scope, super_selector, kind_span)?,
                span: kind_span,
            },
            AtRuleKind::If => Spanned {
                node: AtRule::If(If::from_tokens(toks)?),
                span: kind_span,
            },
            AtRuleKind::For => Spanned {
                node: for_rule::parse_for(toks, scope, super_selector, kind_span)?,
                span: kind_span,
            },
            AtRuleKind::While => parse_while(toks, kind_span)?,
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
            AtRuleKind::Import => todo!("@import not yet implemented"),
            AtRuleKind::Forward => todo!("@forward not yet implemented"),
            AtRuleKind::Supports => todo!("@supports not yet implemented"),
            AtRuleKind::Keyframes => todo!("@keyframes not yet implemented"),
            AtRuleKind::Extend => todo!("@extend not yet implemented"),
            AtRuleKind::Use => todo!("@use not yet implemented"),
        })
    }
}
