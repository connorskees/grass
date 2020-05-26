use std::convert::TryFrom;
use std::fs;
use std::iter::Iterator;
use std::path::Path;

use codemap::{CodeMap, Span, Spanned};

use peekmore::{PeekMore, PeekMoreIterator};

#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

use crate::atrule::{eat_include, AtRule, AtRuleKind};
use crate::error::{SassError, SassResult};
use crate::imports::import;
use crate::lexer::Lexer;
use crate::output::Css;
use crate::scope::{
    global_var_exists, insert_global_fn, insert_global_mixin, insert_global_var, Scope,
    GLOBAL_SCOPE,
};
use crate::selector::Selector;
use crate::token::Token;
use crate::utils::{
    devour_whitespace, eat_comment, eat_ident, eat_variable_value, parse_quoted_string,
    peek_ident_no_interpolation, peek_whitespace, read_until_newline, VariableDecl,
};
use crate::{eat_expr, Expr, RuleSet, Stmt};

/// Represents a parsed SASS stylesheet with nesting
#[cfg_attr(feature = "wasm", wasm_bindgen)]
#[derive(Debug, Clone)]
pub struct StyleSheet(pub(crate) Vec<Spanned<Stmt>>);

#[cfg(feature = "wasm")]
#[wasm_bindgen]
impl StyleSheet {
    pub fn new(input: String) -> Result<String, JsValue> {
        let mut map = CodeMap::new();
        let file = map.add_file("stdin".into(), input);
        Ok(Css::from_stylesheet(StyleSheet(
            StyleSheetParser {
                lexer: &mut Lexer::new(&file).peekmore(),
                nesting: 0,
                map: &mut map,
                path: Path::new(""),
            }
            .parse_toplevel()
            .map_err(|e| raw_to_parse_error(&map, e).to_string())?
            .0,
        ))
        .map_err(|e| raw_to_parse_error(&map, e).to_string())?
        .pretty_print(&map)
        .map_err(|e| raw_to_parse_error(&map, e).to_string())?)
    }
}

fn raw_to_parse_error(map: &CodeMap, err: SassError) -> SassError {
    let (message, span) = err.raw();
    SassError::from_loc(message, map.look_up_span(span))
}

impl StyleSheet {
    /// Write CSS to `buf`, constructed from a string
    ///
    /// ```
    /// use grass::{SassResult, StyleSheet};
    ///
    /// fn main() -> SassResult<()> {
    ///     let sass = StyleSheet::new("a { b { color: red; } }".to_string())?;
    ///     assert_eq!(sass, "a b {\n  color: red;\n}\n");
    ///     Ok(())
    /// }
    /// ```
    #[cfg_attr(feature = "profiling", inline(never))]
    #[cfg_attr(not(feature = "profiling"), inline)]
    #[cfg(not(feature = "wasm"))]
    pub fn new(input: String) -> SassResult<String> {
        let mut map = CodeMap::new();
        let file = map.add_file("stdin".into(), input);
        Css::from_stylesheet(StyleSheet(
            StyleSheetParser {
                lexer: &mut Lexer::new(&file).peekmore(),
                nesting: 0,
                map: &mut map,
                path: Path::new(""),
            }
            .parse_toplevel()
            .map_err(|e| raw_to_parse_error(&map, e))?
            .0,
        ))
        .map_err(|e| raw_to_parse_error(&map, e))?
        .pretty_print(&map)
        .map_err(|e| raw_to_parse_error(&map, e))
    }

    /// Write CSS to `buf`, constructed from a path
    ///
    /// ```
    /// use grass::{SassResult, StyleSheet};
    ///
    /// fn main() -> SassResult<()> {
    ///     let sass = StyleSheet::from_path("input.scss")?;
    ///     Ok(())
    /// }
    /// ```
    #[cfg_attr(feature = "profiling", inline(never))]
    #[cfg_attr(not(feature = "profiling"), inline)]
    #[cfg(not(feature = "wasm"))]
    pub fn from_path(p: &str) -> SassResult<String> {
        let mut map = CodeMap::new();
        let file = map.add_file(p.into(), String::from_utf8(fs::read(p)?)?);
        Css::from_stylesheet(StyleSheet(
            StyleSheetParser {
                lexer: &mut Lexer::new(&file).peekmore(),
                nesting: 0,
                map: &mut map,
                path: p.as_ref(),
            }
            .parse_toplevel()
            .map_err(|e| raw_to_parse_error(&map, e))?
            .0,
        ))
        .map_err(|e| raw_to_parse_error(&map, e))?
        .pretty_print(&map)
        .map_err(|e| raw_to_parse_error(&map, e))
    }

    pub(crate) fn export_from_path<P: AsRef<Path> + Into<String> + Clone>(
        p: &P,
        map: &mut CodeMap,
    ) -> SassResult<(Vec<Spanned<Stmt>>, Scope)> {
        let file = map.add_file(p.clone().into(), String::from_utf8(fs::read(p)?)?);
        Ok(StyleSheetParser {
            lexer: &mut Lexer::new(&file).peekmore(),
            nesting: 0,
            map,
            path: p.as_ref(),
        }
        .parse_toplevel()?)
    }

    pub(crate) fn from_stmts(s: Vec<Spanned<Stmt>>) -> StyleSheet {
        StyleSheet(s)
    }
}

struct StyleSheetParser<'a> {
    lexer: &'a mut PeekMoreIterator<Lexer<'a>>,
    nesting: u32,
    map: &'a mut CodeMap,
    path: &'a Path,
}

impl<'a> StyleSheetParser<'a> {
    fn parse_toplevel(mut self) -> SassResult<(Vec<Spanned<Stmt>>, Scope)> {
        let mut rules: Vec<Spanned<Stmt>> = Vec::new();
        devour_whitespace(self.lexer);
        while let Some(Token { kind, .. }) = self.lexer.peek() {
            match kind {
                'a'..='z'
                | 'A'..='Z'
                | '0'..='9'
                | '['
                | '\\'
                | ']'
                | '^'
                | '_'
                | '-'
                | '#'
                | ':'
                | '*'
                | '%'
                | '.'
                | '>'
                | '+'
                | '='
                | ','
                | '('
                | ')'
                | '<'
                | '?'
                | '~'
                | '|'
                | '`'
                | '\''
                | '"'
                | '\u{7f}'..=std::char::MAX => {
                    rules.extend(self.eat_rules(&Selector::new(), &mut Scope::new())?)
                }
                '\t' | '\n' | ' ' | ';' => {
                    self.lexer.next();
                    continue;
                }
                '$' => {
                    let span_before = self.lexer.next().unwrap().pos;
                    let name = peek_ident_no_interpolation(self.lexer, false, span_before)?;
                    let whitespace = peek_whitespace(self.lexer);

                    match self.lexer.peek() {
                        Some(Token { kind: ':', pos }) => {
                            let pos = *pos;
                            self.lexer
                                .take(name.node.chars().count() + whitespace + 1)
                                .for_each(drop);
                            devour_whitespace(self.lexer);

                            let VariableDecl { val, default, .. } = eat_variable_value(
                                self.lexer,
                                &Scope::new(),
                                &Selector::new(),
                                pos,
                            )?;

                            if !(default && global_var_exists(&name.node)) {
                                insert_global_var(&name.node, val)?;
                            }
                        }
                        Some(..) | None => return Err(("expected \":\".", name.span).into()),
                    }
                }
                '/' => {
                    let pos = self.lexer.next().unwrap().pos;
                    match self.lexer.next() {
                        Some(Token { kind: '/', .. }) => {
                            read_until_newline(self.lexer);
                            devour_whitespace(self.lexer);
                        }
                        Some(Token { kind: '*', .. }) => {
                            let comment = eat_comment(self.lexer, &Scope::new(), &Selector::new())?;
                            rules.push(comment.map_node(Stmt::MultilineComment));
                        }
                        _ => return Err(("expected selector.", pos).into()),
                    }
                }
                '@' => {
                    let span_before = self.lexer.next().unwrap().pos();
                    let rule = eat_ident(self.lexer, &Scope::new(), &Selector::new(), span_before)?;
                    match AtRuleKind::try_from(&rule)? {
                        AtRuleKind::Include => rules.extend(eat_include(
                            self.lexer,
                            &Scope::new(),
                            &Selector::new(),
                            None,
                            rule.span,
                        )?),
                        AtRuleKind::Import => {
                            devour_whitespace(self.lexer);
                            let mut file_name = String::new();
                            let next = match self.lexer.next() {
                                Some(v) => v,
                                None => todo!("expected input after @import"),
                            };
                            match next.kind {
                                q @ '"' | q @ '\'' => {
                                    file_name.push_str(
                                        &parse_quoted_string(
                                            self.lexer,
                                            &Scope::new(),
                                            q,
                                            &Selector::new(),
                                            next.pos,
                                        )?
                                        .node
                                        .unquote()
                                        .to_css_string(rule.span)?,
                                    );
                                }
                                _ => return Err(("Expected string.", next.pos()).into()),
                            }
                            if let Some(t) = self.lexer.peek() {
                                if t.kind == ';' {
                                    self.lexer.next();
                                }
                            }

                            devour_whitespace(self.lexer);

                            let (new_rules, new_scope) =
                                import(self.path, file_name.as_ref(), &mut self.map)?;
                            rules.extend(new_rules);
                            GLOBAL_SCOPE.with(|s| {
                                s.borrow_mut().extend(new_scope);
                            });
                        }
                        v => {
                            let rule = AtRule::from_tokens(
                                v,
                                rule.span,
                                self.lexer,
                                &mut Scope::new(),
                                &Selector::new(),
                                None,
                            )?;
                            match rule.node {
                                AtRule::Mixin(name, mixin) => {
                                    insert_global_mixin(&name, *mixin);
                                }
                                AtRule::Function(name, func) => {
                                    insert_global_fn(&name, *func);
                                }
                                AtRule::Charset => continue,
                                AtRule::Warn(message) => self.warn(rule.span, &message),
                                AtRule::Debug(message) => self.debug(rule.span, &message),
                                AtRule::Return(_) => {
                                    return Err(
                                        ("This at-rule is not allowed here.", rule.span).into()
                                    )
                                }
                                AtRule::For(f) => rules.extend(f.ruleset_eval(
                                    &mut Scope::new(),
                                    &Selector::new(),
                                    None,
                                )?),
                                AtRule::While(w) => rules.extend(w.ruleset_eval(
                                    &mut Scope::new(),
                                    &Selector::new(),
                                    true,
                                    None,
                                )?),
                                AtRule::Each(e) => rules.extend(e.ruleset_eval(
                                    &mut Scope::new(),
                                    &Selector::new(),
                                    None,
                                )?),
                                AtRule::Include(s) => rules.extend(s),
                                AtRule::Content => {
                                    return Err((
                                        "@content is only allowed within mixin declarations.",
                                        rule.span,
                                    )
                                        .into())
                                }
                                AtRule::If(i) => {
                                    rules.extend(i.eval(
                                        &mut Scope::new(),
                                        &Selector::new(),
                                        None,
                                    )?);
                                }
                                AtRule::AtRoot(root_rules) => rules.extend(root_rules),
                                AtRule::Unknown(..) | AtRule::Media(..) | AtRule::Keyframes(..) => {
                                    rules.push(rule.map_node(Stmt::AtRule))
                                }
                            }
                        }
                    }
                }
                '&' => {
                    return Err((
                        "Top-level selectors may not contain the parent selector \"&\".",
                        self.lexer.next().unwrap().pos(),
                    )
                        .into())
                }
                '\u{0}'..='\u{8}' | '\u{b}'..='\u{1f}' => {
                    return Err(("expected selector.", self.lexer.next().unwrap().pos).into());
                }
                '{' | '!' => {
                    return Err(("expected \"}\".", self.lexer.next().unwrap().pos).into());
                }
                '}' => return Err(("unmatched \"}\".", self.lexer.next().unwrap().pos).into()),
            };
        }
        Ok((rules, GLOBAL_SCOPE.with(|s| s.borrow().clone())))
    }

    fn eat_rules(
        &mut self,
        super_selector: &Selector,
        scope: &mut Scope,
    ) -> SassResult<Vec<Spanned<Stmt>>> {
        let mut stmts = Vec::new();
        while let Some(expr) = eat_expr(self.lexer, scope, super_selector, None)? {
            let span = expr.span;
            match expr.node {
                Expr::Style(s) => stmts.push(Spanned {
                    node: Stmt::Style(s),
                    span,
                }),
                Expr::AtRule(a) => match a {
                    AtRule::For(f) => stmts.extend(f.ruleset_eval(scope, super_selector, None)?),
                    AtRule::While(w) => {
                        stmts.extend(w.ruleset_eval(scope, super_selector, false, None)?)
                    }
                    AtRule::Each(e) => stmts.extend(e.ruleset_eval(scope, super_selector, None)?),
                    AtRule::Include(s) => stmts.extend(s),
                    AtRule::If(i) => stmts.extend(i.eval(scope, super_selector, None)?),
                    AtRule::Content => {
                        return Err((
                            "@content is only allowed within mixin declarations.",
                            expr.span,
                        )
                            .into())
                    }
                    AtRule::Return(..) => {
                        return Err(("This at-rule is not allowed here.", expr.span).into())
                    }
                    AtRule::AtRoot(root_stmts) => stmts.extend(root_stmts),
                    AtRule::Debug(ref message) => self.debug(expr.span, message),
                    AtRule::Warn(ref message) => self.warn(expr.span, message),
                    AtRule::Mixin(..) | AtRule::Function(..) => todo!(),
                    AtRule::Charset => todo!(),
                    r @ AtRule::Unknown(..) | r @ AtRule::Media(..) | r @ AtRule::Keyframes(..) => {
                        stmts.push(Spanned {
                            node: Stmt::AtRule(r),
                            span,
                        })
                    }
                },
                Expr::Styles(s) => stmts.extend(
                    s.into_iter()
                        .map(Box::new)
                        .map(Stmt::Style)
                        .map(|style| Spanned { node: style, span }),
                ),
                Expr::MixinDecl(name, mixin) => {
                    scope.insert_mixin(&name, *mixin);
                }
                Expr::FunctionDecl(name, func) => {
                    scope.insert_fn(&name, *func);
                }
                Expr::Selector(s) => {
                    self.nesting += 1;
                    let rules = self.eat_rules(&super_selector.zip(&s), scope)?;
                    stmts.push(Spanned {
                        node: Stmt::RuleSet(RuleSet {
                            super_selector: super_selector.clone(),
                            selector: s,
                            rules,
                        }),
                        span,
                    });
                    self.nesting -= 1;
                    if self.nesting == 0 {
                        return Ok(stmts);
                    }
                }
                Expr::VariableDecl(name, val) => {
                    if self.nesting == 0 {
                        scope.insert_var(&name, *val.clone())?;
                        insert_global_var(&name, *val)?;
                    } else {
                        scope.insert_var(&name, *val)?;
                    }
                }
                Expr::MultilineComment(s) => stmts.push(Spanned {
                    node: Stmt::MultilineComment(s),
                    span,
                }),
            }
        }
        Ok(stmts)
    }
}

/// Functions that print to stdout or stderr
impl<'a> StyleSheetParser<'a> {
    fn debug(&self, span: Span, message: &str) {
        let loc = self.map.look_up_span(span);
        eprintln!(
            "{}:{} Debug: {}",
            loc.file.name(),
            loc.begin.line + 1,
            message
        );
    }

    fn warn(&self, span: Span, message: &str) {
        let loc = self.map.look_up_span(span);
        eprintln!(
            "Warning: {}\n    {} {}:{}  root stylesheet",
            message,
            loc.file.name(),
            loc.begin.line + 1,
            loc.begin.column + 1
        );
    }
}
