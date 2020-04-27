/*! # grass
An implementation of the sass specification in pure rust.

All functionality is currently exposed through [`StyleSheet`].

Spec progress as of 2020-04-21:

| Passing | Failing | Total |
|---------|---------|-------|
| 2150    | 2943    | 5093  |

## Use as library
```
use grass::{SassResult, StyleSheet};

fn main() -> SassResult<()> {
    let sass = StyleSheet::new("a { b { color: &; } }".to_string())?;
    assert_eq!(sass, "a b {\n  color: a b;\n}\n");
    Ok(())
}
```

## Use as binary
```bash
cargo install grass
grass input.scss
```
*/

#![warn(
    clippy::all,
    clippy::restriction,
    clippy::pedantic,
    clippy::nursery,
    clippy::cargo
)]
#![deny(missing_debug_implementations)]
#![allow(
    // explicit return makes some things look ugly
    clippy::implicit_return,
    // Self { .. } is less explicit than Foo { .. }
    clippy::use_self,
    // this is way too pedantic -- some things don't need docs!
    clippy::missing_docs_in_private_items,
    // unreachable!() has many valid use cases
    clippy::unreachable,
    // _ => {} has many valid use cases
    clippy::wildcard_enum_match_arm,
    // .expect() has many valid use cases, like when we know a value is `Some(..)`
    clippy::option_expect_used,
    // this is too pedantic -- we are allowed to add numbers!
    clippy::integer_arithmetic,
    // this is too pedantic for now -- the library is changing too quickly for
    // good docs to be written
    clippy::missing_errors_doc,
    // this incorrectly results in errors for types that derive `Debug`
    // https://github.com/rust-lang/rust-clippy/issues/4980
    clippy::let_underscore_must_use,
    // this is too pedantic -- it results in some names being less explicit
    // than they should
    clippy::module_name_repetitions,
    // this is too pedantic -- it is sometimes useful to break up `impl`s
    clippy::multiple_inherent_impl,

    // temporarily allowed while under heavy development.
    // eventually these allows should be refactored away
    // to no longer be necessary
    clippy::as_conversions,
    clippy::todo,
    clippy::too_many_lines,
    clippy::panic,
    clippy::option_unwrap_used,
    clippy::result_unwrap_used,
    clippy::result_expect_used,
    clippy::cast_possible_truncation,
    clippy::single_match_else,
    clippy::indexing_slicing,
    clippy::match_same_arms,
    clippy::or_fun_call,
    clippy::redundant_pub_crate,
)]
#![cfg_attr(feature = "nightly", feature(track_caller))]
use std::fs;
use std::iter::Iterator;
use std::path::Path;

use codemap::{CodeMap, Span, Spanned};

use peekmore::{PeekMore, PeekMoreIterator};

#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

use crate::atrule::{eat_include, AtRule, AtRuleKind, Function, Mixin};
pub use crate::error::{SassError, SassResult};
use crate::imports::import;
use crate::lexer::Lexer;
use crate::output::Css;
use crate::scope::{
    global_var_exists, insert_global_fn, insert_global_mixin, insert_global_var, Scope,
    GLOBAL_SCOPE,
};
use crate::selector::Selector;
use crate::style::Style;
pub(crate) use crate::token::Token;
use crate::utils::{
    devour_whitespace, eat_comment, eat_ident, eat_ident_no_interpolation, eat_variable_value,
    parse_quoted_string, read_until_closing_curly_brace, read_until_closing_paren,
    read_until_newline, VariableDecl,
};
use crate::value::Value;

mod args;
mod atrule;
mod builtin;
mod color;
mod common;
mod error;
mod imports;
mod lexer;
mod output;
mod scope;
mod selector;
mod style;
mod token;
mod unit;
mod utils;
mod value;

/// Represents a parsed SASS stylesheet with nesting
#[cfg_attr(feature = "wasm", wasm_bindgen)]
#[derive(Debug, Clone)]
pub struct StyleSheet(Vec<Spanned<Stmt>>);

#[derive(Clone, Debug)]
pub(crate) enum Stmt {
    /// A [`Style`](/grass/style/struct.Style)
    Style(Box<Style>),
    /// A  [`RuleSet`](/grass/struct.RuleSet.html)
    RuleSet(RuleSet),
    /// A multiline comment: `/* foo bar */`
    MultilineComment(String),
    /// A CSS rule: `@charset "UTF-8";`
    AtRule(AtRule),
}

impl Stmt {
    const fn span(self, span: Span) -> Spanned<Self> {
        Spanned { node: self, span }
    }
}

/// Represents a single rule set. Rule sets can contain other rule sets
///
/// ```scss
/// a {
///   color: blue;
///   b {
///     color: red;
///   }
/// }
/// ```
#[derive(Clone, Debug)]
pub(crate) struct RuleSet {
    selector: Selector,
    rules: Vec<Spanned<Stmt>>,
    // potential optimization: we don't *need* to own the selector
    super_selector: Selector,
}

/// An intermediate representation of what are essentially single lines
/// todo! rename this
#[derive(Clone, Debug)]
enum Expr {
    /// A style: `color: red`
    Style(Box<Style>),
    /// Several styles
    Styles(Vec<Style>),
    /// A full selector `a > h1`
    Selector(Selector),
    /// A variable declaration `$var: 1px`
    VariableDecl(String, Box<Spanned<Value>>),
    /// A mixin declaration `@mixin foo {}`
    MixinDecl(String, Box<Mixin>),
    FunctionDecl(String, Box<Function>),
    /// A multiline comment: `/* foobar */`
    MultilineComment(String),
    AtRule(AtRule),
}

fn raw_to_parse_error(map: &CodeMap, err: SassError) -> SassError {
    let (message, span) = err.raw();
    SassError::from_loc(message, map.look_up_span(span))
}

#[cfg(feature = "wasm")]
#[wasm_bindgen]
impl StyleSheet {
    pub fn new(input: String) -> Result<String, JsValue> {
        let mut map = CodeMap::new();
        let file = map.add_file("stdin".into(), input);
        Ok(Css::from_stylesheet(StyleSheet(
            StyleSheetParser {
                lexer: Lexer::new(&file).peekmore(),
                nesting: 0,
                map: &map,
                path: Path::new(""),
            }
            .parse_toplevel()
            .map_err(|e| raw_to_parse_error(&map, e).to_string())?
            .0,
        ))
        .map_err(|e| raw_to_parse_error(&map, e).to_string())?
        .pretty_print()
        .map_err(|e| raw_to_parse_error(&map, e).to_string())?)
    }
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
    #[inline]
    #[cfg(not(feature = "wasm"))]
    pub fn new(input: String) -> SassResult<String> {
        let mut map = CodeMap::new();
        let file = map.add_file("stdin".into(), input);
        Css::from_stylesheet(StyleSheet(
            StyleSheetParser {
                lexer: Lexer::new(&file).peekmore(),
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
    #[inline]
    #[cfg(not(feature = "wasm"))]
    pub fn from_path(p: &str) -> SassResult<String> {
        let mut map = CodeMap::new();
        let file = map.add_file(p.clone().into(), String::from_utf8(fs::read(p.clone())?)?);
        Css::from_stylesheet(StyleSheet(
            StyleSheetParser {
                lexer: Lexer::new(&file).peekmore(),
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
            lexer: Lexer::new(&file).peekmore(),
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
    lexer: PeekMoreIterator<Lexer<'a>>,
    nesting: u32,
    map: &'a mut CodeMap,
    path: &'a Path,
}

impl<'a> StyleSheetParser<'a> {
    fn parse_toplevel(mut self) -> SassResult<(Vec<Spanned<Stmt>>, Scope)> {
        let mut rules: Vec<Spanned<Stmt>> = Vec::new();
        while let Some(Token { kind, .. }) = self.lexer.peek() {
            match kind {
                'a'..='z' | 'A'..='Z' | '_' | '-' | '0'..='9'
                | '[' | '#' | ':' | '*' | '%' | '.' | '>' => rules
                    .extend(self.eat_rules(&Selector::new(), &mut Scope::new())?),
                &'\t' | &'\n' | ' ' => {
                    self.lexer.next();
                    continue;
                }
                '$' => {
                    self.lexer.next();
                    let name = eat_ident(&mut self.lexer, &Scope::new(), &Selector::new())?;
                    devour_whitespace(&mut self.lexer);
                    let Token { kind, pos } = self
                        .lexer
                        .next()
                        .unwrap();
                    if kind != ':' {
                        return Err(("expected \":\".", pos).into());
                    }
                    let VariableDecl { val, default, .. } =
                        eat_variable_value(&mut self.lexer, &Scope::new(), &Selector::new())?;
                    if !(default && global_var_exists(&name)) {
                        insert_global_var(&name.node, val)?;
                    }
                }
                '/' => {
                    self.lexer.next();
                    if '*' == self.lexer.peek().unwrap().kind {
                        self.lexer.next();
                        let comment = eat_comment(&mut self.lexer, &Scope::new(), &Selector::new())?;
                        rules.push(comment.map_node(Stmt::MultilineComment));
                    } else if '/' == self.lexer.peek().unwrap().kind {
                        read_until_newline(&mut self.lexer);
                        devour_whitespace(&mut self.lexer);
                    } else {
                        todo!()
                    }
                }
                '@' => {
                    self.lexer.next();
                    let Spanned { node: at_rule_kind, span } = eat_ident(&mut self.lexer, &Scope::new(), &Selector::new())?;
                    if at_rule_kind.is_empty() {
                        return Err(("Expected identifier.", span).into());
                    }
                    match AtRuleKind::from(at_rule_kind.as_str()) {
                        AtRuleKind::Include => rules.extend(eat_include(
                            &mut self.lexer,
                            &Scope::new(),
                            &Selector::new(),
                            None,
                        )?),
                        AtRuleKind::Import => {
                            devour_whitespace(&mut self.lexer);
                            let mut file_name = String::new();
                            let next = match self.lexer.next() {
                                Some(v) => v,
                                None => todo!("expected input after @import")
                            };
                            match next.kind {
                                q @ '"' | q @ '\'' => {
                                    file_name.push_str(
                                        &parse_quoted_string(
                                            &mut self.lexer,
                                            &Scope::new(),
                                            q,
                                            &Selector::new())?
                                            .node.unquote().to_css_string(span)?);
                                }
                                _ => return Err(("Expected string.", next.pos()).into()),
                            }
                            if let Some(t) = self.lexer.peek() {
                                if t.kind == ';' {
                                    self.lexer.next();
                                }
                            }

                            devour_whitespace(&mut self.lexer);

                            let (new_rules, new_scope) = import(self.path, file_name.as_ref(), &mut self.map)?;
                            rules.extend(new_rules);
                            GLOBAL_SCOPE.with(|s| {
                                s.borrow_mut().extend(new_scope);
                            });
                        }
                        v => {
                            let rule = AtRule::from_tokens(&v, span, &mut self.lexer, &mut Scope::new(), &Selector::new(), None)?;
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
                                AtRule::For(f) => rules.extend(f.ruleset_eval(&mut Scope::new(), &Selector::new(), None)?),
                                AtRule::While(w) => rules.extend(w.ruleset_eval(&mut Scope::new(), &Selector::new(), true, None)?),
                                AtRule::Each(e) => {
                                    rules.extend(e.ruleset_eval(&mut Scope::new(), &Selector::new(), None)?)
                                }
                                AtRule::Include(s) => rules.extend(s),
                                AtRule::Content => return Err(
                                    ("@content is only allowed within mixin declarations.", rule.span
                                ).into()),
                                AtRule::If(i) => {
                                    rules.extend(i.eval(&mut Scope::new(), &Selector::new(), None)?);
                                }
                                AtRule::AtRoot(root_rules) => rules.extend(root_rules),
                                AtRule::Unknown(..) => rules.push(rule.map_node(Stmt::AtRule)),
                            }
                        }
                    }
                },
                '&' => {
                    return Err(
                        ("Base-level rules cannot contain the parent-selector-referencing character '&'.", self.lexer.next().unwrap().pos()).into(),
                    )
                }
                c if c.is_control() => {
                    return Err(("expected selector.", self.lexer.next().unwrap().pos()).into());
                }
                _ => todo!("unexpected toplevel token: {:?}", kind),
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
        while let Some(expr) = eat_expr(&mut self.lexer, scope, super_selector, None)? {
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
                    r @ AtRule::Unknown(..) => stmts.push(Spanned {
                        node: Stmt::AtRule(r),
                        span,
                    }),
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

pub(crate) fn eat_expr<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    scope: &mut Scope,
    super_selector: &Selector,
    content: Option<&[Spanned<Stmt>]>,
) -> SassResult<Option<Spanned<Expr>>> {
    let mut values = Vec::with_capacity(5);
    let mut span = if let Some(tok) = toks.peek() {
        tok.pos()
    } else {
        return Ok(None);
    };
    while let Some(tok) = toks.peek() {
        span = span.merge(tok.pos());
        match tok.kind {
            ':' => {
                let tok = toks.next();
                if devour_whitespace(toks) {
                    let prop = Style::parse_property(
                        &mut values.into_iter().peekmore(),
                        scope,
                        super_selector,
                        String::new(),
                    )?;
                    return Ok(Some(Spanned {
                        node: Style::from_tokens(toks, scope, super_selector, prop)?,
                        span,
                    }));
                } else {
                    values.push(tok.unwrap());
                }
            }
            ';' => {
                toks.next();
                devour_whitespace(toks);
                // special edge case where there was no space between the colon
                // in a style, e.g. `color:red`. todo: refactor
                let mut v = values.into_iter().peekmore();
                devour_whitespace(&mut v);
                if v.peek().is_none() {
                    devour_whitespace(toks);
                    return Ok(Some(Spanned {
                        node: Expr::Style(Box::new(Style {
                            property: String::new(),
                            value: Value::Null.span(span),
                        })),
                        span,
                    }));
                }
                let property = Style::parse_property(&mut v, scope, super_selector, String::new())?;
                let value = Style::parse_value(&mut v, scope, super_selector)?;
                return Ok(Some(Spanned {
                    node: Expr::Style(Box::new(Style { property, value })),
                    span,
                }));
            }
            '}' => {
                if values.is_empty() {
                    toks.next();
                    devour_whitespace(toks);
                    if toks.peek().is_some() && toks.peek().unwrap().kind == ';' {
                        toks.next();
                    }
                    devour_whitespace(toks);
                    return Ok(None);
                } else {
                    // special edge case where there was no space between the colon
                    // and no semicolon following the style
                    // in a style `color:red`. todo: refactor
                    let mut v = values.into_iter().peekmore();
                    let property =
                        Style::parse_property(&mut v, scope, super_selector, String::new())?;
                    let value = Style::parse_value(&mut v, scope, super_selector)?;
                    return Ok(Some(Spanned {
                        node: Expr::Style(Box::new(Style { property, value })),
                        span,
                    }));
                }
            }
            '{' => {
                toks.next();
                devour_whitespace(toks);
                return Ok(Some(Spanned {
                    node: Expr::Selector(Selector::from_tokens(
                        &mut values.into_iter().peekmore(),
                        scope,
                        super_selector,
                    )?),
                    span,
                }));
            }
            '$' => {
                let tok = toks.next().unwrap();
                if toks.peek().unwrap().kind == '=' {
                    values.push(tok);
                    values.push(toks.next().unwrap());
                    continue;
                }
                let name = eat_ident_no_interpolation(toks, false)?;
                devour_whitespace(toks);
                if toks.peek().unwrap().kind == ':' {
                    toks.next();
                    devour_whitespace(toks);
                    let VariableDecl {
                        val,
                        default,
                        global,
                    } = eat_variable_value(toks, scope, super_selector)?;
                    if global {
                        insert_global_var(&name.node, val.clone())?;
                    }
                    let var_exists = scope.var_exists(&name.node);
                    if !(default && var_exists) {
                        return Ok(Some(Spanned {
                            node: Expr::VariableDecl(name.node, Box::new(val)),
                            span,
                        }));
                    }
                    if !values.is_empty() {
                        todo!()
                    }
                } else {
                    values.push(tok);
                    let mut current_pos = 0;
                    values.extend(name.chars().map(|x| {
                        let len = x.len_utf8() as u64;
                        let tok = Token::new(span.subspan(current_pos, current_pos + len), x);
                        current_pos += len;
                        tok
                    }));
                }
            }
            '/' => {
                let tok = toks.next().unwrap();
                let peeked = toks.peek().ok_or(("expected more input.", tok.pos()))?;
                if peeked.kind == '/' {
                    read_until_newline(toks);
                    devour_whitespace(toks);
                    continue;
                } else if values.is_empty() && peeked.kind == '*' {
                    toks.next();
                    let comment = eat_comment(toks, scope, super_selector)?;
                    devour_whitespace(toks);
                    return Ok(Some(Spanned {
                        node: Expr::MultilineComment(comment.node),
                        span: comment.span,
                    }));
                } else {
                    values.push(tok);
                }
            }
            '@' => {
                toks.next();
                let Spanned { node: ident, span } = eat_ident(toks, scope, super_selector)?;
                devour_whitespace(toks);
                let rule = AtRule::from_tokens(
                    &AtRuleKind::from(ident.as_str()),
                    span,
                    toks,
                    scope,
                    super_selector,
                    content,
                )?;
                return Ok(Some(Spanned {
                    node: match rule.node {
                        AtRule::Mixin(name, mixin) => Expr::MixinDecl(name, mixin),
                        AtRule::Function(name, func) => Expr::FunctionDecl(name, func),
                        AtRule::Charset => todo!("@charset as expr"),
                        d @ AtRule::Debug(..) => Expr::AtRule(d),
                        w @ AtRule::Warn(..) => Expr::AtRule(w),
                        a @ AtRule::Return(_) => Expr::AtRule(a),
                        c @ AtRule::Content => Expr::AtRule(c),
                        f @ AtRule::If(..) => Expr::AtRule(f),
                        f @ AtRule::For(..) => Expr::AtRule(f),
                        f @ AtRule::While(..) => Expr::AtRule(f),
                        f @ AtRule::Each(..) => Expr::AtRule(f),
                        u @ AtRule::Unknown(..) => Expr::AtRule(u),
                        u @ AtRule::AtRoot(..) => Expr::AtRule(u),
                        u @ AtRule::Include(..) => Expr::AtRule(u),
                    },
                    span,
                }));
            }
            '#' => {
                values.push(toks.next().unwrap());
                if toks.peek().unwrap().kind == '{' {
                    values.push(toks.next().unwrap());
                    values.extend(read_until_closing_curly_brace(toks));
                    values.push(toks.next().unwrap());
                }
            }
            '\\' => {
                values.push(toks.next().unwrap());
                values.push(toks.next().unwrap());
            }
            // todo: this should only apply to special functions
            // it is causing us to emit nothing on malformed input
            '(' => {
                values.push(toks.next().unwrap());
                values.extend(read_until_closing_paren(toks));
            }
            _ => values.push(toks.next().unwrap()),
        };
    }
    Ok(None)
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
