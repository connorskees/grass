/*! # grass
An implementation of the sass specification in pure rust.

All functionality is currently exposed through [`StyleSheet`].

Spec progress as of 2020-05-01:

| Passing | Failing | Total |
|---------|---------|-------|
| 2193    | 2900    | 5093  |

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
    // this is too pedantic -- some things don't need docs!
    clippy::missing_docs_in_private_items,
    clippy::unreachable,
    // this disallows binding as well
    clippy::wildcard_enum_match_arm,
    // this is too pedantic -- we are allowed to add numbers!
    clippy::integer_arithmetic,
    // this is too pedantic for now -- the library is changing too quickly for
    // good docs to be written
    clippy::missing_errors_doc,
    // this incorrectly results in errors for types that derive `Debug`
    // https://github.com/rust-lang/rust-clippy/issues/4980
    // clippy::let_underscore_must_use,
    // this is too pedantic -- it results in some names being less explicit
    // than they should
    clippy::module_name_repetitions,
    // this is too pedantic -- it is sometimes useful to break up `impl`s
    clippy::multiple_inherent_impl,
    // filter isn't fallible
    clippy::filter_map,
    clippy::else_if_without_else,

    // temporarily allowed while under heavy development.
    // eventually these allows should be refactored away
    // to no longer be necessary
    clippy::as_conversions,
    clippy::todo,
    clippy::too_many_lines,
    clippy::panic,
    clippy::option_unwrap_used,
    clippy::result_unwrap_used,
    clippy::cast_possible_truncation,
    clippy::single_match_else,
    clippy::indexing_slicing,
    // clippy::match_same_arms,
    // clippy::or_fun_call,
    clippy::redundant_pub_crate,
)]
#![cfg_attr(feature = "nightly", feature(track_caller))]
#![cfg_attr(feature = "profiling", inline(never))]

use std::convert::TryFrom;
use std::iter::Iterator;

use codemap::{Span, Spanned};

use peekmore::{PeekMore, PeekMoreIterator};

use crate::atrule::{AtRule, AtRuleKind, Function, Mixin};
pub use crate::error::{SassError, SassResult};
use crate::scope::{insert_global_var, Scope};
use crate::selector::Selector;
use crate::style::Style;
pub use crate::stylesheet::StyleSheet;
pub(crate) use crate::token::Token;
use crate::utils::{
    devour_whitespace, eat_comment, eat_ident, eat_variable_value, peek_ident_no_interpolation,
    peek_whitespace, read_until_closing_curly_brace, read_until_closing_paren, read_until_newline,
    VariableDecl,
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
mod stylesheet;
mod token;
mod unit;
mod utils;
mod value;

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

pub(crate) fn eat_expr<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    scope: &mut Scope,
    super_selector: &Selector,
    content: Option<&[Spanned<Stmt>]>,
) -> SassResult<Option<Spanned<Expr>>> {
    let mut values = Vec::with_capacity(5);
    let mut span = match toks.peek() {
        Some(tok) => tok.pos(),
        None => return Ok(None),
    };
    while let Some(tok) = toks.peek() {
        span = span.merge(tok.pos());
        match tok.kind {
            ':' => {
                let tok = toks.next().unwrap();
                values.push(tok);
                if devour_whitespace(toks) {
                    let prop = Style::parse_property(
                        &mut values.into_iter().peekmore(),
                        scope,
                        super_selector,
                        String::new(),
                        tok.pos,
                    )?;
                    return Ok(Some(Spanned {
                        node: Style::from_tokens(toks, scope, super_selector, prop)?,
                        span,
                    }));
                }
            }
            ';' => {
                let span_before = toks.next().unwrap().pos;
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
                let property = Style::parse_property(
                    &mut v,
                    scope,
                    super_selector,
                    String::new(),
                    span_before,
                )?;
                let value = Style::parse_value(&mut v, scope, super_selector, span_before)?;
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
                    let property = Style::parse_property(
                        &mut v,
                        scope,
                        super_selector,
                        String::new(),
                        tok.pos,
                    )?;
                    let value = Style::parse_value(&mut v, scope, super_selector, tok.pos)?;
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

                if toks.peek().ok_or(("Expected identifier.", tok.pos))?.kind == '=' {
                    values.push(tok);
                    values.push(toks.next().unwrap());
                    continue;
                }

                let name = peek_ident_no_interpolation(toks, false, tok.pos)?;
                let whitespace = peek_whitespace(toks);

                if toks.peek().ok_or(("expected \":\".", name.span))?.kind == ':' {
                    toks.take(name.node.chars().count() + whitespace + 1)
                        .for_each(drop);
                    devour_whitespace(toks);
                    let VariableDecl {
                        val,
                        default,
                        global,
                    } = eat_variable_value(toks, scope, super_selector, name.span)?;
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
                    toks.reset_view();
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
                let span = toks.next().unwrap().pos();
                let rule = eat_ident(toks, scope, super_selector, span)?;
                devour_whitespace(toks);
                let rule = AtRule::from_tokens(
                    AtRuleKind::try_from(&rule)?,
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
                        a => Expr::AtRule(a),
                    },
                    span,
                }));
            }
            '#' => {
                let next = toks.next().unwrap();
                values.push(next);
                match toks.peek() {
                    Some(Token { kind: '{', .. }) => {
                        let next = toks.next().unwrap();
                        values.push(next);
                        values.extend(read_until_closing_curly_brace(toks)?);
                        if let Some(tok) = toks.next() {
                            values.push(tok);
                        } else {
                            return Err(("expected \"}\".", next.pos).into());
                        }
                    }
                    Some(..) => {}
                    None => return Err(("expected \"{\".", next.pos).into()),
                }
            }
            '\\' => {
                let next = toks.next().unwrap();
                values.push(next);
                values.push(toks.next().ok_or(("expected \"}\".", next.pos))?);
            }
            // todo: this should only apply to special functions
            // it is causing us to emit nothing on malformed input
            '(' => {
                values.push(toks.next().unwrap());
                values.extend(read_until_closing_paren(toks)?);
            }
            _ => values.push(toks.next().unwrap()),
        };
    }

    // if `values` is not empty, there was an unexpected toplevel token
    // that should be part of a selector
    if let Some(v) = values.pop() {
        return Err(("expected \"{\".", v.pos).into());
    }

    Ok(None)
}
