#![allow(dead_code, unused_variables, unused_mut)]

use std::fmt::{self, Display};

use peekmore::{PeekMore, PeekMoreIterator};

use crate::error::SassResult;
use crate::scope::Scope;
use crate::utils::{devour_whitespace, eat_comment, parse_interpolation, read_until_newline};
use crate::value::Value;
use crate::Token;

pub(crate) use attribute::Attribute;
pub(crate) use common::*;
pub(crate) use complex::*;
pub(crate) use compound::*;
pub(crate) use extend::*;
pub(crate) use list::*;
pub(crate) use parse::*;
pub(crate) use simple::*;

mod attribute;
mod common;
mod complex;
mod compound;
mod extend;
mod list;
mod parse;
mod simple;

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Selector(pub SelectorList);

impl Display for Selector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Selector {
    pub fn from_tokens<I: Iterator<Item = Token>>(
        toks: &mut PeekMoreIterator<I>,
        scope: &Scope,
        super_selector: &Selector,
        allows_parent: bool,
    ) -> SassResult<Selector> {
        let mut string = String::new();

        let mut span = if let Some(tok) = toks.peek() {
            tok.pos()
        } else {
            return Ok(Selector::new());
        };

        while let Some(tok) = toks.next() {
            span = span.merge(tok.pos());
            match tok.kind {
                '#' => {
                    if let Some(Token { kind: '{', pos }) = toks.peek().cloned() {
                        toks.next();
                        string.push_str(
                            &parse_interpolation(toks, scope, super_selector, pos)?
                                .to_css_string(span)?,
                        );
                    } else {
                        string.push('#');
                    }
                }
                ',' => {
                    while let Some(c) = string.pop() {
                        if c == ' ' || c == ',' {
                            continue;
                        }
                        string.push(c);
                        string.push(',');
                        break;
                    }
                }
                '/' => {
                    if toks.peek().is_none() {
                        return Err(("Expected selector.", tok.pos()).into());
                    } else if '*' == toks.peek().unwrap().kind {
                        toks.next();
                        eat_comment(toks, &Scope::new(), &Selector::new())?;
                    } else if '/' == toks.peek().unwrap().kind {
                        read_until_newline(toks);
                        devour_whitespace(toks);
                    } else {
                        return Err(("Expected selector.", tok.pos()).into());
                    }
                    string.push(' ');
                }
                c => string.push(c),
            }
        }

        while let Some(c) = string.pop() {
            if c == ' ' || c == ',' || c == '\t' {
                continue;
            }
            string.push(c);
            break;
        }

        let sel_toks: Vec<Token> = string.chars().map(|x| Token::new(span, x)).collect();

        let mut iter = sel_toks.into_iter().peekmore();

        Ok(Selector(
            SelectorParser::new(&mut iter, scope, super_selector, allows_parent, true, span)
                .parse()?,
        ))
    }

    pub fn replace(super_selector: &Selector, this: Selector) -> Selector {
        todo!()
    }

    /// Small wrapper around `SelectorList`'s method that turns an empty parent selector
    /// into `None`. This is a hack and in the future should be replaced.
    // todo: take Option<Self> for parent
    pub fn resolve_parent_selectors(&self, parent: &Self, implicit_parent: bool) -> Self {
        Self(self.0.clone().resolve_parent_selectors(
            if parent.is_empty() {
                None
            } else {
                Some(parent.0.clone())
            },
            implicit_parent,
        ))
    }

    pub fn is_super_selector(&self, other: &Self) -> bool {
        self.0.is_superselector(&other.0)
    }

    pub fn contains_parent_selector(&self) -> bool {
        self.0.contains_parent_selector()
    }

    pub fn remove_placeholders(self) -> Selector {
        Self(SelectorList {
            components: self
                .0
                .components
                .into_iter()
                .filter(|c| !c.is_invisible())
                .collect(),
        })
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn new() -> Selector {
        Selector(SelectorList::new())
    }

    pub fn into_value(self) -> Value {
        self.0.to_sass_list()
    }

    pub fn unify(self, other: Self) -> Option<Self> {
        Some(Selector(self.0.unify(other.0)?))
    }
}
