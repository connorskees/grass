use std::fmt::{self, Display};
use std::iter::Peekable;
use std::string::ToString;

use super::{Selector, SelectorKind};
use crate::error::SassResult;
use crate::scope::Scope;
use crate::utils::{
    devour_whitespace, eat_ident,
    parse_interpolation, parse_quoted_string,
};
use crate::Token;


#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Attribute {
    pub attr: String,
    pub value: String,
    pub modifier: String,
    pub kind: AttributeKind,
}

impl Attribute {
    pub fn from_tokens<I: Iterator<Item = Token>>(
        toks: &mut Peekable<I>,
        scope: &Scope,
        super_selector: &Selector,
    ) -> SassResult<SelectorKind> {
        devour_whitespace(toks);
        let attr = if let Some(t) = toks.next() {
            match t.kind {
                v @ 'a'..='z' | v @ 'A'..='Z' | v @ '-' | v @ '_' => {
                    format!("{}{}", v, eat_ident(toks, scope, super_selector)?)
                }
                '#' if toks.next().unwrap().kind == '{' => {
                    parse_interpolation(toks, scope, super_selector)?.to_string()
                }
                q @ '"' | q @ '\'' => {
                    parse_quoted_string(toks, scope, q, super_selector)?.to_string()
                }
                _ => return Err("Expected identifier.".into()),
            }
        } else {
            todo!()
        };

        devour_whitespace(toks);

        let kind = if let Some(t) = toks.next() {
            match t.kind {
                v @ 'a'..='z' | v @ 'A'..='Z' => {
                    match toks.next().unwrap().kind {
                        ']' => {}
                        _ => return Err("expected \"]\".".into()),
                    }
                    return Ok(SelectorKind::Attribute(Attribute {
                        kind: AttributeKind::Any,
                        attr,
                        value: String::new(),
                        modifier: v.to_string(),
                    }));
                }
                ']' => {
                    return Ok(SelectorKind::Attribute(Attribute {
                        kind: AttributeKind::Any,
                        attr,
                        value: String::new(),
                        modifier: String::new(),
                    }));
                }
                '=' => AttributeKind::Equals,
                '~' => AttributeKind::InList,
                '|' => AttributeKind::BeginsWithHyphenOrExact,
                '^' => AttributeKind::StartsWith,
                '$' => AttributeKind::EndsWith,
                '*' => AttributeKind::Contains,
                _ => return Err("Expected \"]\".".into()),
            }
        } else {
            todo!()
        };

        if kind != AttributeKind::Equals {
            match toks.next().unwrap().kind {
                '=' => {}
                _ => return Err("expected \"=\".".into()),
            }
        }

        devour_whitespace(toks);

        let value = if let Some(t) = toks.next() {
            match t.kind {
                v @ 'a'..='z' | v @ 'A'..='Z' | v @ '-' | v @ '_' => {
                    format!("{}{}", v, eat_ident(toks, scope, super_selector)?)
                }
                q @ '"' | q @ '\'' => {
                    parse_quoted_string(toks, scope, q, super_selector)?.to_string()
                }
                _ => return Err("Expected identifier.".into()),
            }
        } else {
            todo!()
        };

        devour_whitespace(toks);

        let modifier = if let Some(t) = toks.next() {
            match t.kind {
                ']' => {
                    return Ok(SelectorKind::Attribute(Attribute {
                        kind,
                        attr,
                        value,
                        modifier: String::new(),
                    }))
                }
                v @ 'a'..='z' | v @ 'A'..='Z' => {
                    match toks.next().unwrap().kind {
                        ']' => {}
                        _ => return Err("expected \"]\".".into()),
                    }
                    format!(" {}", v)
                }
                _ => return Err("Expected \"]\".".into()),
            }
        } else {
            todo!()
        };

        Ok(SelectorKind::Attribute(Attribute {
            kind,
            attr,
            value,
            modifier,
        }))
    }
}

impl Display for Attribute {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            AttributeKind::Any => write!(f, "[{}{}]", self.attr, self.modifier),
            AttributeKind::Equals => write!(f, "[{}={}{}]", self.attr, self.value, self.modifier),
            AttributeKind::InList => write!(f, "[{}~={}{}]", self.attr, self.value, self.modifier),
            AttributeKind::BeginsWithHyphenOrExact => {
                write!(f, "[{}|={}{}]", self.attr, self.value, self.modifier)
            }
            AttributeKind::StartsWith => {
                write!(f, "[{}^={}{}]", self.attr, self.value, self.modifier)
            }
            AttributeKind::EndsWith => {
                write!(f, "[{}$={}{}]", self.attr, self.value, self.modifier)
            }
            AttributeKind::Contains => {
                write!(f, "[{}*={}{}]", self.attr, self.value, self.modifier)
            }
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum AttributeKind {
    /// [attr]
    /// Represents elements with an attribute name of `attr`
    Any,
    /// [attr=value]
    /// Represents elements with an attribute name of `attr` whose value is exactly `value`
    Equals,
    /// [attr~=value]
    /// Represents elements with an attribute name of `attr` whose value is a whitespace-separated list of words, one of which is exactly `value`
    InList,
    /// [attr|=value]
    /// Represents elements with an attribute name of `attr` whose value can be exactly value or can begin with `value` immediately followed by a hyphen (`-`)
    BeginsWithHyphenOrExact,
    /// [attr^=value]
    StartsWith,
    /// [attr$=value]
    EndsWith,
    /// [attr*=value]
    /// Represents elements with an attribute name of `attr` whose value contains at least one occurrence of `value` within the string
    Contains,
}
