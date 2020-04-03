use std::fmt::{self, Display};
use std::iter::Peekable;
use std::string::ToString;

use super::{Selector, SelectorKind};
use crate::error::SassResult;
use crate::scope::Scope;
use crate::utils::{
    devour_whitespace, eat_ident, is_ident_char, parse_interpolation, parse_quoted_string,
};
use crate::Token;

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Attribute {
    attr: String,
    value: String,
    modifier: Option<char>,
    kind: AttributeKind,
}

impl Attribute {
    pub fn from_tokens<I: Iterator<Item = Token>>(
        toks: &mut Peekable<I>,
        scope: &Scope,
        super_selector: &Selector,
    ) -> SassResult<SelectorKind> {
        devour_whitespace(toks);
        let attr = match toks.peek().ok_or("Expected identifier.")?.kind {
            c if is_ident_char(c) => eat_ident(toks, scope, super_selector)?,
            '#' => {
                toks.next();
                if toks.next().ok_or("Expected expression.")?.kind == '{' {
                    parse_interpolation(toks, scope, super_selector)?.to_string()
                } else {
                    return Err("Expected expression.".into());
                }
            }
            _ => return Err("Expected identifier.".into()),
        };

        devour_whitespace(toks);

        let kind = match toks.next().ok_or("expected \"{\".")?.kind {
            c if is_ident_char(c) => return Err("Expected \"]\".".into()),
            ']' => {
                return Ok(SelectorKind::Attribute(Attribute {
                    kind: AttributeKind::Any,
                    attr,
                    value: String::new(),
                    modifier: None,
                }));
            }
            '=' => AttributeKind::Equals,
            '~' => AttributeKind::Include,
            '|' => AttributeKind::Dash,
            '^' => AttributeKind::Prefix,
            '$' => AttributeKind::Suffix,
            '*' => AttributeKind::Contains,
            _ => return Err("expected \"]\".".into()),
        };

        if kind != AttributeKind::Equals {
            match toks.next().ok_or("expected \"=\".")?.kind {
                '=' => {}
                _ => return Err("expected \"=\".".into()),
            }
        }

        devour_whitespace(toks);

        let value = match toks.next().ok_or("Expected identifier.")?.kind {
            v @ 'a'..='z' | v @ 'A'..='Z' | v @ '-' | v @ '_' => {
                format!("{}{}", v, eat_ident(toks, scope, super_selector)?)
            }
            q @ '"' | q @ '\'' => parse_quoted_string(toks, scope, q, super_selector)?.to_string(),
            _ => return Err("Expected identifier.".into()),
        };

        devour_whitespace(toks);

        let modifier = match toks.next().ok_or("expected \"]\".")?.kind {
            ']' => {
                return Ok(SelectorKind::Attribute(Attribute {
                    kind,
                    attr,
                    value,
                    modifier: None,
                }))
            }
            v @ 'a'..='z' | v @ 'A'..='Z' => {
                match toks.next().ok_or("expected \"]\".")?.kind {
                    ']' => {}
                    _ => return Err("expected \"]\".".into()),
                }
                Some(v)
            }
            _ => return Err("expected \"]\".".into()),
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
        let modifier = if let Some(c) = self.modifier {
            format!(" {}", c)
        } else {
            String::new()
        };
        match self.kind {
            AttributeKind::Any => write!(f, "[{}{}]", self.attr, modifier),
            AttributeKind::Equals => write!(f, "[{}={}{}]", self.attr, self.value, modifier),
            AttributeKind::Include => write!(f, "[{}~={}{}]", self.attr, self.value, modifier),
            AttributeKind::Dash => write!(f, "[{}|={}{}]", self.attr, self.value, modifier),
            AttributeKind::Prefix => write!(f, "[{}^={}{}]", self.attr, self.value, modifier),
            AttributeKind::Suffix => write!(f, "[{}$={}{}]", self.attr, self.value, modifier),
            AttributeKind::Contains => write!(f, "[{}*={}{}]", self.attr, self.value, modifier),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum AttributeKind {
    /// [attr]
    ///
    /// Represents elements with an attribute name of `attr`
    Any,

    /// [attr=value]
    ///
    /// Represents elements with an attribute name of `attr`
    /// whose value is exactly `value`
    Equals,

    /// [attr~=value]
    ///
    /// Represents elements with an attribute name of `attr`
    /// whose value is a whitespace-separated list of words,
    /// one of which is exactly `value`
    Include,

    /// [attr|=value]
    ///
    /// Represents elements with an attribute name of `attr`
    /// whose value can be exactly value or can begin with
    /// `value` immediately followed by a hyphen (`-`)
    Dash,

    /// [attr^=value]
    Prefix,

    /// [attr$=value]
    Suffix,

    /// [attr*=value]
    ///
    /// Represents elements with an attribute name of `attr`
    /// whose value contains at least one occurrence of
    /// `value` within the string
    Contains,
}
