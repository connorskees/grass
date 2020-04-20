use std::fmt::{self, Display};

use peekmore::PeekMoreIterator;

use codemap::Span;

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
        toks: &mut PeekMoreIterator<I>,
        scope: &Scope,
        super_selector: &Selector,
        mut start: Span,
    ) -> SassResult<SelectorKind> {
        devour_whitespace(toks);
        let next_tok = toks.peek().ok_or(("Expected identifier.", start))?;
        let attr = match next_tok.kind {
            c if is_ident_char(c) => {
                let i = eat_ident(toks, scope, super_selector)?;
                start = i.span;
                i.node
            }
            '#' => {
                start.merge(toks.next().unwrap().pos());
                if toks.next().ok_or(("Expected expression.", start))?.kind == '{' {
                    let interpolation = parse_interpolation(toks, scope, super_selector)?;
                    interpolation.node.to_css_string(interpolation.span)?
                } else {
                    return Err(("Expected expression.", start).into());
                }
            }
            _ => return Err(("Expected identifier.", start).into()),
        };

        devour_whitespace(toks);

        let next = toks.next().ok_or(("expected \"]\".", start))?;

        let kind = match next.kind {
            c if is_ident_char(c) => return Err(("Expected \"]\".", next.pos()).into()),
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
            _ => return Err(("expected \"]\".", next.pos()).into()),
        };

        if kind != AttributeKind::Equals {
            let next = toks.next().ok_or(("expected \"=\".", next.pos()))?;
            match next.kind {
                '=' => {}
                _ => return Err(("expected \"=\".", next.pos()).into()),
            }
        }

        devour_whitespace(toks);

        let next = toks.next().ok_or(("Expected identifier.", next.pos()))?;

        let value = match next.kind {
            v @ 'a'..='z' | v @ 'A'..='Z' | v @ '-' | v @ '_' => {
                format!("{}{}", v, eat_ident(toks, scope, super_selector)?.node)
            }
            q @ '"' | q @ '\'' => {
                parse_quoted_string(toks, scope, q, super_selector)?.to_css_string(next.pos())?
            }
            _ => return Err(("Expected identifier.", next.pos()).into()),
        };

        devour_whitespace(toks);

        let next = toks.next().ok_or(("expected \"]\".", next.pos()))?;

        let modifier = match next.kind {
            ']' => {
                return Ok(SelectorKind::Attribute(Attribute {
                    kind,
                    attr,
                    value,
                    modifier: None,
                }))
            }
            v @ 'a'..='z' | v @ 'A'..='Z' => {
                let next = toks.next().ok_or(("expected \"]\".", next.pos()))?;
                match next.kind {
                    ']' => {}
                    _ => return Err(("expected \"]\".", next.pos()).into()),
                }
                Some(v)
            }
            _ => return Err(("expected \"]\".", next.pos()).into()),
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
    /// \[attr\]
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
