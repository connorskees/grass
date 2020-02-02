use crate::common::{Scope, Symbol};
use crate::utils::{devour_whitespace_or_comment, parse_interpolation};
use crate::value::Value;
use crate::{Token, TokenKind};
use std::fmt::{self, Display};
use std::iter::Peekable;
use std::vec::IntoIter;

/// A style: `color: red`
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Style {
    property: String,
    value: Value,
}

impl Display for Style {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {};", self.property, self.value)
    }
}

impl Style {
    pub fn from_tokens(tokens: Vec<Token>, scope: &Scope) -> Result<Self, ()> {
        Ok(StyleParser::new(tokens, scope)?.parse())
    }
}

struct StyleParser<'a> {
    tokens: Peekable<IntoIter<Token>>,
    scope: &'a Scope,
}

impl<'a> StyleParser<'a> {
    fn new(tokens: Vec<Token>, scope: &'a Scope) -> Result<Self, ()> {
        if tokens.is_empty() {
            return Err(());
        }
        let tokens = tokens.into_iter().peekable();
        Ok(StyleParser { tokens, scope })
    }

    fn parse(&mut self) -> Style {
        let mut property = String::new();
        // read property until `:`
        while let Some(Token { kind, .. }) = self.tokens.next() {
            match kind {
                TokenKind::Whitespace(_) | TokenKind::MultilineComment(_) => continue,
                TokenKind::Ident(ref s) => property.push_str(s),
                TokenKind::Interpolation => property.push_str(
                    &parse_interpolation(&mut self.tokens, self.scope)
                        .iter()
                        .map(|x| x.kind.to_string())
                        .collect::<String>(),
                ),
                TokenKind::Symbol(Symbol::Colon) => break,
                _ => property.push_str(&kind.to_string()),
            };
        }

        devour_whitespace_or_comment(&mut self.tokens);

        let value = Value::from_tokens(&mut self.tokens, self.scope).unwrap();

        Style { property, value }
    }
}
