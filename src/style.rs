use crate::common::Symbol;
use crate::{Token, TokenKind};
use std::collections::HashMap;
use std::fmt::{self, Display};
use std::iter::Peekable;
use std::slice::Iter;

/// A style: `color: red`
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Style {
    property: String,
    value: String,
}

impl Display for Style {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {};", self.property, self.value)
    }
}

impl Style {
    pub fn from_tokens(tokens: &[Token], vars: &HashMap<String, Vec<Token>>) -> Result<Self, ()> {
        Ok(StyleParser::new(tokens, vars)?.parse())
    }
}

struct StyleParser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
    vars: &'a HashMap<String, Vec<Token>>,
}

impl<'a> StyleParser<'a> {
    fn new(tokens: &'a [Token], vars: &'a HashMap<String, Vec<Token>>) -> Result<Self, ()> {
        if tokens.is_empty() {
            return Err(());
        }
        let tokens = tokens.iter().peekable();
        Ok(StyleParser { tokens, vars })
    }

    fn deref_variable(&mut self, variable: &TokenKind) -> String {
        let mut val = String::with_capacity(25);
        let mut v = match variable {
            TokenKind::Variable(ref v) => {
                self.vars.get(v).expect("todo! expected variable to exist")
            }
            _ => panic!("expected variable"),
        }
        .iter()
        .peekable();
        while let Some(tok) = v.next() {
            match &tok.kind {
                TokenKind::Variable(_) => val.push_str(&self.deref_variable(&tok.kind)),
                TokenKind::Whitespace(_) => {
                    while let Some(w) = v.peek() {
                        if let TokenKind::Whitespace(_) = w.kind {
                            v.next();
                        } else {
                            val.push(' ');
                            break;
                        }
                    }
                }
                _ => val.push_str(&tok.kind.to_string()),
            };
        }
        val
    }

    fn devour_whitespace_or_comment(&mut self) {
        while let Some(Token { kind, .. }) = self.tokens.peek() {
            match kind {
                TokenKind::Whitespace(_) | TokenKind::MultilineComment(_) => self.tokens.next(),
                _ => break,
            };
        }
    }

    fn eat_interpolation(&mut self) -> String {
        let mut val = String::new();
        while let Some(Token { kind, .. }) = self.tokens.next() {
            match &kind {
                TokenKind::Symbol(Symbol::CloseBrace) => break,
                TokenKind::Symbol(Symbol::OpenBrace) => todo!("invalid character in interpolation"),
                TokenKind::Variable(_) => val.push_str(&self.deref_variable(kind)),
                _ => val.push_str(&kind.to_string()),
            }
        }
        val
    }

    fn parse(&mut self) -> Style {
        let mut property = String::new();
        // read property until `:`
        while let Some(Token { kind, .. }) = self.tokens.next() {
            match kind {
                TokenKind::Whitespace(_) | TokenKind::MultilineComment(_) => continue,
                TokenKind::Ident(ref s) => property.push_str(s),
                TokenKind::Interpolation => property.push_str(&self.eat_interpolation()),
                TokenKind::Symbol(Symbol::Colon) => break,
                _ => property.push_str(&kind.to_string()),
            };
        }

        self.devour_whitespace_or_comment();

        let mut value = String::new();

        // read styles
        while let Some(tok) = self.tokens.next() {
            match &tok.kind {
                TokenKind::Whitespace(_) => {
                    while let Some(Token { kind, .. }) = self.tokens.peek() {
                        match kind {
                            TokenKind::Whitespace(_) | TokenKind::MultilineComment(_) => {
                                self.tokens.next();
                                continue;
                            }
                            TokenKind::Ident(ref s) => {
                                if s == &String::from("-") {
                                    self.tokens.next();
                                    value.push('-');
                                    self.devour_whitespace_or_comment();
                                    break;
                                }
                            }
                            TokenKind::Interpolation => {
                                self.tokens.next();
                                value.push_str(&self.eat_interpolation());
                                break;
                            }
                            _ => {}
                        }
                        value.push(' ');
                        break;
                    }
                }
                TokenKind::Variable(_) => value.push_str(&self.deref_variable(&tok.kind)),
                TokenKind::MultilineComment(_) => continue,
                TokenKind::Interpolation => value.push_str(&self.eat_interpolation()),
                _ => value.push_str(&tok.kind.to_string()),
            }
        }
        Style { property, value }
    }
}
