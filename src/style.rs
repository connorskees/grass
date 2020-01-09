use crate::common::Symbol;
use crate::{Token, TokenKind};
use std::collections::HashMap;
use std::fmt::{self, Display};
use std::iter::Peekable;
use std::slice::Iter;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Style {
    property: String,
    value: String,
}

impl Display for Style {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{};", self.property, self.value)
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

    fn parse(&mut self) -> Style {
        let mut property = String::new();
        // read property
        while let Some(Token { kind, .. }) = self.tokens.next() {
            match kind {
                TokenKind::Whitespace(_) | TokenKind::MultilineComment(_) => continue,
                TokenKind::Ident(ref s) => {
                    property = s.clone();
                    break;
                }
                _ => todo!(),
            };
        }

        // read until `:`
        while let Some(Token { kind, .. }) = self.tokens.next() {
            match kind {
                TokenKind::Whitespace(_) | TokenKind::MultilineComment(_) => continue,
                TokenKind::Symbol(Symbol::Colon) => break,
                _ => todo!("found tokens before style value"),
            }
        }

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
                            _ => {}
                        }
                        value.push(' ');
                        break;
                    }
                }
                TokenKind::Variable(_) => value.push_str(&self.deref_variable(&tok.kind)),
                TokenKind::MultilineComment(_) => continue,
                _ => value.push_str(&tok.kind.to_string()),
            }
        }
        Style { property, value }
    }
}
