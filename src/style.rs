use crate::common::{Pos, QuoteKind, Scope, Symbol};
use crate::error::SassResult;
use crate::selector::Selector;
use crate::utils::{devour_whitespace, parse_interpolation, parse_quoted_string};
use crate::value::Value;
use crate::{Expr, Token, TokenKind};
use std::fmt::{self, Display};
use std::iter::Peekable;

/// A style: `color: red`
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Style {
    pub property: String,
    pub value: Value,
}

impl Display for Style {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {};", self.property, self.value)
    }
}

impl Style {
    pub fn parse_property<I: Iterator<Item = Token>>(
        toks: &mut Peekable<I>,
        scope: &Scope,
        super_selector: &Selector,
        super_property: String,
    ) -> SassResult<String> {
        StyleParser::new(scope, super_selector).parse_property(toks, super_property)
    }

    pub fn parse_value<I: Iterator<Item = Token>>(
        toks: &mut Peekable<I>,
        scope: &Scope,
        super_selector: &Selector,
    ) -> SassResult<Value> {
        StyleParser::new(scope, super_selector).parse_style_value(toks, scope)
    }

    pub fn from_tokens<I: Iterator<Item = Token>>(
        toks: &mut Peekable<I>,
        scope: &Scope,
        super_selector: &Selector,
        super_property: String,
    ) -> SassResult<Expr> {
        StyleParser::new(scope, super_selector).eat_style_group(toks, super_property, scope)
    }
}

struct StyleParser<'a> {
    scope: &'a Scope,
    super_selector: &'a Selector,
}

impl<'a> StyleParser<'a> {
    const fn new(scope: &'a Scope, super_selector: &'a Selector) -> Self {
        StyleParser {
            scope,
            super_selector,
        }
    }

    pub(crate) fn parse_style_value<I: Iterator<Item = Token>>(
        &self,
        toks: &mut Peekable<I>,
        scope: &Scope,
    ) -> SassResult<Value> {
        let mut style = Vec::new();
        let mut n = 0;
        devour_whitespace(toks);
        while let Some(tok) = toks.peek() {
            match tok.kind {
                TokenKind::MultilineComment(_) => {
                    toks.next();
                    continue;
                }
                TokenKind::Interpolation => n += 1,
                TokenKind::Symbol(Symbol::CloseCurlyBrace) => {
                    if n == 0 {
                        break;
                    } else {
                        // todo: toks.next() and push
                        n -= 1;
                    }
                }
                ref q @ TokenKind::Symbol(Symbol::DoubleQuote)
                | ref q @ TokenKind::Symbol(Symbol::SingleQuote) => {
                    let q = q.clone();
                    toks.next();
                    let (s, q) = if let Value::Ident(s, q) = parse_quoted_string(toks, scope, &q)? {
                        (s, q)
                    } else {
                        unreachable!()
                    };
                    let quote_kind = Token::from_symbol(match q {
                        QuoteKind::Single => Symbol::SingleQuote,
                        QuoteKind::Double => Symbol::DoubleQuote,
                        _ => unreachable!(),
                    });
                    style.push(quote_kind.clone());
                    style.push(Token::from_string(s));
                    style.push(quote_kind);
                    continue;
                }
                TokenKind::Symbol(Symbol::OpenCurlyBrace)
                | TokenKind::Symbol(Symbol::SemiColon) => break,
                TokenKind::Symbol(Symbol::BitAnd) => {
                    style.push(Token {
                        kind: TokenKind::Ident(self.super_selector.to_string()),
                        pos: Pos::new(),
                    });
                    toks.next();
                    continue;
                }
                _ => {}
            };
            style.push(toks.next().unwrap());
        }
        Value::from_tokens(&mut style.into_iter().peekable(), self.scope)
    }

    pub(crate) fn eat_style_group<I: Iterator<Item = Token>>(
        &self,
        toks: &mut Peekable<I>,
        super_property: String,
        scope: &Scope,
    ) -> SassResult<Expr> {
        let mut styles = Vec::new();
        devour_whitespace(toks);
        while let Some(tok) = toks.peek() {
            match tok.kind {
                TokenKind::Symbol(Symbol::OpenCurlyBrace) => {
                    toks.next();
                    devour_whitespace(toks);
                    loop {
                        let property = self.parse_property(toks, super_property.clone())?;
                        if let Some(tok) = toks.peek() {
                            if tok.is_symbol(Symbol::OpenCurlyBrace) {
                                match self.eat_style_group(toks, property, scope)? {
                                    Expr::Styles(s) => styles.extend(s),
                                    Expr::Style(s) => styles.push(*s),
                                    _ => unreachable!(),
                                }
                                devour_whitespace(toks);
                                if let Some(tok) = toks.peek() {
                                    if tok.is_symbol(Symbol::CloseCurlyBrace) {
                                        toks.next();
                                        devour_whitespace(toks);
                                        return Ok(Expr::Styles(styles));
                                    } else {
                                        continue;
                                    }
                                }
                                continue;
                            }
                        }
                        let value = self.parse_style_value(toks, scope)?;
                        match toks.peek().unwrap().kind {
                            TokenKind::Symbol(Symbol::CloseCurlyBrace) => {
                                styles.push(Style { property, value });
                            }
                            TokenKind::Symbol(Symbol::SemiColon) => {
                                toks.next();
                                devour_whitespace(toks);
                                styles.push(Style { property, value });
                            }
                            TokenKind::Symbol(Symbol::OpenCurlyBrace) => {
                                styles.push(Style {
                                    property: property.clone(),
                                    value,
                                });
                                match self.eat_style_group(toks, property, scope)? {
                                    Expr::Style(s) => styles.push(*s),
                                    Expr::Styles(s) => styles.extend(s),
                                    _ => unreachable!(),
                                }
                            }
                            _ => {
                                devour_whitespace(toks);
                                styles.push(Style { property, value });
                            }
                        }
                        if let Some(tok) = toks.peek() {
                            match tok.kind {
                                TokenKind::Symbol(Symbol::CloseCurlyBrace) => {
                                    toks.next();
                                    devour_whitespace(toks);
                                    return Ok(Expr::Styles(styles));
                                }
                                _ => continue,
                            }
                        }
                    }
                }
                _ => {
                    let val = self.parse_style_value(toks, scope)?;
                    match toks.peek().unwrap().kind {
                        TokenKind::Symbol(Symbol::CloseCurlyBrace) => {}
                        TokenKind::Symbol(Symbol::SemiColon) => {
                            toks.next();
                            devour_whitespace(toks);
                        }
                        TokenKind::Symbol(Symbol::OpenCurlyBrace) => {
                            let mut v = vec![Style {
                                property: super_property.clone(),
                                value: val,
                            }];
                            match self.eat_style_group(toks, super_property, scope)? {
                                Expr::Style(s) => v.push(*s),
                                Expr::Styles(s) => v.extend(s),
                                _ => unreachable!(),
                            }
                            return Ok(Expr::Styles(v));
                        }
                        _ => {}
                    }
                    return Ok(Expr::Style(Box::new(Style {
                        property: super_property,
                        value: val,
                    })));
                }
            }
        }
        Ok(Expr::Styles(styles))
    }

    pub(crate) fn parse_property<I: Iterator<Item = Token>>(
        &self,
        toks: &mut Peekable<I>,
        mut super_property: String,
    ) -> SassResult<String> {
        let mut property = String::new();
        while let Some(Token { kind, .. }) = toks.next() {
            match kind {
                TokenKind::Whitespace(_) | TokenKind::MultilineComment(_) => continue,
                TokenKind::Ident(ref s) => property.push_str(s),
                TokenKind::Interpolation => property.push_str(
                    &parse_interpolation(toks, self.scope)?
                        .iter()
                        .map(|x| x.kind.to_string())
                        .collect::<String>(),
                ),
                TokenKind::Symbol(Symbol::Colon) => break,
                TokenKind::Symbol(Symbol::BitAnd) => {
                    property.push_str(&self.super_selector.to_string())
                }
                _ => property.push_str(&kind.to_string()),
            };
        }
        devour_whitespace(toks);
        if super_property.is_empty() {
            Ok(property)
        } else {
            super_property.reserve(1 + property.len());
            super_property.push('-');
            super_property.push_str(&property);
            Ok(super_property)
        }
    }
}
