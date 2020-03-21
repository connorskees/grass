use std::convert::TryFrom;
use std::iter::{Iterator, Peekable};

use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::pow;

use crate::args::eat_call_args;
use crate::builtin::GLOBAL_FUNCTIONS;
use crate::color::Color;
use crate::common::{Keyword, ListSeparator, Op, QuoteKind, Symbol};
use crate::error::SassResult;
use crate::scope::Scope;
use crate::selector::Selector;
use crate::unit::Unit;
use crate::utils::{
    devour_whitespace_or_comment, flatten_ident, parse_interpolation, parse_quoted_string,
};
use crate::value::Value;
use crate::{Token, TokenKind};

use super::number::Number;

fn parse_hex(s: &str) -> Value {
    match s.len() {
        3 => {
            let v = match u16::from_str_radix(s, 16) {
                Ok(a) => a,
                Err(_) => return Value::Ident(format!("#{}", s), QuoteKind::None),
            };
            let red = (((v & 0xf00) >> 8) * 0x11) as u8;
            let green = (((v & 0x0f0) >> 4) * 0x11) as u8;
            let blue = ((v & 0x00f) * 0x11) as u8;
            Value::Color(Color::new(red, green, blue, 1, format!("#{}", s)))
        }
        4 => {
            let v = match u16::from_str_radix(s, 16) {
                Ok(a) => a,
                Err(_) => return Value::Ident(format!("#{}", s), QuoteKind::None),
            };
            let red = (((v & 0xf000) >> 12) * 0x11) as u8;
            let green = (((v & 0x0f00) >> 8) * 0x11) as u8;
            let blue = (((v & 0x00f0) >> 4) * 0x11) as u8;
            let alpha = ((v & 0x000f) * 0x11) as u8;
            Value::Color(Color::new(red, green, blue, alpha, format!("#{}", s)))
        }
        6 => {
            let v = match u32::from_str_radix(s, 16) {
                Ok(a) => a,
                Err(_) => return Value::Ident(format!("#{}", s), QuoteKind::None),
            };
            let red = ((v & 0x00ff_0000) >> 16) as u8;
            let green = ((v & 0x0000_ff00) >> 8) as u8;
            let blue = (v & 0x0000_00ff) as u8;
            Value::Color(Color::new(red, green, blue, 1, format!("#{}", s)))
        }
        8 => {
            let v = match u32::from_str_radix(s, 16) {
                Ok(a) => a,
                Err(_) => return Value::Ident(format!("#{}", s), QuoteKind::None),
            };
            let red = ((v & 0xff00_0000) >> 24) as u8;
            let green = ((v & 0x00ff_0000) >> 16) as u8;
            let blue = ((v & 0x0000_ff00) >> 8) as u8;
            let alpha = (v & 0x0000_00ff) as u8;
            Value::Color(Color::new(red, green, blue, alpha, format!("#{}", s)))
        }
        _ => Value::Ident(format!("#{}", s), QuoteKind::None),
    }
}

impl Value {
    pub fn from_tokens<I: Iterator<Item = Token>>(
        toks: &mut Peekable<I>,
        scope: &Scope,
        super_selector: &Selector,
    ) -> SassResult<Self> {
        let left = Self::_from_tokens(toks, scope, super_selector)?;
        devour_whitespace_or_comment(toks);
        let next = match toks.peek() {
            Some(x) => x,
            None => return Ok(left),
        };
        match next.kind {
            TokenKind::Symbol(Symbol::SemiColon) | TokenKind::Symbol(Symbol::CloseParen) => {
                Ok(left)
            }
            TokenKind::Symbol(Symbol::Comma) => {
                toks.next();
                devour_whitespace_or_comment(toks);
                let right = Self::from_tokens(toks, scope, super_selector)?;
                if let Value::List(v, ListSeparator::Comma) = right {
                    let mut v2 = vec![left];
                    v2.extend(v);
                    Ok(Value::List(v2, ListSeparator::Comma))
                } else {
                    Ok(Value::List(vec![left, right], ListSeparator::Comma))
                }
            }
            TokenKind::Symbol(Symbol::Plus)
            | TokenKind::Symbol(Symbol::Minus)
            | TokenKind::Op(_)
            | TokenKind::Symbol(Symbol::Mul)
            | TokenKind::Symbol(Symbol::Div)
            | TokenKind::Symbol(Symbol::Percent) => {
                let op = match next.kind {
                    TokenKind::Symbol(Symbol::Plus) => Op::Plus,
                    TokenKind::Symbol(Symbol::Minus) => Op::Minus,
                    TokenKind::Symbol(Symbol::Mul) => Op::Mul,
                    TokenKind::Symbol(Symbol::Div) => Op::Div,
                    TokenKind::Symbol(Symbol::Percent) => Op::Rem,
                    TokenKind::Op(op) => op,
                    _ => unsafe { std::hint::unreachable_unchecked() },
                };
                toks.next();
                devour_whitespace_or_comment(toks);
                let right = Self::from_tokens(toks, scope, super_selector)?;
                Ok(Value::BinaryOp(Box::new(left), op, Box::new(right)))
            }
            _ => {
                devour_whitespace_or_comment(toks);
                let right = Self::from_tokens(toks, scope, super_selector)?;
                if let Value::List(v, ListSeparator::Space) = right {
                    let mut v2 = vec![left];
                    v2.extend(v);
                    Ok(Value::List(v2, ListSeparator::Space))
                } else {
                    Ok(Value::List(vec![left, right], ListSeparator::Space))
                }
            }
        }
    }

    fn _from_tokens<I: Iterator<Item = Token>>(
        toks: &mut Peekable<I>,
        scope: &Scope,
        super_selector: &Selector,
    ) -> SassResult<Self> {
        let kind = if let Some(tok) = toks.next() {
            tok.kind
        } else {
            panic!("Unexpected EOF");
        };
        match kind {
            TokenKind::Number(val) => {
                let unit = if let Some(tok) = toks.peek() {
                    match tok.kind.clone() {
                        TokenKind::Ident(i) => {
                            toks.next();
                            Unit::from(&i)
                        }
                        TokenKind::Symbol(Symbol::Percent) => {
                            toks.next();
                            Unit::Percent
                        }
                        _ => Unit::None,
                    }
                } else {
                    Unit::None
                };
                let n = if let Ok(v) = val.parse::<BigRational>() {
                    // the number is an integer!
                    v
                // the number is floating point
                } else {
                    let mut num = String::new();
                    let mut chars = val.chars();
                    let mut num_dec = 0;
                    while let Some(c) = chars.next() {
                        if c == '.' {
                            break;
                        }
                        num.push(c);
                    }
                    for c in chars {
                        num_dec += 1;
                        num.push(c);
                    }
                    BigRational::new(num.parse().unwrap(), pow(BigInt::from(10), num_dec))
                };
                Ok(Value::Dimension(Number::new(n), unit))
            }
            TokenKind::Symbol(Symbol::OpenParen) => {
                devour_whitespace_or_comment(toks);
                if toks.peek().unwrap().is_symbol(Symbol::CloseParen) {
                    toks.next();
                    return Ok(Value::List(Vec::new(), ListSeparator::Space));
                }
                let val = Self::from_tokens(toks, scope, super_selector)?;
                let next = toks.next();
                if next.is_none() || !next.unwrap().is_symbol(Symbol::CloseParen) {
                    return Err("expected \")\".".into());
                }
                Ok(Value::Paren(Box::new(val)))
            }
            TokenKind::Symbol(Symbol::BitAnd) => {
                Ok(Value::Ident(super_selector.to_string(), QuoteKind::None))
            }
            TokenKind::Symbol(Symbol::Hash) => {
                Ok(parse_hex(&flatten_ident(toks, scope, super_selector)?))
            }
            TokenKind::Ident(mut s) => {
                s.push_str(&flatten_ident(toks, scope, super_selector)?);
                match toks.peek() {
                    Some(Token {
                        kind: TokenKind::Symbol(Symbol::OpenParen),
                        ..
                    }) => {
                        toks.next();
                        let func = match scope.get_fn(&s) {
                            Ok(f) => f,
                            Err(_) => match GLOBAL_FUNCTIONS.get(&s) {
                                Some(f) => {
                                    return f(
                                        &mut eat_call_args(toks, scope, super_selector)?,
                                        scope,
                                    )
                                }
                                None => {
                                    s.push('(');
                                    let mut unclosed_parens = 0;
                                    while let Some(t) = toks.next() {
                                        match &t.kind {
                                            TokenKind::Symbol(Symbol::OpenParen) => {
                                                unclosed_parens += 1;
                                            }
                                            TokenKind::Interpolation => s.push_str(
                                                &parse_interpolation(toks, scope, super_selector)?
                                                    .to_string(),
                                            ),
                                            TokenKind::Variable(v) => {
                                                s.push_str(&scope.get_var(v)?.to_string())
                                            }
                                            TokenKind::Symbol(Symbol::CloseParen) => {
                                                if unclosed_parens <= 1 {
                                                    s.push(')');
                                                    break;
                                                } else {
                                                    unclosed_parens -= 1;
                                                }
                                            }
                                            _ => {}
                                        }
                                        s.push_str(&t.kind.to_string());
                                    }
                                    return Ok(Value::Ident(s, QuoteKind::None));
                                }
                            },
                        };
                        Ok(func
                            .clone()
                            .args(&mut eat_call_args(toks, scope, super_selector)?)?
                            .call(super_selector)?)
                    }
                    _ => {
                        if let Ok(c) = crate::color::ColorName::try_from(s.as_ref()) {
                            Ok(Value::Color(c.into_color(s)))
                        } else {
                            Ok(Value::Ident(s, QuoteKind::None))
                        }
                    }
                }
            }
            q @ TokenKind::Symbol(Symbol::DoubleQuote)
            | q @ TokenKind::Symbol(Symbol::SingleQuote) => {
                parse_quoted_string(toks, scope, &q, super_selector)
            }
            TokenKind::Variable(ref v) => Ok(scope.get_var(v)?),
            TokenKind::Interpolation => {
                let mut s = parse_interpolation(toks, scope, super_selector)?.to_string();
                while let Some(tok) = toks.peek() {
                    match tok.kind.clone() {
                        TokenKind::Interpolation => {
                            toks.next();
                            s.push_str(
                                &parse_interpolation(toks, scope, super_selector)?.to_string(),
                            )
                        }
                        TokenKind::Ident(ref i) => {
                            toks.next();
                            s.push_str(i)
                        }
                        _ => break,
                    }
                }
                Ok(Value::Ident(s, QuoteKind::None))
            }
            TokenKind::Keyword(Keyword::Important) => Ok(Value::Important),
            TokenKind::Keyword(Keyword::True) => Ok(Value::True),
            TokenKind::Keyword(Keyword::False) => Ok(Value::False),
            TokenKind::Keyword(Keyword::Null) => Ok(Value::Null),
            TokenKind::Keyword(Keyword::From(s)) => Ok(Value::Ident(s, QuoteKind::None)),
            TokenKind::Keyword(Keyword::Through(s)) => Ok(Value::Ident(s, QuoteKind::None)),
            TokenKind::Keyword(Keyword::To(s)) => Ok(Value::Ident(s, QuoteKind::None)),
            TokenKind::AtRule(_) => Err("expected \";\".".into()),
            TokenKind::Error(e) => return Err(e),
            TokenKind::Op(Op::Plus) | TokenKind::Symbol(Symbol::Plus) => {
                devour_whitespace_or_comment(toks);
                let v = Self::_from_tokens(toks, scope, super_selector)?;
                Ok(Value::UnaryOp(Op::Plus, Box::new(v)))
            }
            TokenKind::Op(Op::Minus) | TokenKind::Symbol(Symbol::Minus) => {
                devour_whitespace_or_comment(toks);
                let v = Self::_from_tokens(toks, scope, super_selector)?;
                Ok(Value::UnaryOp(Op::Minus, Box::new(v)))
            }
            v => {
                dbg!(v);
                panic!("Unexpected token in value parsing")
            }
        }
    }
}
