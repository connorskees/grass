#![allow(dead_code, unused_variables)]
use std::convert::{TryFrom, TryInto};
use std::fmt::{self, Display};
use std::iter::{Iterator, Peekable};

use num_rational::BigRational;
use num_bigint::BigInt;

use crate::args::eat_call_args;
use crate::builtin::GLOBAL_FUNCTIONS;
use crate::color::Color;
use crate::common::{Keyword, ListSeparator, Op, QuoteKind, Scope, Symbol};
use crate::units::Unit;
use crate::utils::{devour_whitespace_or_comment, parse_interpolation};
use crate::{Token, TokenKind};

mod ops;

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum Value {
    Important,
    True,
    False,
    Null,
    Dimension(BigRational, Unit),
    List(Vec<Value>, ListSeparator),
    Color(Color),
    BinaryOp(Box<Value>, Op, Box<Value>),
    Paren(Box<Value>),
    Ident(String, QuoteKind),
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Important => write!(f, "!important"),
            Self::Dimension(num, unit) => write!(f, "{}{}", num, unit),
            Self::List(vals, sep) => write!(
                f,
                "{}",
                vals.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(sep.as_str())
            ),
            Self::Color(c) => write!(f, "{}", c),
            Self::BinaryOp(..) => write!(f, "{}", self.eval()),
            Self::Paren(val) => write!(f, "{}", val),
            Self::Ident(val, kind) => write!(f, "{}{}{}", kind.as_str(), val, kind.as_str()),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Null => write!(f, "null"),
        }
    }
}

impl TryInto<u16> for Value {
    type Error = &'static str;
    fn try_into(self) -> Result<u16, Self::Error> {
        match self {
            Self::BinaryOp(..) => self.eval().try_into(),
            Self::Dimension(n, Unit::Percent) => {
                todo!()
            }
            Self::Dimension(n, Unit::None) => {
                if n >= BigRational::from_integer(BigInt::from(255)) {
                    Ok(255)
                } else {
                    Ok(n.to_integer().to_str_radix(10).parse().unwrap())
                }
            }
            Self::Dimension(n, _) => {
                Err("Expected `val` to have no units or \"%\".")
            }
            _ => Err("expected number")
        }
    }
}

impl Value {
    pub fn is_null(&self) -> bool {
        self == &Value::Null
    }

    pub fn is_true(&self) -> bool {
        match self {
            Value::Null | Value::False => false,
            Self::BinaryOp(..) => self.eval().is_true(),
            _ => true
        }
    }

    pub fn unquote(self) -> Self {
        match self {
            Self::Ident(s1, _) => Self::Ident(s1, QuoteKind::None),
            _ => todo!(),
        }
    }

    pub fn kind(&self) -> &'static str {
        match self {
            Value::Color(..) => "color",
            Value::Ident(..) => "string",
            Value::Dimension(..) => "number",
            Value::List(..) => "list",
            // Value::Function(..) => "function",
            Value::True | Value::False => "bool",
            Value::Null => "null",
            Value::BinaryOp(..) => self.eval().kind(),
            _ => "unknown",
        }
    }

    pub fn bool(b: bool) -> Self {
        if b {
            Value::True
        } else {
            Value::False
        }
    }

    pub fn eval(&self) -> Self {
        match self {
            Self::BinaryOp(lhs, op, rhs) => match op {
                Op::Plus => *lhs.clone() + *rhs.clone(),
                Op::Minus => *lhs.clone() - *rhs.clone(),
                Op::Equal => Self::bool(*lhs == *rhs),
                Op::NotEqual => Self::bool(*lhs != *rhs),
                _ => Self::BinaryOp(lhs.clone(), *op, rhs.clone()),
            },
            _ => self.clone(),
        }
    }

    pub fn from_tokens<I: Iterator<Item = Token>>(
        toks: &mut Peekable<I>,
        scope: &Scope,
    ) -> Option<Self> {
        let left = Self::_from_tokens(toks, scope)?;
        let whitespace = devour_whitespace_or_comment(toks);
        let next = match toks.peek() {
            Some(x) => x,
            None => return Some(left),
        };
        match next.kind {
            TokenKind::Symbol(Symbol::SemiColon) | TokenKind::Symbol(Symbol::CloseParen) => {
                Some(left)
            }
            TokenKind::Symbol(Symbol::Comma) => {
                toks.next();
                devour_whitespace_or_comment(toks);
                let right = match Self::from_tokens(toks, scope) {
                    Some(x) => x,
                    None => return Some(left),
                };
                Some(Value::List(vec![left, right], ListSeparator::Comma))
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
                let right = match Self::from_tokens(toks, scope) {
                    Some(x) => x,
                    None => return Some(left),
                };
                Some(Value::BinaryOp(Box::new(left), op, Box::new(right)))
            }
            _ if whitespace => {
                devour_whitespace_or_comment(toks);
                let right = match Self::from_tokens(toks, scope) {
                    Some(x) => x,
                    None => return Some(left),
                };
                Some(Value::List(vec![left, right], ListSeparator::Space))
            }
            _ => {
                dbg!(&next.kind);
                todo!("unimplemented token in value")
            }
        }
    }

    fn _from_tokens<I: Iterator<Item = Token>>(
        toks: &mut Peekable<I>,
        scope: &Scope,
    ) -> Option<Self> {
        let kind = if let Some(tok) = toks.next() {
            tok.kind
        } else {
            return None;
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
                Some(Value::Dimension(
                    val.parse().expect("error parsing integer"),
                    unit,
                ))
            }
            TokenKind::Symbol(Symbol::OpenParen) => {
                devour_whitespace_or_comment(toks);
                let val = Self::from_tokens(toks, scope)?;
                assert_eq!(
                    toks.next().unwrap().kind,
                    TokenKind::Symbol(Symbol::CloseParen)
                );
                Some(Value::Paren(Box::new(val)))
            }
            TokenKind::Symbol(Symbol::BitAnd) => {
                Some(Value::Ident(String::from("&"), QuoteKind::None))
            }
            TokenKind::Symbol(Symbol::Hash) => {
                let mut s = String::new();
                while let Some(tok) = toks.peek() {
                    match tok.kind.clone() {
                        TokenKind::Interpolation => {
                            toks.next();
                            s.push_str(
                                &parse_interpolation(toks, scope)
                                    .iter()
                                    .map(|x| x.kind.to_string())
                                    .collect::<String>(),
                            )
                        }
                        TokenKind::Ident(ref i) => {
                            toks.next();
                            s.push_str(i)
                        }
                        TokenKind::Number(ref n) => {
                            toks.next();
                            s.push_str(n)
                        }
                        _ => break,
                    }
                }

                match s.len() {
                    3 => {
                        let v = u16::from_str_radix(&s, 16).unwrap();
                        let red = ((v & 0xf00) >> 8) * 0x11;
                        let green = ((v & 0x0f0) >> 4) * 0x11;
                        let blue = (v & 0x00f) * 0x11;
                        Some(Value::Color(Color::new(
                            red,
                            green,
                            blue,
                            1,
                            format!("#{}", s),
                        )))
                    }
                    4 => {
                        let v = u16::from_str_radix(&s, 16).unwrap();
                        let red = ((v & 0xf000) >> 12) * 0x11;
                        let green = ((v & 0x0f00) >> 8) * 0x11;
                        let blue = ((v & 0x00f0) >> 4) * 0x11;
                        let alpha = (v & 0x000f) * 0x11;
                        Some(Value::Color(Color::new(
                            red,
                            green,
                            blue,
                            alpha,
                            format!("#{}", s),
                        )))
                    }
                    6 => {
                        let v = u32::from_str_radix(&s, 16).unwrap();
                        let red: u16 = ((v & 0xff0000) >> 16) as u16;
                        let green: u16 = ((v & 0x00ff00) >> 8) as u16;
                        let blue: u16 = (v & 0x0000ff) as u16;
                        Some(Value::Color(Color::new(
                            red,
                            green,
                            blue,
                            1,
                            format!("#{}", s),
                        )))
                    }
                    8 => {
                        let v = u32::from_str_radix(&s, 16).unwrap();
                        let red = ((v & 0xff000000) >> 24) as u16;
                        let green = ((v & 0x00ff0000) >> 16) as u16;
                        let blue = ((v & 0x0000ff00) >> 8) as u16;
                        let alpha = (v & 0x000000ff) as u16;
                        Some(Value::Color(Color::new(
                            red,
                            green,
                            blue,
                            alpha,
                            format!("#{}", s),
                        )))
                    }
                    _ => Some(Value::Ident(s, QuoteKind::None)),
                }
            }
            // TokenKind::Interpolation => {
            //     Some(Value::Ident(
            //         parse_interpolation(toks, scope)
            //             .iter()
            //             .map(|x| x.kind.to_string())
            //             .collect::<String>(),
            //             QuoteKind::None
            //     ))
            // }
            TokenKind::Ident(mut s) => {
                while let Some(tok) = toks.peek() {
                    match tok.kind.clone() {
                        TokenKind::Interpolation => {
                            toks.next();
                            s.push_str(
                                &parse_interpolation(toks, scope)
                                    .iter()
                                    .map(|x| x.kind.to_string())
                                    .collect::<String>(),
                            )
                        }
                        TokenKind::Ident(ref i) => {
                            toks.next();
                            s.push_str(i)
                        }
                        _ => break,
                    }
                }
                match toks.peek() {
                    Some(Token {
                        kind: TokenKind::Symbol(Symbol::OpenParen),
                        ..
                    }) => {
                        toks.next();
                        let args = eat_call_args(toks, scope);
                        let func = match scope.functions.get(&s) {
                            Some(f) => f,
                            None => match GLOBAL_FUNCTIONS.get(&s) {
                                Some(f) => return f(&args),
                                None => todo!("called undefined function"),
                            },
                        };
                        Some(func.clone().args(&args).call())
                    }
                    _ => if let Ok(c) = crate::color::ColorName::try_from(s.as_ref()) {
                        Some(Value::Color(c.into_color(s)))
                    } else {
                        Some(Value::Ident(s, QuoteKind::None))
                    }
                }
            }
            TokenKind::Symbol(Symbol::DoubleQuote) => {
                let mut s = String::new();
                let mut is_escaped = false;
                while let Some(tok) = toks.next() {
                    match tok.kind {
                        TokenKind::Symbol(Symbol::DoubleQuote) if !is_escaped => break,
                        TokenKind::Symbol(Symbol::BackSlash) if !is_escaped => is_escaped = true,
                        TokenKind::Symbol(Symbol::BackSlash) => s.push('\\'),
                        _ => {}
                    }
                    if is_escaped && tok.kind != TokenKind::Symbol(Symbol::BackSlash) {
                        is_escaped = false;
                    }
                    s.push_str(&tok.kind.to_string());
                }
                Some(Value::Ident(s, QuoteKind::Double))
            }
            TokenKind::Symbol(Symbol::SingleQuote) => {
                let mut s = String::new();
                while let Some(tok) = toks.next() {
                    if tok.kind == TokenKind::Symbol(Symbol::SingleQuote) {
                        break;
                    }
                    s.push_str(&tok.kind.to_string());
                }
                Some(Value::Ident(s, QuoteKind::Single))
            }
            TokenKind::Variable(ref v) => {
                Some(scope.vars.get(v).expect("expected variable").clone())
            }
            TokenKind::Interpolation => {
                let mut s = parse_interpolation(toks, scope)
                    .iter()
                    .map(|x| x.kind.to_string())
                    .collect::<String>();
                while let Some(tok) = toks.peek() {
                    match tok.kind.clone() {
                        TokenKind::Interpolation => {
                            toks.next();
                            s.push_str(
                                &parse_interpolation(toks, scope)
                                    .iter()
                                    .map(|x| x.kind.to_string())
                                    .collect::<String>(),
                            )
                        }
                        TokenKind::Ident(ref i) => {
                            toks.next();
                            s.push_str(i)
                        }
                        _ => break,
                    }
                }
                Some(Value::Ident(s, QuoteKind::None))
            }
            TokenKind::Keyword(Keyword::Important) => Some(Value::Important),
            TokenKind::Keyword(Keyword::True) => Some(Value::True),
            TokenKind::Keyword(Keyword::False) => Some(Value::False),
            TokenKind::Keyword(Keyword::Null) => Some(Value::Null),
            _ => None,
        }
    }
}
