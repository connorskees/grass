#![allow(dead_code, unused_variables)]
use std::fmt::{self, Display};
use std::iter::{Iterator, Peekable};
use std::ops::{Add, Sub};

use crate::args::eat_call_args;
use crate::builtin::GLOBAL_FUNCTIONS;
use crate::color::Color;
use crate::common::{Keyword, Op, QuoteKind, Scope, Symbol};
use crate::units::Unit;
use crate::utils::{devour_whitespace_or_comment, parse_interpolation};
use crate::{Token, TokenKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Dimension {
    val: u64,
}

impl Add for Dimension {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Dimension {
            val: self.val + other.val,
        }
    }
}

impl Display for Dimension {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.val)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ListSeparator {
    Space,
    Comma,
}

impl ListSeparator {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Space => " ",
            Self::Comma => ", ",
        }
    }

    pub fn name(self) -> &'static str {
        match self {
            Self::Space => "space",
            Self::Comma => "comma",
        }
    }
}

impl Display for ListSeparator {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Space => write!(f, " "),
            Self::Comma => write!(f, ", "),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum Value {
    Important,
    True,
    False,
    Null,
    Dimension(Dimension, Unit),
    List(Vec<Value>, ListSeparator),
    Color(Color),
    BinaryOp(Box<Value>, Op, Box<Value>),
    Paren(Box<Value>),
    Ident(String, QuoteKind),
}

impl Add for Value {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        match self {
            // Self::Important => todo!(),
            // Self::True => todo!(),
            // Self::False => todo!(),
            // Self::Null => todo!(),
            Self::Dimension(num, unit) => match other {
                Self::Dimension(num2, unit2) => Value::Dimension(num + num2, unit),
                _ => todo!(),
            },
            // Self::List(..) => todo!(),
            // Self::Color(..) => todo!(),
            // Self::BinaryOp(..) => todo!(),
            // Self::Paren(..) => todo!(),
            Self::Ident(s1, quotes1) => match other {
                Self::Ident(s2, quotes2) => {
                    let quotes = match (quotes1, quotes2) {
                        (QuoteKind::Double, _)
                        | (QuoteKind::Single, _)
                        | (_, QuoteKind::Double)
                        | (_, QuoteKind::Single) => QuoteKind::Double,
                        _ => QuoteKind::None,
                    };
                    Value::Ident(format!("{}{}", s1, s2), quotes)
                }
                Self::Important | Self::True | Self::False | Self::Dimension(..) => {
                    let quotes = match quotes1 {
                        QuoteKind::Double | QuoteKind::Single => QuoteKind::Double,
                        QuoteKind::None => QuoteKind::None,
                    };
                    Value::Ident(format!("{}{}", s1, other), quotes)
                }
                Self::Null => {
                    let quotes = match quotes1 {
                        QuoteKind::Double | QuoteKind::Single => QuoteKind::Double,
                        QuoteKind::None => QuoteKind::None,
                    };
                    Value::Ident(s1, quotes)
                }
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

impl Sub for Value {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        match self {
            // Self::Important => todo!(),
            // Self::True => todo!(),
            // Self::False => todo!(),
            // Self::Null => todo!(),
            Self::Dimension(num, unit) => match other {
                // Self::Dimension(num2, unit2) => Value::Dimension(num - num2, unit),
                _ => todo!(),
            },
            // Self::List(..) => todo!(),
            // Self::Color(..) => todo!(),
            // Self::BinaryOp(..) => todo!(),
            // Self::Paren(..) => todo!(),
            Self::Ident(s1, quotes1) => match other {
                Self::Ident(s2, quotes2) => {
                    let quotes1 = match quotes1 {
                        QuoteKind::Double | QuoteKind::Single => QuoteKind::Double,
                        QuoteKind::None => QuoteKind::None,
                    };
                    let quotes2 = match quotes2 {
                        QuoteKind::Double | QuoteKind::Single => QuoteKind::Double,
                        QuoteKind::None => QuoteKind::None,
                    };
                    Value::Ident(
                        format!("{}{}{}-{}{}{}", quotes1, s1, quotes1, quotes2, s2, quotes2),
                        QuoteKind::None,
                    )
                }
                Self::Important | Self::True | Self::False | Self::Dimension(..) => {
                    let quotes = match quotes1 {
                        QuoteKind::Double | QuoteKind::Single => QuoteKind::Double,
                        QuoteKind::None => QuoteKind::None,
                    };
                    Value::Ident(
                        format!("{}{}{}-{}", quotes, s1, quotes, other),
                        QuoteKind::None,
                    )
                }
                Self::Null => {
                    let quotes = match quotes1 {
                        QuoteKind::Double | QuoteKind::Single => QuoteKind::Double,
                        QuoteKind::None => QuoteKind::None,
                    };
                    Value::Ident(format!("{}{}{}-", quotes, s1, quotes), QuoteKind::None)
                }
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
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
            Self::BinaryOp(lhs, op, rhs) => match op {
                Op::Plus => write!(f, "{}", *lhs.clone() + *rhs.clone()),
                Op::Minus => write!(f, "{}", *lhs.clone() - *rhs.clone()),
                _ => write!(f, "{}{}{}", lhs, op, rhs),
            },
            Self::Paren(val) => write!(f, "{}", val),
            Self::Ident(val, kind) => write!(f, "{}{}{}", kind.as_str(), val, kind.as_str()),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Null => write!(f, "null"),
        }
    }
}

impl Value {
    pub fn is_null(&self) -> bool {
        self == &Value::Null
    }

    pub fn is_true(&self) -> bool {
        todo!()
    }

    pub fn unquote(&mut self) -> &mut Self {
        todo!()
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
                    Dimension {
                        val: val.parse().unwrap(),
                    },
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
                    _ => Some(Value::Ident(s, QuoteKind::None)),
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
