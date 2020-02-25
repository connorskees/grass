#![allow(dead_code, unused_variables)]
use std::fmt::{self, Display, Write};
use std::iter::Iterator;

use crate::color::Color;
use crate::common::{ListSeparator, Op, QuoteKind};
use crate::error::SassResult;
use crate::units::Unit;
pub(crate) use number::Number;

mod number;
mod ops;
mod parse;

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum Value {
    Important,
    True,
    False,
    Null,
    Dimension(Number, Unit),
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
                    .map(std::string::ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(sep.as_str())
            ),
            Self::Color(c) => write!(f, "{}", c),
            Self::BinaryOp(..) => write!(f, "{}", self.clone().eval().unwrap()),
            Self::Paren(val) => write!(f, "{}", val),
            Self::Ident(val, kind) => {
                if kind == &QuoteKind::None {
                    return write!(f, "{}", val);
                }
                let has_single_quotes = val.contains(|x| x == '\'');
                let has_double_quotes = val.contains(|x| x == '"');
                if has_single_quotes && !has_double_quotes {
                    write!(f, "\"{}\"", val)
                } else if !has_single_quotes && has_double_quotes {
                    write!(f, "'{}'", val)
                } else {
                    let quote_char = match kind {
                        QuoteKind::Double => '"',
                        QuoteKind::Single => '\'',
                        _ => unreachable!(),
                    };
                    f.write_char(quote_char)?;
                    for c in val.chars() {
                        match c {
                            '"' | '\'' if c == quote_char => {
                                f.write_char('\\')?;
                                f.write_char(quote_char)?;
                            }
                            v => f.write_char(v)?,
                        }
                    }
                    f.write_char(quote_char)?;
                    Ok(())
                }
            }
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

    pub fn is_true(&self) -> SassResult<bool> {
        match self {
            Value::Null | Value::False => Ok(false),
            Self::BinaryOp(..) => self.clone().eval()?.is_true(),
            _ => Ok(true),
        }
    }

    pub fn unquote(self) -> Self {
        match self {
            Self::Ident(s1, _) => Self::Ident(s1, QuoteKind::None),
            v => v,
        }
    }

    pub fn kind(&self) -> SassResult<&'static str> {
        match self {
            Value::Color(..) => Ok("color"),
            Value::Ident(..) => Ok("string"),
            Value::Dimension(..) => Ok("number"),
            Value::List(..) => Ok("list"),
            // Value::Function(..) => Ok("function"),
            Value::True | Value::False => Ok("bool"),
            Value::Null => Ok("null"),
            Value::BinaryOp(..) => self.clone().eval()?.kind(),
            _ => Ok("unknown"),
        }
    }

    pub fn bool(b: bool) -> Self {
        if b {
            Value::True
        } else {
            Value::False
        }
    }

    pub fn eval(self) -> SassResult<Self> {
        match self {
            Self::BinaryOp(lhs, op, rhs) => match op {
                Op::Plus => *lhs + *rhs,
                Op::Minus => *lhs - *rhs,
                Op::Equal => Ok(Self::bool(*lhs == *rhs)),
                Op::NotEqual => Ok(Self::bool(*lhs != *rhs)),
                Op::Mul => *lhs * *rhs,
                Op::Div => *lhs / *rhs,
                _ => Ok(Self::BinaryOp(lhs, op, rhs)),
            },
            _ => Ok(self),
        }
    }
}
