use std::fmt::{self, Display, Write};
use std::iter::Iterator;

use crate::color::Color;
use crate::common::{ListSeparator, Op, QuoteKind};
use crate::error::SassResult;
use crate::unit::Unit;
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
    UnaryOp(Op, Box<Value>),
    BinaryOp(Box<Value>, Op, Box<Value>),
    Paren(Box<Value>),
    Ident(String, QuoteKind),
    // Returned by `get-function()`
    // Function(String),
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Important => write!(f, "!important"),
            Self::Dimension(num, unit) => match unit {
                Unit::Mul(..) => {
                    eprintln!("Error: {}{} isn't a valid CSS value.", num, unit);
                    std::process::exit(1);
                }
                _ => write!(f, "{}{}", num, unit),
            },
            Self::List(vals, sep) => write!(
                f,
                "{}",
                vals.iter()
                    .map(std::string::ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(sep.as_str())
            ),
            Self::Color(c) => write!(f, "{}", c),
            Self::UnaryOp(..) | Self::BinaryOp(..) => write!(
                f,
                "{}",
                match self.clone().eval() {
                    Ok(v) => v,
                    Err(e) => {
                        eprintln!("{}", e);
                        std::process::exit(1);
                    }
                }
            ),
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
                } else if !has_single_quotes && !has_double_quotes {
                    write!(f, "\"{}\"", val)
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
        match self {
            &Value::Null => true,
            Value::Ident(i, QuoteKind::None) if i.is_empty() => true,
            _ => false,
        }
    }

    pub fn is_true(&self) -> SassResult<bool> {
        match self {
            Value::Null | Value::False => Ok(false),
            Self::BinaryOp(..) | Self::Paren(..) | Self::UnaryOp(..) => self.clone().eval()?.is_true(),
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
            Self::Color(..) => Ok("color"),
            Self::Ident(..) | Self::Important => Ok("string"),
            Self::Dimension(..) => Ok("number"),
            Self::List(..) => Ok("list"),
            // Self::Function(..) => Ok("function"),
            Self::True | Self::False => Ok("bool"),
            Self::Null => Ok("null"),
            Self::BinaryOp(..) | Self::Paren(..) | Self::UnaryOp(..) => self.clone().eval()?.kind(),
        }
    }

    pub fn bool(b: bool) -> Self {
        if b {
            Value::True
        } else {
            Value::False
        }
    }

    pub fn equals(self, other: Value) -> SassResult<bool> {
        Ok(match self.eval()? {
            Self::Ident(s1, ..) => match other {
                Self::Ident(s2, ..) => s1 == s2,
                _ => false,
            },
            s => s == other.eval()?,
        })
    }

    pub fn unary_op_plus(self) -> SassResult<Self> {
        Ok(match self.eval()? {
            v @ Value::Dimension(..) => v,
            v => Value::Ident(format!("+{}", v), QuoteKind::None),
        })
    }

    pub fn eval(self) -> SassResult<Self> {
        match self {
            Self::BinaryOp(lhs, op, rhs) => match op {
                Op::Plus => *lhs + *rhs,
                Op::Minus => *lhs - *rhs,
                Op::Equal => Ok(Self::bool(lhs.equals(*rhs)?)),
                Op::NotEqual => Ok(Self::bool(!lhs.equals(*rhs)?)),
                Op::Mul => *lhs * *rhs,
                Op::Div => *lhs / *rhs,
                Op::Rem => *lhs % *rhs,
                _ => Ok(Self::BinaryOp(lhs, op, rhs)),
            },
            Self::Paren(v) => v.eval(),
            Self::UnaryOp(op, val) => match op {
                Op::Plus => val.unary_op_plus(),
                Op::Minus => -*val,
                _ => unreachable!(),
            },
            _ => Ok(self),
        }
    }
}
