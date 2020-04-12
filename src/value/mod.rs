use std::cmp::Ordering;
use std::iter::Iterator;

use codemap::{Span, Spanned};

use crate::color::Color;
use crate::common::{Brackets, ListSeparator, Op, QuoteKind};
use crate::error::SassResult;
use crate::unit::{Unit, UNIT_CONVERSION_TABLE};

use css_function::is_special_function;
pub(crate) use map::SassMap;
pub(crate) use number::Number;
pub(crate) use sass_function::SassFunction;

mod css_function;
mod map;
mod number;
mod ops;
mod parse;
mod sass_function;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Value {
    Important,
    True,
    False,
    Null,
    Dimension(Number, Unit),
    List(Vec<Value>, ListSeparator, Brackets),
    Color(Color),
    UnaryOp(Op, Box<Value>),
    BinaryOp(Box<Value>, Op, Box<Value>),
    Paren(Box<Value>),
    Ident(String, QuoteKind),
    Map(SassMap),
    ArgList(Vec<Spanned<Value>>),
    /// Returned by `get-function()`
    Function(SassFunction),
}

impl Value {
    pub fn is_null(&self) -> bool {
        match self {
            &Value::Null => true,
            Value::Ident(i, QuoteKind::None) if i.is_empty() => true,
            _ => false,
        }
    }
    pub fn to_css_string(&self, span: Span) -> SassResult<String> {
        Ok(match self {
            Self::Important => format!("!important"),
            Self::Dimension(num, unit) => match unit {
                Unit::Mul(..) => {
                    return Err((
                        format!("Error: {}{} isn't a valid CSS value.", num, unit),
                        span,
                    )
                        .into());
                }
                _ => format!("{}{}", num, unit),
            },
            Self::Map(map) => format!(
                "({})",
                map.iter()
                    .map(|(k, v)| Ok(format!(
                        "{}: {}",
                        k.to_css_string(span)?,
                        v.to_css_string(span)?
                    )))
                    .collect::<SassResult<Vec<String>>>()?
                    .join(", ")
            ),
            Self::Function(func) => format!("get-function(\"{}\")", func.name()),
            Self::List(vals, sep, brackets) => match brackets {
                Brackets::None => format!(
                    "{}",
                    vals.iter()
                        .filter(|x| !x.is_null())
                        .map(|x| x.to_css_string(span))
                        .collect::<SassResult<Vec<String>>>()?
                        .join(sep.as_str()),
                ),
                Brackets::Bracketed => format!(
                    "[{}]",
                    vals.iter()
                        .filter(|x| !x.is_null())
                        .map(|x| x.to_css_string(span))
                        .collect::<SassResult<Vec<String>>>()?
                        .join(sep.as_str()),
                ),
            },
            Self::Color(c) => format!("{}", c),
            Self::UnaryOp(..) | Self::BinaryOp(..) => {
                format!("{}", self.clone().eval(span)?.to_css_string(span)?)
            }
            Self::Paren(val) => format!("{}", val.to_css_string(span)?),
            Self::Ident(val, kind) => {
                if kind == &QuoteKind::None {
                    return Ok(val.clone());
                }
                let has_single_quotes = val.contains(|x| x == '\'');
                let has_double_quotes = val.contains(|x| x == '"');
                if has_single_quotes && !has_double_quotes {
                    format!("\"{}\"", val)
                } else if !has_single_quotes && has_double_quotes {
                    format!("'{}'", val)
                } else if !has_single_quotes && !has_double_quotes {
                    format!("\"{}\"", val)
                } else {
                    let quote_char = match kind {
                        QuoteKind::Double => '"',
                        QuoteKind::Single => '\'',
                        _ => unreachable!(),
                    };
                    let mut buf = String::with_capacity(val.len() + 2);
                    buf.push(quote_char);
                    for c in val.chars() {
                        match c {
                            '"' | '\'' if c == quote_char => {
                                buf.push('\\');
                                buf.push(quote_char);
                            }
                            v => buf.push(v),
                        }
                    }
                    buf.push(quote_char);
                    buf
                }
            }
            Self::True => "true".to_string(),
            Self::False => "false".to_string(),
            Self::Null => "null".to_string(),
            Self::ArgList(args) => format!(
                "{}",
                args.iter()
                    .filter(|x| !x.is_null())
                    .map(|a| Ok(a.node.to_css_string(span)?))
                    .collect::<SassResult<Vec<String>>>()?
                    .join(", "),
            ),
        })
    }

    pub fn is_true(&self, span: Span) -> SassResult<bool> {
        match self {
            Value::Null | Value::False => Ok(false),
            Self::BinaryOp(..) | Self::Paren(..) | Self::UnaryOp(..) => {
                self.clone().eval(span)?.is_true(span)
            }
            _ => Ok(true),
        }
    }

    pub fn unquote(self) -> Self {
        match self {
            Self::Ident(s1, _) => Self::Ident(s1, QuoteKind::None),
            v => v,
        }
    }

    pub fn span(self, span: Span) -> Spanned<Self> {
        Spanned { node: self, span }
    }

    pub fn kind(&self, span: Span) -> SassResult<&'static str> {
        match self {
            Self::Color(..) => Ok("color"),
            Self::Ident(..) | Self::Important => Ok("string"),
            Self::Dimension(..) => Ok("number"),
            Self::List(..) => Ok("list"),
            Self::Function(..) => Ok("function"),
            Self::ArgList(..) => Ok("arglist"),
            Self::True | Self::False => Ok("bool"),
            Self::Null => Ok("null"),
            Self::Map(..) => Ok("map"),
            Self::BinaryOp(..) | Self::Paren(..) | Self::UnaryOp(..) => {
                self.clone().eval(span)?.kind(span)
            }
        }
    }

    pub fn is_special_function(&self) -> bool {
        match self {
            Self::Ident(s, QuoteKind::None) => is_special_function(s),
            _ => false,
        }
    }

    pub fn bool(b: bool) -> Self {
        if b {
            Value::True
        } else {
            Value::False
        }
    }

    pub fn inspect(&self, span: Span) -> SassResult<String> {
        Ok(match self {
            Value::List(v, _, brackets) if v.is_empty() => match brackets {
                Brackets::None => "()".to_string(),
                Brackets::Bracketed => "[]".to_string(),
            },
            Value::Function(f) => format!("get-function(\"{}\")", f.name()),
            v => v.to_css_string(span)?,
        })
    }

    pub fn equals(self, other: Value, span: Span) -> SassResult<bool> {
        Ok(match self.eval(span)?.node {
            Self::Ident(s1, ..) => match other {
                Self::Ident(s2, ..) => s1 == s2,
                _ => false,
            },
            Self::Dimension(n, unit) => match other {
                Self::Dimension(n2, unit2) => {
                    if !unit.comparable(&unit2) {
                        false
                    } else if unit == unit2 {
                        n == n2
                    } else if unit == Unit::None || unit2 == Unit::None {
                        false
                    } else {
                        n == (n2
                            * UNIT_CONVERSION_TABLE[&unit.to_string()][&unit2.to_string()].clone())
                    }
                }
                _ => false,
            },
            s => s == other.eval(span)?.node,
        })
    }

    pub fn unary_op_plus(self, span: Span) -> SassResult<Self> {
        Ok(match self.eval(span)?.node {
            v @ Value::Dimension(..) => v,
            v => Value::Ident(format!("+{}", v.to_css_string(span)?), QuoteKind::None),
        })
    }

    pub fn eval(self, span: Span) -> SassResult<Spanned<Self>> {
        Ok(match self {
            Self::BinaryOp(lhs, op, rhs) => match op {
                Op::Plus => lhs.add(*rhs, span)?,
                Op::Minus => lhs.sub(*rhs, span)?,
                Op::Equal => Self::bool(lhs.equals(*rhs, span)?),
                Op::NotEqual => Self::bool(!lhs.equals(*rhs, span)?),
                Op::Mul => lhs.mul(*rhs, span)?,
                Op::Div => lhs.div(*rhs, span)?,
                Op::Rem => lhs.rem(*rhs, span)?,
                Op::GreaterThan => return lhs.cmp(*rhs, op, span),
                Op::GreaterThanEqual => return lhs.cmp(*rhs, op, span),
                Op::LessThan => return lhs.cmp(*rhs, op, span),
                Op::LessThanEqual => return lhs.cmp(*rhs, op, span),
                Op::Not => unreachable!(),
                Op::And => {
                    if lhs.clone().is_true(span)? {
                        rhs.eval(span)?.node
                    } else {
                        lhs.eval(span)?.node
                    }
                }
                Op::Or => {
                    if lhs.is_true(span)? {
                        lhs.eval(span)?.node
                    } else {
                        rhs.eval(span)?.node
                    }
                }
            },
            Self::Paren(v) => v.eval(span)?.node,
            Self::UnaryOp(op, val) => match op {
                Op::Plus => val.unary_op_plus(span)?,
                Op::Minus => val.neg(span)?,
                Op::Not => Self::bool(!val.eval(span)?.is_true(span)?),
                _ => unreachable!(),
            },
            _ => self,
        }
        .span(span))
    }

    pub fn cmp(self, mut other: Self, op: Op, span: Span) -> SassResult<Spanned<Value>> {
        if let Self::Paren(..) = other {
            other = other.eval(span)?.node
        }
        let precedence = op.precedence();
        let ordering = match self {
            Self::Dimension(num, unit) => match &other {
                Self::Dimension(num2, unit2) => {
                    if !unit.comparable(&unit2) {
                        return Err(
                            (format!("Incompatible units {} and {}.", unit2, unit), span).into(),
                        );
                    }
                    if &unit == unit2 {
                        num.cmp(num2)
                    } else if unit == Unit::None {
                        num.cmp(num2)
                    } else if unit2 == &Unit::None {
                        num.cmp(num2)
                    } else {
                        num.cmp(
                            &(num2.clone()
                                * UNIT_CONVERSION_TABLE[&unit.to_string()][&unit2.to_string()]
                                    .clone()),
                        )
                    }
                }
                Self::BinaryOp(..) => todo!(),
                v => {
                    return Err((
                        format!(
                            "Undefined operation \"{} {} {}\".",
                            v.to_css_string(span)?,
                            op,
                            other.to_css_string(span)?
                        ),
                        span,
                    )
                        .into())
                }
            },
            Self::BinaryOp(left, op2, right) => {
                return if op2.precedence() >= precedence {
                    Self::BinaryOp(left, op2, right)
                        .eval(span)?
                        .node
                        .cmp(other, op, span)
                } else {
                    Self::BinaryOp(
                        left,
                        op2,
                        Box::new(Self::BinaryOp(right, op, Box::new(other)).eval(span)?.node),
                    )
                    .eval(span)
                }
            }
            Self::UnaryOp(..) | Self::Paren(..) => {
                return self.eval(span)?.node.cmp(other, op, span)
            }
            _ => {
                return Err((
                    format!(
                        "Undefined operation \"{} {} {}\".",
                        self.to_css_string(span)?,
                        op,
                        other.to_css_string(span)?
                    ),
                    span,
                )
                    .into())
            }
        };
        Ok(match op {
            Op::GreaterThan => match ordering {
                Ordering::Greater => Self::True,
                Ordering::Less | Ordering::Equal => Self::False,
            },
            Op::GreaterThanEqual => match ordering {
                Ordering::Greater | Ordering::Equal => Self::True,
                Ordering::Less => Self::False,
            },
            Op::LessThan => match ordering {
                Ordering::Less => Self::True,
                Ordering::Greater | Ordering::Equal => Self::False,
            },
            Op::LessThanEqual => match ordering {
                Ordering::Less | Ordering::Equal => Self::True,
                Ordering::Greater => Self::False,
            },
            _ => unreachable!(),
        }
        .span(span))
    }
}
