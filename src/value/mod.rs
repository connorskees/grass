#![allow(dead_code, unused_variables)]
use std::fmt::{self, Display};
use std::iter::Iterator;

use crate::color::Color;
use crate::common::{ListSeparator, Op, QuoteKind};
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
            Self::BinaryOp(..) => write!(f, "{}", self.eval()),
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
        match self {
            Value::Null | Value::False => false,
            Self::BinaryOp(..) => self.eval().is_true(),
            _ => true,
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
}
