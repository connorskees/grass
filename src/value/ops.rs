use std::cmp::Ordering;

use codemap::{Span, Spanned};

use crate::common::{Op, QuoteKind};
use crate::error::SassResult;
use crate::unit::{Unit, UNIT_CONVERSION_TABLE};
use crate::value::Value;

impl Value {
    pub fn equals(self, mut other: Value, span: Span) -> SassResult<Spanned<Value>> {
        if let Self::Paren(..) = other {
            other = other.eval(span)?.node
        }

        let precedence = Op::Equal.precedence();

        Ok(Value::bool(match self {
            Self::String(s1, ..) => match other {
                Self::String(s2, ..) => s1 == s2,
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
                            * UNIT_CONVERSION_TABLE[unit.to_string().as_str()]
                                [unit2.to_string().as_str()]
                            .clone())
                    }
                }
                _ => false,
            },
            Self::BinaryOp(left, op2, right) => {
                if op2.precedence() >= precedence {
                    Self::BinaryOp(left, op2, right).eval(span)?.node == other
                } else {
                    return Self::BinaryOp(
                        left,
                        op2,
                        Box::new(
                            Self::BinaryOp(right, Op::Equal, Box::new(other))
                                .eval(span)?
                                .node,
                        ),
                    )
                    .eval(span);
                }
            }
            s => s == other.eval(span)?.node,
        })
        .span(span))
    }

    pub fn not_equals(self, mut other: Value, span: Span) -> SassResult<Spanned<Value>> {
        if let Self::Paren(..) = other {
            other = other.eval(span)?.node
        }

        let precedence = Op::Equal.precedence();

        Ok(Value::bool(match self {
            Self::String(s1, ..) => match other {
                Self::String(s2, ..) => s1 != s2,
                _ => true,
            },
            Self::Dimension(n, unit) => match other {
                Self::Dimension(n2, unit2) => {
                    if !unit.comparable(&unit2) {
                        true
                    } else if unit == unit2 {
                        n != n2
                    } else if unit == Unit::None || unit2 == Unit::None {
                        true
                    } else {
                        n != (n2
                            * UNIT_CONVERSION_TABLE[unit.to_string().as_str()]
                                [unit2.to_string().as_str()]
                            .clone())
                    }
                }
                _ => true,
            },
            Self::BinaryOp(left, op2, right) => {
                if op2.precedence() >= precedence {
                    Self::BinaryOp(left, op2, right).eval(span)?.node != other
                } else {
                    return Self::BinaryOp(
                        left,
                        op2,
                        Box::new(
                            Self::BinaryOp(right, Op::NotEqual, Box::new(other))
                                .eval(span)?
                                .node,
                        ),
                    )
                    .eval(span);
                }
            }
            s => s != other.eval(span)?.node,
        })
        .span(span))
    }

    pub fn unary_op_plus(self, span: Span) -> SassResult<Self> {
        Ok(match self.eval(span)?.node {
            v @ Value::Dimension(..) => v,
            v => Value::String(format!("+{}", v.to_css_string(span)?), QuoteKind::None),
        })
    }

    pub fn eval(self, span: Span) -> SassResult<Spanned<Self>> {
        Ok(match self {
            Self::BinaryOp(lhs, op, rhs) => match op {
                Op::Plus => lhs.add(*rhs, span)?,
                Op::Minus => lhs.sub(*rhs, span)?,
                Op::Equal => lhs.equals(*rhs, span)?.node,
                Op::NotEqual => lhs.not_equals(*rhs, span)?.node,
                Op::Mul => lhs.mul(*rhs, span)?,
                Op::Div => lhs.div(*rhs, span)?,
                Op::Rem => lhs.rem(*rhs, span)?,
                Op::GreaterThan => return lhs.cmp(*rhs, op, span),
                Op::GreaterThanEqual => return lhs.cmp(*rhs, op, span),
                Op::LessThan => return lhs.cmp(*rhs, op, span),
                Op::LessThanEqual => return lhs.cmp(*rhs, op, span),
                Op::Not => unreachable!(),
                Op::And => {
                    if lhs.is_true(span)? {
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
                    if !unit.comparable(unit2) {
                        return Err(
                            (format!("Incompatible units {} and {}.", unit2, unit), span).into(),
                        );
                    }
                    if &unit == unit2 || unit == Unit::None || unit2 == &Unit::None {
                        num.cmp(num2)
                    } else {
                        num.cmp(
                            &(num2.clone()
                                * UNIT_CONVERSION_TABLE[unit.to_string().as_str()]
                                    [unit2.to_string().as_str()]
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

    pub fn add(self, mut other: Self, span: Span) -> SassResult<Self> {
        if let Self::Paren(..) = other {
            other = other.eval(span)?.node
        } else if let Self::UnaryOp(..) = other {
            other = other.eval(span)?.node
        }
        let precedence = Op::Plus.precedence();
        Ok(match self {
            Self::Map(..) => {
                return Err((
                    format!("{} isn't a valid CSS value.", self.inspect(span)?),
                    span,
                )
                    .into())
            }
            Self::Function(..) | Self::ArgList(..) => todo!(),
            Self::Important | Self::True | Self::False => match other {
                Self::String(s, QuoteKind::Quoted) => Value::String(
                    format!("{}{}", self.to_css_string(span)?, s),
                    QuoteKind::Quoted,
                ),
                Self::Null => Value::String(self.to_css_string(span)?.into(), QuoteKind::None),
                _ => Value::String(
                    format!(
                        "{}{}",
                        self.to_css_string(span)?,
                        other.to_css_string(span)?
                    ),
                    QuoteKind::None,
                ),
            },
            Self::Null => match other {
                Self::Null => Self::Null,
                _ => Value::String(other.to_css_string(span)?.into(), QuoteKind::None),
            },
            Self::Dimension(num, unit) => match other {
                Self::Dimension(num2, unit2) => {
                    if !unit.comparable(&unit2) {
                        return Err(
                            (format!("Incompatible units {} and {}.", unit2, unit), span).into(),
                        );
                    }
                    if unit == unit2 {
                        Value::Dimension(num + num2, unit)
                    } else if unit == Unit::None {
                        Value::Dimension(num + num2, unit2)
                    } else if unit2 == Unit::None {
                        Value::Dimension(num + num2, unit)
                    } else {
                        Value::Dimension(
                            num + num2
                                * UNIT_CONVERSION_TABLE[unit.to_string().as_str()]
                                    [unit2.to_string().as_str()]
                                .clone(),
                            unit,
                        )
                    }
                }
                Self::String(s, q) => Value::String(format!("{}{}{}", num, unit, s), q),
                Self::Null => Value::String(format!("{}{}", num, unit), QuoteKind::None),
                Self::List(..) => Value::String(
                    format!("{}{}{}", num, unit, other.to_css_string(span)?),
                    QuoteKind::None,
                ),
                Self::True | Self::False => Self::String(
                    format!("{}{}{}", num, unit, other.to_css_string(span)?),
                    QuoteKind::None,
                ),
                _ => {
                    return Err((
                        format!(
                            "Undefined operation \"{}{} + {}\".",
                            num,
                            unit,
                            other.to_css_string(span)?
                        ),
                        span,
                    )
                        .into())
                }
            },
            Self::Color(c) => match other {
                Self::String(s, q) => Value::String(format!("{}{}", c, s), q),
                Self::Null => Value::String(c.to_string(), QuoteKind::None),
                Self::List(..) => Value::String(
                    format!("{}{}", c, other.to_css_string(span)?),
                    QuoteKind::None,
                ),
                _ => {
                    return Err((
                        format!(
                            "Undefined operation \"{} + {}\".",
                            c,
                            other.to_css_string(span)?
                        ),
                        span,
                    )
                        .into())
                }
            },
            Self::BinaryOp(left, op, right) => {
                if op.precedence() >= precedence {
                    Self::BinaryOp(left, op, right)
                        .eval(span)?
                        .node
                        .add(other, span)?
                } else {
                    Self::BinaryOp(
                        left,
                        op,
                        Box::new(
                            Self::BinaryOp(right, Op::Plus, Box::new(other))
                                .eval(span)?
                                .node,
                        ),
                    )
                    .eval(span)?
                    .node
                }
            }
            Self::UnaryOp(..) | Self::Paren(..) => self.eval(span)?.node.add(other, span)?,
            Self::String(text, quotes) => match other {
                Self::String(text2, ..) => Self::String(text + &text2, quotes),
                _ => Value::String(text + &other.to_css_string(span)?, quotes),
            },
            Self::List(..) => match other {
                Self::String(s, q) => {
                    Value::String(format!("{}{}", self.to_css_string(span)?, s), q)
                }
                Self::Paren(..) => (self.add(other.eval(span)?.node, span))?,
                _ => Value::String(
                    format!(
                        "{}{}",
                        self.to_css_string(span)?,
                        other.to_css_string(span)?
                    ),
                    QuoteKind::None,
                ),
            },
        })
    }

    pub fn sub(self, mut other: Self, span: Span) -> SassResult<Self> {
        if let Self::Paren(..) = other {
            other = other.eval(span)?.node
        }
        let precedence = Op::Mul.precedence();
        Ok(match self {
            Self::Null => todo!(),
            Self::Dimension(num, unit) => match other {
                Self::Dimension(num2, unit2) => {
                    if !unit.comparable(&unit2) {
                        return Err(
                            (format!("Incompatible units {} and {}.", unit2, unit), span).into(),
                        );
                    }
                    if unit == unit2 {
                        Value::Dimension(num - num2, unit)
                    } else if unit == Unit::None {
                        Value::Dimension(num - num2, unit2)
                    } else if unit2 == Unit::None {
                        Value::Dimension(num - num2, unit)
                    } else {
                        Value::Dimension(
                            num - num2
                                * UNIT_CONVERSION_TABLE[unit.to_string().as_str()]
                                    [unit2.to_string().as_str()]
                                .clone(),
                            unit,
                        )
                    }
                }
                Self::List(..) => Value::String(
                    format!("{}{}-{}", num, unit, other.to_css_string(span)?),
                    QuoteKind::None,
                ),
                Self::String(..) => Value::String(
                    format!("{}{}-{}", num, unit, other.to_css_string(span)?),
                    QuoteKind::None,
                ),
                _ => todo!(),
            },
            Self::Color(c) => match other {
                Self::String(s, q) => {
                    Value::String(format!("{}-{}{}{}", c, q, s, q), QuoteKind::None)
                }
                Self::Null => Value::String(format!("{}-", c), QuoteKind::None),
                Self::Dimension(..) | Self::Color(..) => {
                    return Err((
                        format!(
                            "Undefined operation \"{} - {}\".",
                            c,
                            other.to_css_string(span)?
                        ),
                        span,
                    )
                        .into())
                }
                _ => Value::String(
                    format!("{}-{}", c, other.to_css_string(span)?),
                    QuoteKind::None,
                ),
            },
            Self::BinaryOp(left, op, right) => {
                if op.precedence() >= precedence {
                    Self::BinaryOp(left, op, right)
                        .eval(span)?
                        .node
                        .sub(other, span)?
                } else {
                    Self::BinaryOp(
                        left,
                        op,
                        Box::new(
                            Self::BinaryOp(right, Op::Minus, Box::new(other))
                                .eval(span)?
                                .node,
                        ),
                    )
                    .eval(span)?
                    .node
                }
            }
            Self::Paren(..) => self.eval(span)?.node.sub(other, span)?,
            Self::String(..) => Self::String(
                format!(
                    "{}-{}",
                    self.to_css_string(span)?,
                    other.to_css_string(span)?
                ),
                QuoteKind::None,
            ),
            Self::List(..) => match other {
                Self::String(s, q) => Value::String(
                    format!("{}-{}{}{}", self.to_css_string(span)?, q, s, q),
                    QuoteKind::None,
                ),
                _ => Value::String(
                    format!(
                        "{}-{}",
                        self.to_css_string(span)?,
                        other.to_css_string(span)?
                    ),
                    QuoteKind::None,
                ),
            },
            _ => match other {
                Self::String(s, q) => Value::String(
                    format!("{}-{}{}{}", self.to_css_string(span)?, q, s, q),
                    QuoteKind::None,
                ),
                Self::Null => {
                    Value::String(format!("{}-", self.to_css_string(span)?), QuoteKind::None)
                }
                _ => Value::String(
                    format!(
                        "{}-{}",
                        self.to_css_string(span)?,
                        other.to_css_string(span)?
                    ),
                    QuoteKind::None,
                ),
            },
        })
    }

    pub fn mul(self, mut other: Self, span: Span) -> SassResult<Self> {
        if let Self::Paren(..) = other {
            other = other.eval(span)?.node
        }
        let precedence = Op::Mul.precedence();
        Ok(match self {
            Self::Null => todo!(),
            Self::Dimension(num, unit) => match other {
                Self::Dimension(num2, unit2) => {
                    if unit == Unit::None {
                        Value::Dimension(num * num2, unit2)
                    } else if unit2 == Unit::None {
                        Value::Dimension(num * num2, unit)
                    } else if let Unit::Mul(mut u) = unit {
                        u.push(unit2);
                        Value::Dimension(num * num2, Unit::Mul(u))
                    } else if let Unit::Mul(u2) = unit2 {
                        let mut u = vec![unit];
                        u.extend(u2);
                        Value::Dimension(num * num2, Unit::Mul(u))
                    } else {
                        Value::Dimension(num * num2, Unit::Mul(vec![unit, unit2]))
                    }
                }
                _ => {
                    return Err((
                        format!(
                            "Undefined operation \"{}{} * {}\".",
                            num,
                            unit,
                            other.to_css_string(span)?
                        ),
                        span,
                    )
                        .into())
                }
            },
            Self::BinaryOp(left, op, right) => {
                if op.precedence() >= precedence {
                    Self::BinaryOp(left, op, right)
                        .eval(span)?
                        .node
                        .mul(other, span)?
                } else {
                    Self::BinaryOp(
                        left,
                        op,
                        Box::new(
                            Self::BinaryOp(right, Op::Mul, Box::new(other))
                                .eval(span)?
                                .node,
                        ),
                    )
                    .eval(span)?
                    .node
                }
            }
            Self::UnaryOp(..) | Self::Paren(..) => self.eval(span)?.node.mul(other, span)?,
            _ => {
                return Err((
                    format!(
                        "Undefined operation \"{} * {}\".",
                        self.to_css_string(span)?,
                        other.to_css_string(span)?
                    ),
                    span,
                )
                    .into())
            }
        })
    }

    pub fn div(self, other: Self, span: Span) -> SassResult<Self> {
        let precedence = Op::Div.precedence();
        Ok(match self {
            Self::Null => todo!(),
            Self::Dimension(num, unit) => match other {
                Self::Dimension(num2, unit2) => {
                    if !unit.comparable(&unit2) {
                        return Err(
                            (format!("Incompatible units {} and {}.", unit2, unit), span).into(),
                        );
                    }
                    if unit == unit2 {
                        Value::Dimension(num / num2, Unit::None)
                    } else if unit == Unit::None {
                        todo!("inverse units")
                    } else if unit2 == Unit::None {
                        Value::Dimension(num / num2, unit)
                    } else {
                        Value::Dimension(
                            num / (num2
                                * UNIT_CONVERSION_TABLE[unit.to_string().as_str()]
                                    [unit2.to_string().as_str()]
                                .clone()),
                            Unit::None,
                        )
                    }
                }
                Self::String(s, q) => {
                    Value::String(format!("{}{}/{}{}{}", num, unit, q, s, q), QuoteKind::None)
                }
                Self::BinaryOp(..) | Self::Paren(..) => {
                    Self::Dimension(num, unit).div(other.eval(span)?.node, span)?
                }
                _ => todo!(),
            },
            Self::Color(c) => match other {
                Self::String(s, q) => {
                    Value::String(format!("{}/{}{}{}", c, q, s, q), QuoteKind::None)
                }
                Self::Null => Value::String(format!("{}/", c), QuoteKind::None),
                Self::Dimension(..) | Self::Color(..) => {
                    return Err((
                        format!(
                            "Undefined operation \"{} / {}\".",
                            c,
                            other.to_css_string(span)?
                        ),
                        span,
                    )
                        .into())
                }
                _ => Value::String(
                    format!("{}/{}", c, other.to_css_string(span)?),
                    QuoteKind::None,
                ),
            },
            Self::BinaryOp(left, op, right) => {
                if op.precedence() >= precedence {
                    Self::BinaryOp(left, op, right)
                        .eval(span)?
                        .node
                        .div(other, span)?
                } else {
                    Self::BinaryOp(
                        left,
                        op,
                        Box::new(
                            Self::BinaryOp(right, Op::Div, Box::new(other))
                                .eval(span)?
                                .node,
                        ),
                    )
                    .eval(span)?
                    .node
                }
            }
            Self::Paren(..) => self.eval(span)?.node.div(other, span)?,
            Self::String(s1, q1) => match other {
                Self::String(s2, q2) => Value::String(
                    format!("{}{}{}/{}{}{}", q1, s1, q1, q2, s2, q2),
                    QuoteKind::None,
                ),
                Self::Important
                | Self::True
                | Self::False
                | Self::Dimension(..)
                | Self::Color(..) => Value::String(
                    format!("{}{}{}/{}", q1, s1, q1, other.to_css_string(span)?),
                    QuoteKind::None,
                ),
                Self::Null => Value::String(format!("{}{}{}/", q1, s1, q1), QuoteKind::None),
                _ => todo!(),
            },
            _ => match other {
                Self::String(s, q) => Value::String(
                    format!("{}/{}{}{}", self.to_css_string(span)?, q, s, q),
                    QuoteKind::None,
                ),
                Self::Null => {
                    Value::String(format!("{}/", self.to_css_string(span)?), QuoteKind::None)
                }
                _ => Value::String(
                    format!(
                        "{}/{}",
                        self.to_css_string(span)?,
                        other.to_css_string(span)?
                    ),
                    QuoteKind::None,
                ),
            },
        })
    }

    pub fn rem(self, other: Self, span: Span) -> SassResult<Self> {
        Ok(match self {
            Value::Dimension(n, u) => match other {
                Value::Dimension(n2, u2) => {
                    if !u.comparable(&u2) {
                        return Err((format!("Incompatible units {} and {}.", u2, u), span).into());
                    }
                    if u == u2 {
                        Value::Dimension(n % n2, u)
                    } else if u == Unit::None {
                        Value::Dimension(n % n2, u2)
                    } else if u2 == Unit::None {
                        Value::Dimension(n % n2, u)
                    } else {
                        Value::Dimension(n, u)
                    }
                }
                _ => {
                    return Err((
                        format!(
                            "Undefined operation \"{} % {}\".",
                            Value::Dimension(n, u).to_css_string(span)?,
                            other.to_css_string(span)?
                        ),
                        span,
                    )
                        .into())
                }
            },
            _ => {
                return Err((
                    format!(
                        "Undefined operation \"{} % {}\".",
                        self.to_css_string(span)?,
                        other.to_css_string(span)?
                    ),
                    span,
                )
                    .into())
            }
        })
    }

    pub fn neg(self, span: Span) -> SassResult<Self> {
        Ok(match self.eval(span)?.node {
            Value::Dimension(n, u) => Value::Dimension(-n, u),
            v => Value::String(format!("-{}", v.to_css_string(span)?), QuoteKind::None),
        })
    }
}
