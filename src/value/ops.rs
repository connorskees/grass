use codemap::Span;

use crate::common::{Op, QuoteKind};
use crate::error::SassResult;
use crate::unit::{Unit, UNIT_CONVERSION_TABLE};
use crate::value::Value;

impl Value {
    pub fn add(self, mut other: Self, span: Span) -> SassResult<Self> {
        if let Self::Paren(..) = other {
            other = other.eval(span)?.node
        } else if let Self::UnaryOp(..) = other {
            other = other.eval(span)?.node
        }
        let precedence = Op::Plus.precedence();
        Ok(match self {
            Self::Function(..) | Self::ArgList(..) | Self::Map(..) => todo!(),
            Self::Important | Self::True | Self::False => match other {
                Self::Ident(s, QuoteKind::Quoted) => Value::Ident(
                    format!("{}{}", self.to_css_string(span)?, s),
                    QuoteKind::Quoted,
                ),
                Self::Null => Value::Ident(self.to_css_string(span)?.into(), QuoteKind::None),
                _ => Value::Ident(
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
                _ => Value::Ident(other.to_css_string(span)?.into(), QuoteKind::None),
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
                Self::Ident(s, q) => Value::Ident(format!("{}{}{}", num, unit, s), q),
                Self::Null => Value::Ident(format!("{}{}", num, unit), QuoteKind::None),
                Self::List(..) => Value::Ident(
                    format!("{}{}{}", num, unit, other.to_css_string(span)?),
                    QuoteKind::None,
                ),
                Self::True | Self::False => Self::Ident(
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
                Self::Ident(s, q) => Value::Ident(format!("{}{}", c, s), q),
                Self::Null => Value::Ident(c.to_string(), QuoteKind::None),
                Self::List(..) => Value::Ident(
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
            Self::Ident(text, quotes) => match other {
                Self::Ident(text2, ..) => Self::Ident(text + &text2, quotes),
                _ => Value::Ident(text + &other.to_css_string(span)?, quotes),
            },
            Self::List(..) => match other {
                Self::Ident(s, q) => Value::Ident(format!("{}{}", self.to_css_string(span)?, s), q),
                Self::Paren(..) => (self.add(other.eval(span)?.node, span))?,
                _ => Value::Ident(
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
                Self::List(..) => Value::Ident(
                    format!("{}{}-{}", num, unit, other.to_css_string(span)?),
                    QuoteKind::None,
                ),
                Self::Ident(..) => Value::Ident(
                    format!("{}{}-{}", num, unit, other.to_css_string(span)?),
                    QuoteKind::None,
                ),
                _ => todo!(),
            },
            Self::Color(c) => match other {
                Self::Ident(s, q) => {
                    Value::Ident(format!("{}-{}{}{}", c, q, s, q), QuoteKind::None)
                }
                Self::Null => Value::Ident(format!("{}-", c), QuoteKind::None),
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
                _ => Value::Ident(
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
            Self::Ident(..) => Self::Ident(
                format!(
                    "{}-{}",
                    self.to_css_string(span)?,
                    other.to_css_string(span)?
                ),
                QuoteKind::None,
            ),
            Self::List(..) => match other {
                Self::Ident(s, q) => Value::Ident(
                    format!("{}-{}{}{}", self.to_css_string(span)?, q, s, q),
                    QuoteKind::None,
                ),
                _ => Value::Ident(
                    format!(
                        "{}-{}",
                        self.to_css_string(span)?,
                        other.to_css_string(span)?
                    ),
                    QuoteKind::None,
                ),
            },
            _ => match other {
                Self::Ident(s, q) => Value::Ident(
                    format!("{}-{}{}{}", self.to_css_string(span)?, q, s, q),
                    QuoteKind::None,
                ),
                Self::Null => {
                    Value::Ident(format!("{}-", self.to_css_string(span)?), QuoteKind::None)
                }
                _ => Value::Ident(
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
                Self::Ident(s, q) => {
                    Value::Ident(format!("{}{}/{}{}{}", num, unit, q, s, q), QuoteKind::None)
                }
                Self::BinaryOp(..) | Self::Paren(..) => {
                    Self::Dimension(num, unit).div(other.eval(span)?.node, span)?
                }
                _ => todo!(),
            },
            Self::Color(c) => match other {
                Self::Ident(s, q) => {
                    Value::Ident(format!("{}/{}{}{}", c, q, s, q), QuoteKind::None)
                }
                Self::Null => Value::Ident(format!("{}/", c), QuoteKind::None),
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
                _ => Value::Ident(
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
            Self::Ident(s1, q1) => match other {
                Self::Ident(s2, q2) => Value::Ident(
                    format!("{}{}{}/{}{}{}", q1, s1, q1, q2, s2, q2),
                    QuoteKind::None,
                ),
                Self::Important
                | Self::True
                | Self::False
                | Self::Dimension(..)
                | Self::Color(..) => Value::Ident(
                    format!("{}{}{}/{}", q1, s1, q1, other.to_css_string(span)?),
                    QuoteKind::None,
                ),
                Self::Null => Value::Ident(format!("{}{}{}/", q1, s1, q1), QuoteKind::None),
                _ => todo!(),
            },
            _ => match other {
                Self::Ident(s, q) => Value::Ident(
                    format!("{}/{}{}{}", self.to_css_string(span)?, q, s, q),
                    QuoteKind::None,
                ),
                Self::Null => {
                    Value::Ident(format!("{}/", self.to_css_string(span)?), QuoteKind::None)
                }
                _ => Value::Ident(
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
            v => Value::Ident(format!("-{}", v.to_css_string(span)?), QuoteKind::None),
        })
    }
}
