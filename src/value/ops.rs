use std::ops::{Add, Div, Mul, Neg, Rem, Sub};

use crate::common::{Op, QuoteKind};
use crate::error::SassResult;
use crate::unit::{Unit, UNIT_CONVERSION_TABLE};
use crate::value::Value;

impl Add for Value {
    type Output = SassResult<Self>;

    fn add(self, mut other: Self) -> Self::Output {
        if let Self::Paren(..) = other {
            other = other.eval()?
        }
        let precedence = Op::Plus.precedence();
        Ok(match self {
            Self::Function(..) | Self::ArgList(..) | Self::Map(..) => todo!(),
            Self::Important | Self::True | Self::False => match other {
                Self::Ident(s, QuoteKind::Double) | Self::Ident(s, QuoteKind::Single) => {
                    Value::Ident(format!("{}{}", self, s), QuoteKind::Double)
                }
                Self::Null => Value::Ident(self.to_string(), QuoteKind::None),
                _ => Value::Ident(format!("{}{}", self, other), QuoteKind::None),
            },
            Self::Null => match other {
                Self::Null => Self::Null,
                _ => Value::Ident(format!("{}", other), QuoteKind::None),
            },
            Self::Dimension(num, unit) => match other {
                Self::Dimension(num2, unit2) => {
                    if !unit.comparable(&unit2) {
                        return Err(format!("Incompatible units {} and {}.", unit2, unit).into());
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
                                * UNIT_CONVERSION_TABLE[&unit.to_string()][&unit2.to_string()]
                                    .clone(),
                            unit,
                        )
                    }
                }
                Self::Ident(s, q) => Value::Ident(format!("{}{}{}", num, unit, s), q.normalize()),
                Self::Null => Value::Ident(format!("{}{}", num, unit), QuoteKind::None),
                Self::List(..) => {
                    Value::Ident(format!("{}{}{}", num, unit, other), QuoteKind::None)
                }
                _ => {
                    return Err(
                        format!("Undefined operation \"{}{} + {}\".", num, unit, other).into(),
                    )
                }
            },
            Self::Color(c) => match other {
                Self::Ident(s, q) => Value::Ident(format!("{}{}", c, s), q.normalize()),
                Self::Null => Value::Ident(c.to_string(), QuoteKind::None),
                Self::List(..) => Value::Ident(format!("{}{}", c, other), QuoteKind::None),
                _ => return Err(format!("Undefined operation \"{} + {}\".", c, other).into()),
            },
            Self::BinaryOp(left, op, right) => {
                if op.precedence() >= precedence {
                    (Self::BinaryOp(left, op, right).eval()? + other)?
                } else {
                    Self::BinaryOp(
                        left,
                        op,
                        Box::new(Self::BinaryOp(right, Op::Plus, Box::new(other)).eval()?),
                    )
                    .eval()?
                }
            }
            Self::UnaryOp(..) | Self::Paren(..) => (self.eval()? + other)?,
            Self::Ident(s1, quotes1) => match other {
                Self::Ident(s2, _) => Value::Ident(format!("{}{}", s1, s2), quotes1.normalize()),
                Self::Important | Self::True | Self::False | Self::Dimension(..) => {
                    Value::Ident(format!("{}{}", s1, other), quotes1.normalize())
                }
                Self::Null => Value::Ident(s1, quotes1.normalize()),
                Self::Color(c) => Value::Ident(format!("{}{}", s1, c), quotes1.normalize()),
                Self::List(..) => Value::Ident(format!("{}{}", s1, other), quotes1),
                Self::UnaryOp(..) | Self::BinaryOp(..) => todo!(),
                Self::Paren(..) => (Self::Ident(s1, quotes1) + other.eval()?)?,
                Self::Function(..) | Self::ArgList(..) | Self::Map(..) => todo!(),
            },
            Self::List(..) => match other {
                Self::Ident(s, q) => Value::Ident(format!("{}{}", self, s), q.normalize()),
                Self::Paren(..) => (self + other.eval()?)?,
                _ => Value::Ident(format!("{}{}", self, other), QuoteKind::None),
            },
        })
    }
}

impl Sub for Value {
    type Output = SassResult<Self>;

    fn sub(self, mut other: Self) -> Self::Output {
        other = other.eval()?;
        Ok(match self {
            Self::Null => todo!(),
            Self::Dimension(num, unit) => match other {
                Self::Dimension(num2, unit2) => {
                    if !unit.comparable(&unit2) {
                        return Err(format!("Incompatible units {} and {}.", unit2, unit).into());
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
                                * UNIT_CONVERSION_TABLE[&unit.to_string()][&unit2.to_string()]
                                    .clone(),
                            unit,
                        )
                    }
                }
                Self::List(..) => {
                    Value::Ident(format!("{}{}-{}", num, unit, other), QuoteKind::None)
                }
                Self::Ident(..) => {
                    Value::Ident(format!("{}{}-{}", num, unit, other), QuoteKind::None)
                }
                _ => todo!(),
            },
            Self::Color(c) => match other {
                Self::Ident(s, q) => Value::Ident(
                    format!("{}-{}{}{}", c, q.normalize(), s, q.normalize()),
                    QuoteKind::None,
                ),
                Self::Null => Value::Ident(format!("{}-", c), QuoteKind::None),
                Self::Dimension(..) | Self::Color(..) => {
                    return Err(format!("Undefined operation \"{} - {}\".", c, other).into())
                }
                _ => Value::Ident(format!("{}-{}", c, other), QuoteKind::None),
            },
            Self::BinaryOp(..) | Self::Paren(..) => (self.eval()? - other)?,
            Self::Ident(s1, q1) => match other {
                Self::Ident(s2, q2) => Value::Ident(
                    format!(
                        "{}{}{}-{}{}{}",
                        q1.normalize(),
                        s1,
                        q1.normalize(),
                        q2.normalize(),
                        s2,
                        q2.normalize()
                    ),
                    QuoteKind::None,
                ),
                Self::Important
                | Self::True
                | Self::False
                | Self::Dimension(..)
                | Self::Color(..) => Value::Ident(
                    format!("{}{}{}-{}", q1.normalize(), s1, q1.normalize(), other),
                    QuoteKind::None,
                ),
                Self::Null => Value::Ident(
                    format!("{}{}{}-", q1.normalize(), s1, q1.normalize()),
                    QuoteKind::None,
                ),
                Self::List(..) => Value::Ident(
                    format!("{}{}{}-{}", q1.normalize(), s1, q1.normalize(), other),
                    QuoteKind::None,
                ),
                _ => todo!(),
            },
            Self::List(..) => match other {
                Self::Ident(s, q) => Value::Ident(
                    format!("{}-{}{}{}", self, q.normalize(), s, q.normalize()),
                    QuoteKind::None,
                ),
                Self::Paren(..) => (self - other.eval()?)?,
                _ => Value::Ident(format!("{}-{}", self, other), QuoteKind::None),
            },
            _ => match other {
                Self::Ident(s, q) => Value::Ident(
                    format!("{}-{}{}{}", self, q.normalize(), s, q.normalize()),
                    QuoteKind::None,
                ),
                Self::Null => Value::Ident(format!("{}-", self), QuoteKind::None),
                _ => Value::Ident(format!("{}-{}", self, other), QuoteKind::None),
            },
        })
    }
}

impl Mul for Value {
    type Output = SassResult<Self>;

    fn mul(self, mut other: Self) -> Self::Output {
        other = other.eval()?;
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
                    return Err(
                        format!("Undefined operation \"{}{} * {}\".", num, unit, other).into(),
                    )
                }
            },
            Self::BinaryOp(..) | Self::Paren(..) => (self.eval()? * other)?,
            Self::UnaryOp(..) => (self.eval()? * other)?,
            _ => return Err(format!("Undefined operation \"{} * {}\".", self, other).into()),
        })
    }
}

impl Div for Value {
    type Output = SassResult<Self>;

    fn div(self, other: Self) -> Self::Output {
        let precedence = Op::Div.precedence();
        Ok(match self {
            Self::Null => todo!(),
            Self::Dimension(num, unit) => match other {
                Self::Dimension(num2, unit2) => {
                    if !unit.comparable(&unit2) {
                        return Err(format!("Incompatible units {} and {}.", unit2, unit).into());
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
                                * UNIT_CONVERSION_TABLE[&unit.to_string()][&unit2.to_string()]
                                    .clone()),
                            Unit::None,
                        )
                    }
                }
                Self::Ident(s, q) => Value::Ident(
                    format!("{}{}/{}{}{}", num, unit, q.normalize(), s, q.normalize()),
                    QuoteKind::None,
                ),
                Self::BinaryOp(..) | Self::Paren(..) => {
                    (Self::Dimension(num, unit) / other.eval()?)?
                }
                _ => todo!(),
            },
            Self::Color(c) => match other {
                Self::Ident(s, q) => Value::Ident(
                    format!("{}/{}{}{}", c, q.normalize(), s, q.normalize()),
                    QuoteKind::None,
                ),
                Self::Null => Value::Ident(format!("{}/", c), QuoteKind::None),
                Self::Dimension(..) | Self::Color(..) => {
                    return Err(format!("Undefined operation \"{} / {}\".", c, other).into())
                }
                _ => Value::Ident(format!("{}/{}", c, other), QuoteKind::None),
            },
            Self::BinaryOp(left, op, right) => {
                if op.precedence() >= precedence {
                    (Self::BinaryOp(left, op, right).eval()? / other)?
                } else {
                    Self::BinaryOp(
                        left,
                        op,
                        Box::new(Self::BinaryOp(right, Op::Div, Box::new(other)).eval()?),
                    )
                    .eval()?
                }
            }
            Self::Paren(..) => (self.eval()? / other)?,
            Self::Ident(s1, q1) => match other {
                Self::Ident(s2, q2) => Value::Ident(
                    format!(
                        "{}{}{}/{}{}{}",
                        q1.normalize(),
                        s1,
                        q1.normalize(),
                        q2.normalize(),
                        s2,
                        q2.normalize()
                    ),
                    QuoteKind::None,
                ),
                Self::Important
                | Self::True
                | Self::False
                | Self::Dimension(..)
                | Self::Color(..) => Value::Ident(
                    format!("{}{}{}/{}", q1.normalize(), s1, q1.normalize(), other),
                    QuoteKind::None,
                ),
                Self::Null => Value::Ident(
                    format!("{}{}{}/", q1.normalize(), s1, q1.normalize()),
                    QuoteKind::None,
                ),
                _ => todo!(),
            },
            _ => match other {
                Self::Ident(s, q) => Value::Ident(
                    format!("{}/{}{}{}", self, q.normalize(), s, q.normalize()),
                    QuoteKind::None,
                ),
                Self::Null => Value::Ident(format!("{}/", self), QuoteKind::None),
                _ => Value::Ident(format!("{}/{}", self, other), QuoteKind::None),
            },
        })
    }
}

impl Rem for Value {
    type Output = SassResult<Self>;

    fn rem(self, other: Self) -> Self::Output {
        Ok(match self {
            Value::Dimension(n, u) => match other {
                Value::Dimension(n2, u2) => {
                    if !u.comparable(&u2) {
                        return Err(format!("Incompatible units {} and {}.", u2, u).into());
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
                    return Err(format!(
                        "Undefined operation \"{} % {}\".",
                        Value::Dimension(n, u),
                        other
                    )
                    .into())
                }
            },
            _ => return Err(format!("Undefined operation \"{} % {}\".", self, other).into()),
        })
    }
}

impl Neg for Value {
    type Output = SassResult<Self>;

    fn neg(self) -> Self::Output {
        Ok(match self.eval()? {
            Value::Dimension(n, u) => Value::Dimension(-n, u),
            v => Value::Ident(format!("-{}", v), QuoteKind::None),
        })
    }
}
