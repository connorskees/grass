use std::ops::{Add, Div, Mul, Rem, Sub};

use crate::common::QuoteKind;
use crate::error::SassResult;
use crate::units::{Unit, UNIT_CONVERSION_TABLE};
use crate::value::Value;

impl Add for Value {
    type Output = SassResult<Self>;

    fn add(self, mut other: Self) -> Self::Output {
        other = other.eval()?;
        Ok(match self {
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
                Self::Ident(s, q) => {
                    let quotes = match q {
                        QuoteKind::Double | QuoteKind::Single => QuoteKind::Double,
                        QuoteKind::None => QuoteKind::None,
                    };
                    Value::Ident(format!("{}{}{}", num, unit, s), quotes)
                }
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
                Self::Ident(s, QuoteKind::Double) | Self::Ident(s, QuoteKind::Single) => {
                    Value::Ident(format!("{}{}", c, s), QuoteKind::Double)
                }
                Self::Ident(s, QuoteKind::None) => {
                    Value::Ident(format!("{}{}", c, s), QuoteKind::None)
                }
                Self::Null => Value::Ident(c.to_string(), QuoteKind::None),
                Self::List(..) => Value::Ident(format!("{}{}", c, other), QuoteKind::None),
                _ => return Err(format!("Undefined operation \"{} + {}\".", c, other).into()),
            },
            Self::BinaryOp(..) | Self::Paren(..) => (self.eval()? + other)?,
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
                Self::Color(c) => {
                    let quotes = match quotes1 {
                        QuoteKind::Double | QuoteKind::Single => QuoteKind::Double,
                        QuoteKind::None => QuoteKind::None,
                    };
                    Value::Ident(format!("{}{}", s1, c), quotes)
                }
                Self::List(..) => Value::Ident(format!("{}{}", s1, other), quotes1),
                Self::BinaryOp(..) | Self::Paren(..) => todo!(),
            },
            Self::List(..) => match other {
                Self::Ident(s, q) => {
                    let quotes = match q {
                        QuoteKind::Double | QuoteKind::Single => QuoteKind::Double,
                        QuoteKind::None => QuoteKind::None,
                    };
                    Value::Ident(format!("{}{}", self, s), quotes)
                }
                Self::Paren(..) => (self + other.eval()?)?,
                _ => Value::Ident(format!("{}{}", self, other), QuoteKind::None),
            }
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
                _ => todo!(),
            },
            Self::Color(c) => match other {
                Self::Ident(s, quotes) => {
                    let quotes = match quotes {
                        QuoteKind::Double | QuoteKind::Single => QuoteKind::Double,
                        QuoteKind::None => QuoteKind::None,
                    };
                    Value::Ident(format!("{}-{}{}{}", c, quotes, s, quotes), QuoteKind::None)
                }
                Self::Null => Value::Ident(format!("{}-", c), QuoteKind::None),
                Self::Dimension(..) | Self::Color(..) => {
                    return Err(format!("Undefined operation \"{} - {}\".", c, other).into())
                }
                _ => Value::Ident(format!("{}-{}", c, other), QuoteKind::None),
            },
            Self::BinaryOp(..) | Self::Paren(..) => (self.eval()? - other)?,
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
                Self::Important
                | Self::True
                | Self::False
                | Self::Dimension(..)
                | Self::Color(..) => {
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
                Self::List(..) => {
                    let quotes = match quotes1 {
                        QuoteKind::Double | QuoteKind::Single => QuoteKind::Double,
                        QuoteKind::None => QuoteKind::None,
                    };
                    Value::Ident(format!("{}{}{}-{}", quotes, s1, quotes, other), QuoteKind::None)
                }
                _ => todo!(),
            },
            Self::List(..) => match other {
                Self::Ident(s, q) => {
                    let quotes = match q {
                        QuoteKind::Double | QuoteKind::Single => QuoteKind::Double,
                        QuoteKind::None => QuoteKind::None,
                    };
                    Value::Ident(format!("{}-{}{}{}", self, quotes, s, quotes), QuoteKind::None)
                }
                Self::Paren(..) => (self + other.eval()?)?,
                _ => Value::Ident(format!("{}-{}", self, other), QuoteKind::None),
            }
            _ => match other {
                Self::Ident(s, quotes) => {
                    let quotes = match quotes {
                        QuoteKind::Double | QuoteKind::Single => QuoteKind::Double,
                        QuoteKind::None => QuoteKind::None,
                    };
                    Value::Ident(
                        format!("{}-{}{}{}", self, quotes, s, quotes),
                        QuoteKind::None,
                    )
                }
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
            Self::BinaryOp(..) | Self::Paren(..) => self.eval()?,
            _ => return Err(format!("Undefined operation \"{} * {}\".", self, other).into()),
        })
    }
}

impl Div for Value {
    type Output = SassResult<Self>;

    fn div(self, other: Self) -> Self::Output {
        Ok(match self {
            Self::Null => todo!(),
            Self::Dimension(num, unit) => match other {
                Self::Dimension(num2, unit2) => {
                    if unit == unit2 {
                        Value::Dimension(num / num2, Unit::None)
                    } else {
                        todo!("unit conversions")
                    }
                }
                Self::Ident(s, q) => {
                    let quotes = match q {
                        QuoteKind::Double | QuoteKind::Single => QuoteKind::Double,
                        QuoteKind::None => QuoteKind::None,
                    };
                    Value::Ident(
                        format!("{}{}/{}{}{}", num, unit, quotes, s, quotes),
                        QuoteKind::None,
                    )
                }
                Self::BinaryOp(..) | Self::Paren(..) => {
                    (Self::Dimension(num, unit) / other.eval()?)?
                }
                _ => todo!(),
            },
            Self::Color(c) => match other {
                Self::Ident(s, quotes) => {
                    let quotes = match quotes {
                        QuoteKind::Double | QuoteKind::Single => QuoteKind::Double,
                        QuoteKind::None => QuoteKind::None,
                    };
                    Value::Ident(format!("{}/{}{}{}", c, quotes, s, quotes), QuoteKind::None)
                }
                Self::Null => Value::Ident(format!("{}/", c), QuoteKind::None),
                Self::Dimension(..) | Self::Color(..) => {
                    return Err(format!("Undefined operation \"{} / {}\".", c, other).into())
                }
                _ => Value::Ident(format!("{}/{}", c, other), QuoteKind::None),
            },
            Self::BinaryOp(..) | Self::Paren(..) => self.eval()?,
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
                        format!("{}{}{}/{}{}{}", quotes1, s1, quotes1, quotes2, s2, quotes2),
                        QuoteKind::None,
                    )
                }
                Self::Important
                | Self::True
                | Self::False
                | Self::Dimension(..)
                | Self::Color(..) => {
                    let quotes = match quotes1 {
                        QuoteKind::Double | QuoteKind::Single => QuoteKind::Double,
                        QuoteKind::None => QuoteKind::None,
                    };
                    Value::Ident(
                        format!("{}{}{}/{}", quotes, s1, quotes, other),
                        QuoteKind::None,
                    )
                }
                Self::Null => {
                    let quotes = match quotes1 {
                        QuoteKind::Double | QuoteKind::Single => QuoteKind::Double,
                        QuoteKind::None => QuoteKind::None,
                    };
                    Value::Ident(format!("{}{}{}/", quotes, s1, quotes), QuoteKind::None)
                }
                _ => todo!(),
            },
            _ => match other {
                Self::Ident(s, quotes) => {
                    let quotes = match quotes {
                        QuoteKind::Double | QuoteKind::Single => QuoteKind::Double,
                        QuoteKind::None => QuoteKind::None,
                    };
                    Value::Ident(
                        format!("{}/{}{}{}", self, quotes, s, quotes),
                        QuoteKind::None,
                    )
                }
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
