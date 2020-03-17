use std::ops::{Add, Div, Mul, Sub};

use crate::common::QuoteKind;
use crate::error::SassResult;
use crate::units::Unit;
use crate::value::Value;

impl Add for Value {
    type Output = SassResult<Self>;

    fn add(self, other: Self) -> Self::Output {
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
                    } else {
                        todo!("unit conversions")
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
                _ => {
                    return Err(
                        format!("Undefined operation \"{}{} + {}\".", num, unit, other).into(),
                    )
                }
            },
            // Self::List(..) => todo!(),
            Self::Color(c) => match other {
                Self::Ident(s, QuoteKind::Double) | Self::Ident(s, QuoteKind::Single) => {
                    Value::Ident(format!("{}{}", c, s), QuoteKind::Double)
                }
                Self::Ident(s, QuoteKind::None) => {
                    Value::Ident(format!("{}{}", c, s), QuoteKind::None)
                }
                Self::Null => Value::Ident(c.to_string(), QuoteKind::None),
                _ => return Err(format!("Undefined operation \"{} + {}\".", c, other).into()),
            },
            Self::BinaryOp(..) | Self::Paren(..) => self.eval()?,
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
                Self::Color(c) => {
                    let quotes = match quotes1 {
                        QuoteKind::Double | QuoteKind::Single => QuoteKind::Double,
                        QuoteKind::None => QuoteKind::None,
                    };
                    Value::Ident(format!("{}{}", s1, c), quotes)
                }
                Self::BinaryOp(..) | Self::Paren(..) => {
                    return Self::Ident(s1, quotes1) + other.eval()?
                }
                Self::List(..) => todo!(),
            },
            _ => todo!(),
        })
    }
}

impl Sub for Value {
    type Output = SassResult<Self>;

    fn sub(self, other: Self) -> Self::Output {
        Ok(match self {
            Self::Null => todo!(),
            Self::Dimension(num, unit) => match other {
                Self::Dimension(num2, unit2) => {
                    if !unit.comparable(&unit2) {
                        return Err(format!("Incompatible units {} and {}.", unit2, unit).into());
                    }
                    if unit == unit2 {
                        Value::Dimension(num - num2, unit)
                    } else {
                        todo!("unit conversions")
                    }
                }
                _ => todo!(),
            },
            // Self::List(..) => todo!(),
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
                _ => todo!(),
            },
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

    fn mul(self, other: Self) -> Self::Output {
        Ok(match self {
            Self::Null => todo!(),
            Self::Dimension(num, unit) => match other {
                Self::Dimension(num2, unit2) => {
                    if unit == Unit::None {
                        Value::Dimension(num * num2, unit2)
                    } else if unit2 == Unit::None {
                        Value::Dimension(num * num2, unit)
                    } else {
                        Value::Dimension(num * num2, Unit::Mul(Box::new(unit), Box::new(unit2)))
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
            // Self::List(..) => todo!(),
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
