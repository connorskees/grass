use std::ops::{Add, Sub};

use crate::common::QuoteKind;
use crate::value::Value;

impl Add for Value {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        match self {
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
