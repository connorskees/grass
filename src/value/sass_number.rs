use std::{
    borrow::Cow,
    cell::Cell,
    cmp::Ordering,
    collections::BTreeMap,
    ops::{Add, Div, Mul, Sub},
    sync::Arc,
};

use codemap::{Span, Spanned};

use crate::{
    color::Color,
    common::{BinaryOp, Brackets, Identifier, ListSeparator, QuoteKind},
    error::SassResult,
    evaluate::Visitor,
    selector::Selector,
    unit::{Unit, UNIT_CONVERSION_TABLE},
    utils::{hex_char_for, is_special_function},
};

use super::Number;

// num, unit, as_slash
// todo: is as_slash included in eq
#[derive(Debug, Clone)]
pub(crate) struct SassNumber {
    pub num: f64,
    pub unit: Unit,
    pub as_slash: Option<Box<(Self, Self)>>,
}

impl PartialEq for SassNumber {
    fn eq(&self, other: &Self) -> bool {
        self.num == other.num && self.unit == other.unit
    }
}

impl Add<SassNumber> for SassNumber {
    type Output = SassNumber;
    fn add(self, rhs: SassNumber) -> Self::Output {
        if self.unit == rhs.unit {
            SassNumber {
                num: self.num + rhs.num,
                unit: self.unit,
                as_slash: None,
            }
        } else if self.unit == Unit::None {
            SassNumber {
                num: self.num + rhs.num,
                unit: rhs.unit,
                as_slash: None,
            }
        } else if rhs.unit == Unit::None {
            SassNumber {
                num: self.num + rhs.num,
                unit: self.unit,
                as_slash: None,
            }
        } else {
            SassNumber {
                num: self.num + Number(rhs.num).convert(&rhs.unit, &self.unit).0,
                unit: self.unit,
                as_slash: None,
            }
        }
    }
}

impl Sub<SassNumber> for SassNumber {
    type Output = SassNumber;

    fn sub(self, rhs: SassNumber) -> Self::Output {
        if self.unit == rhs.unit {
            SassNumber {
                num: self.num - rhs.num,
                unit: self.unit,
                as_slash: None,
            }
        } else if self.unit == Unit::None {
            SassNumber {
                num: self.num - rhs.num,
                unit: rhs.unit,
                as_slash: None,
            }
        } else if rhs.unit == Unit::None {
            SassNumber {
                num: self.num - rhs.num,
                unit: self.unit,
                as_slash: None,
            }
        } else {
            SassNumber {
                num: self.num - Number(rhs.num).convert(&rhs.unit, &self.unit).0,
                unit: self.unit,
                as_slash: None,
            }
        }
    }
}

impl Mul<SassNumber> for SassNumber {
    type Output = SassNumber;
    fn mul(self, rhs: SassNumber) -> Self::Output {
        if self.unit == Unit::None {
            SassNumber {
                num: self.num * rhs.num,
                unit: rhs.unit,
                as_slash: None,
            }
        } else if rhs.unit == Unit::None {
            SassNumber {
                num: self.num * rhs.num,
                unit: self.unit,
                as_slash: None,
            }
        } else {
            SassNumber {
                num: self.num * rhs.num,
                unit: self.unit * rhs.unit,
                as_slash: None,
            }
        }
    }
}

impl Div<SassNumber> for SassNumber {
    type Output = SassNumber;
    fn div(self, rhs: SassNumber) -> Self::Output {
        // `unit(1em / 1em)` => `""`
        if self.unit == rhs.unit {
            SassNumber {
                num: self.num / rhs.num,
                unit: Unit::None,
                as_slash: None,
            }

        // `unit(1 / 1em)` => `"em^-1"`
        } else if self.unit == Unit::None {
            SassNumber {
                num: self.num / rhs.num,
                unit: Unit::None / rhs.unit,
                as_slash: None,
            }

        // `unit(1em / 1)` => `"em"`
        } else if rhs.unit == Unit::None {
            SassNumber {
                num: self.num / rhs.num,
                unit: self.unit,
                as_slash: None,
            }

        // `unit(1in / 1px)` => `""`
        } else if self.unit.comparable(&rhs.unit) {
            SassNumber {
                num: self.num / Number(rhs.num).convert(&rhs.unit, &self.unit).0,
                unit: Unit::None,
                as_slash: None,
            }
            // `unit(1em / 1px)` => `"em/px"`
        } else {
            SassNumber {
                num: self.num / rhs.num,
                unit: self.unit / rhs.unit,
                as_slash: None,
            }
        }
    }
}

impl Eq for SassNumber {}

impl SassNumber {
    pub fn is_comparable_to(&self, other: &Self) -> bool {
        self.unit.comparable(&other.unit)
    }

    pub fn num(&self) -> Number {
        Number(self.num)
    }

    pub fn unit(&self) -> &Unit {
        &self.unit
    }

    pub fn as_slash(&self) -> &Option<Box<(Self, Self)>> {
        &self.as_slash
    }

    /// Invariants: `from.comparable(&to)` must be true
    pub fn convert(mut self, to: &Unit) -> Self {
        let from = &self.unit;
        debug_assert!(from.comparable(to));

        if from == &Unit::None || to == &Unit::None {
            self.unit = self.unit * to.clone();
            return self;
        }

        self.num *= UNIT_CONVERSION_TABLE[to][from];
        self.unit = self.unit * to.clone();

        self
    }
}
