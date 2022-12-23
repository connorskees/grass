use std::ops::{Add, Div, Mul, Sub};

use codemap::Span;

use crate::{
    error::SassResult,
    serializer::inspect_number,
    unit::{known_compatibilities_by_unit, Unit, UNIT_CONVERSION_TABLE},
    Options,
};

use super::Number;

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
    pub fn assert_no_units(&self, name: &'static str, span: Span) -> SassResult<()> {
        if self.unit == Unit::None {
            Ok(())
        } else {
            Err((
                format!(
                    "${name}: Expected {} to have no units.",
                    inspect_number(self, &Options::default(), span)?
                ),
                span,
            )
                .into())
        }
    }

    pub fn is_comparable_to(&self, other: &Self) -> bool {
        self.unit.comparable(&other.unit)
    }

    /// For use in calculations
    pub fn has_possibly_compatible_units(&self, other: &Self) -> bool {
        if self.unit.is_complex() || other.unit.is_complex() {
            return false;
        }

        let known_compatibilities = match known_compatibilities_by_unit(&self.unit) {
            Some(known_compatibilities) => known_compatibilities,
            None => return true,
        };

        known_compatibilities.contains(&other.unit)
            || known_compatibilities_by_unit(&other.unit).is_none()
    }

    pub fn num(&self) -> Number {
        Number(self.num)
    }

    pub fn unit(&self) -> &Unit {
        &self.unit
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
