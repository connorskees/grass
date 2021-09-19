use std::{
    cmp::Ordering,
    convert::{From, TryFrom},
    fmt, mem,
    ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Rem, RemAssign, Sub, SubAssign},
};

use num_bigint::BigInt;
use num_rational::{BigRational, Rational64};
use num_traits::{
    CheckedAdd, CheckedDiv, CheckedMul, CheckedSub, Num, One, Signed, ToPrimitive, Zero,
};

use crate::unit::{Unit, UNIT_CONVERSION_TABLE};

use integer::Integer;

mod integer;

const PRECISION: usize = 10;

#[derive(Clone)]
pub(crate) enum Number {
    Small(Rational64),
    Big(Box<BigRational>),
}

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Number::Small(val1), Number::Small(val2)) => val1 == val2,
            (Number::Big(val1), val2 @ Number::Small(..)) => {
                **val1 == val2.clone().into_big_rational()
            }
            (val1 @ Number::Small(..), Number::Big(val2)) => {
                val1.clone().into_big_rational() == **val2
            }
            (Number::Big(val1), Number::Big(val2)) => val1 == val2,
        }
    }
}

impl Eq for Number {}

impl Number {
    pub const fn new_small(val: Rational64) -> Number {
        Number::Small(val)
    }

    pub fn new_big(val: BigRational) -> Number {
        Number::Big(Box::new(val))
    }

    fn into_big_rational(self) -> BigRational {
        match self {
            Number::Small(small) => {
                let tuple: (i64, i64) = small.into();

                BigRational::new_raw(BigInt::from(tuple.0), BigInt::from(tuple.1))
            }
            Number::Big(big) => *big,
        }
    }

    pub fn to_integer(&self) -> Integer {
        match self {
            Self::Small(val) => Integer::Small(val.to_integer()),
            Self::Big(val) => Integer::Big(val.to_integer()),
        }
    }

    pub fn small_ratio<A: Into<i64>, B: Into<i64>>(a: A, b: B) -> Self {
        Number::new_small(Rational64::new(a.into(), b.into()))
    }

    #[allow(dead_code)]
    pub fn big_ratio<A: Into<BigInt>, B: Into<BigInt>>(a: A, b: B) -> Self {
        Number::new_big(BigRational::new(a.into(), b.into()))
    }

    pub fn round(&self) -> Self {
        match self {
            Self::Small(val) => Self::Small(val.round()),
            Self::Big(val) => Self::Big(Box::new(val.round())),
        }
    }

    pub fn ceil(&self) -> Self {
        match self {
            Self::Small(val) => Self::Small(val.ceil()),
            Self::Big(val) => Self::Big(Box::new(val.ceil())),
        }
    }

    pub fn floor(&self) -> Self {
        match self {
            Self::Small(val) => Self::Small(val.floor()),
            Self::Big(val) => Self::Big(Box::new(val.floor())),
        }
    }

    pub fn abs(&self) -> Self {
        match self {
            Self::Small(val) => Self::Small(val.abs()),
            Self::Big(val) => Self::Big(Box::new(val.abs())),
        }
    }

    pub fn is_decimal(&self) -> bool {
        match self {
            Self::Small(v) => !v.is_integer(),
            Self::Big(v) => !v.is_integer(),
        }
    }

    pub fn fract(&mut self) -> Number {
        match self {
            Self::Small(v) => Number::new_small(v.fract()),
            Self::Big(v) => Number::new_big(v.fract()),
        }
    }

    pub fn clamp<A: Into<Number> + Zero, B: Into<Number>>(self, min: A, max: B) -> Self {
        let max = max.into();
        if self > max {
            return max;
        }

        if min.is_zero() && self.is_negative() {
            return Number::zero();
        }

        let min = min.into();
        if self < min {
            return min;
        }

        self
    }

    #[allow(clippy::cast_precision_loss)]
    pub fn as_float(self) -> Option<f64> {
        Some(match self {
            Number::Small(n) => ((*n.numer() as f64) / (*n.denom() as f64)),
            Number::Big(n) => ((n.numer().to_f64()?) / (n.denom().to_f64()?)),
        })
    }

    pub fn sqrt(self) -> Option<Self> {
        Some(Number::Big(Box::new(BigRational::from_float(
            self.as_float()?.sqrt(),
        )?)))
    }

    pub fn ln(self) -> Option<Self> {
        Some(Number::Big(Box::new(BigRational::from_float(
            self.as_float()?.ln(),
        )?)))
    }

    pub fn log(self, base: Number) -> Option<Self> {
        Some(Number::Big(Box::new(BigRational::from_float(
            self.as_float()?.log(base.as_float()?),
        )?)))
    }

    pub fn pow(self, exponent: Self) -> Option<Self> {
        Some(Number::Big(Box::new(BigRational::from_float(
            self.as_float()?.powf(exponent.as_float()?),
        )?)))
    }

    pub fn pi() -> Self {
        Number::from(std::f64::consts::PI)
    }

    pub fn atan2(self, other: Self) -> Option<Self> {
        Some(Number::Big(Box::new(BigRational::from_float(
            self.as_float()?.atan2(other.as_float()?),
        )?)))
    }

    /// Invariants: `from.comparable(&to)` must be true
    pub fn convert(self, from: &Unit, to: &Unit) -> Self {
        debug_assert!(from.comparable(to));
        self * UNIT_CONVERSION_TABLE[to][from].clone()
    }
}

macro_rules! trig_fn(
    ($name:ident, $name_deg:ident) => {
        pub fn $name(self) -> Option<Self> {
            Some(Number::Big(Box::new(BigRational::from_float(
                self.as_float()?.$name(),
            )?)))
        }

        pub fn $name_deg(self) -> Option<Self> {
            Some(Number::Big(Box::new(BigRational::from_float(
                self.as_float()?.to_radians().$name(),
            )?)))
        }
    }
);

macro_rules! inverse_trig_fn(
    ($name:ident) => {
        pub fn $name(self) -> Option<Self> {
            Some(Number::Big(Box::new(BigRational::from_float(
                self.as_float()?.$name().to_degrees(),
            )?)))
        }
    }
);

/// Trigonometry methods
impl Number {
    trig_fn!(cos, cos_deg);
    trig_fn!(sin, sin_deg);
    trig_fn!(tan, tan_deg);

    inverse_trig_fn!(acos);
    inverse_trig_fn!(asin);
    inverse_trig_fn!(atan);
}

impl Default for Number {
    fn default() -> Self {
        Self::zero()
    }
}

impl Zero for Number {
    fn zero() -> Self {
        Number::new_small(Rational64::from_integer(0))
    }

    fn is_zero(&self) -> bool {
        match self {
            Self::Small(v) => v.is_zero(),
            Self::Big(v) => v.is_zero(),
        }
    }
}

impl One for Number {
    fn one() -> Self {
        Number::new_small(Rational64::from_integer(1))
    }

    fn is_one(&self) -> bool {
        match self {
            Self::Small(v) => v.is_one(),
            Self::Big(v) => v.is_one(),
        }
    }
}

impl Num for Number {
    type FromStrRadixErr = ();
    #[cold]
    fn from_str_radix(_: &str, _: u32) -> Result<Self, Self::FromStrRadixErr> {
        unimplemented!()
    }
}

impl Signed for Number {
    fn abs(&self) -> Self {
        self.abs()
    }

    #[cold]
    fn abs_sub(&self, _: &Self) -> Self {
        unimplemented!()
    }

    #[cold]
    fn signum(&self) -> Self {
        if self.is_zero() {
            Self::zero()
        } else if self.is_positive() {
            Self::one()
        } else {
            -Self::one()
        }
    }

    fn is_positive(&self) -> bool {
        match self {
            Self::Small(v) => v.is_positive(),
            Self::Big(v) => v.is_positive(),
        }
    }

    fn is_negative(&self) -> bool {
        match self {
            Self::Small(v) => v.is_negative(),
            Self::Big(v) => v.is_negative(),
        }
    }
}

macro_rules! from_integer {
    ($ty:ty) => {
        impl From<$ty> for Number {
            fn from(b: $ty) -> Self {
                if let Ok(v) = i64::try_from(b) {
                    Number::Small(Rational64::from_integer(v))
                } else {
                    Number::Big(Box::new(BigRational::from_integer(BigInt::from(b))))
                }
            }
        }
    };
}

macro_rules! from_smaller_integer {
    ($ty:ty) => {
        impl From<$ty> for Number {
            fn from(val: $ty) -> Self {
                Number::new_small(Rational64::from_integer(val as i64))
            }
        }
    };
}

impl From<i64> for Number {
    fn from(val: i64) -> Self {
        Number::new_small(Rational64::from_integer(val))
    }
}

#[allow(clippy::fallible_impl_from)]
impl From<f64> for Number {
    fn from(b: f64) -> Self {
        Number::Big(Box::new(BigRational::from_float(b).unwrap()))
    }
}

from_integer!(usize);
from_integer!(isize);
from_smaller_integer!(i32);
from_smaller_integer!(u32);
from_smaller_integer!(u8);

impl fmt::Debug for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Small(..) => write!(f, "Number::Small( {} )", self.to_string(false)),
            Self::Big(..) => write!(f, "Number::Big( {} )", self.to_string(false)),
        }
    }
}

impl ToPrimitive for Number {
    fn to_u64(&self) -> Option<u64> {
        match self {
            Self::Small(n) => {
                if !n.denom().is_one() {
                    return None;
                }
                n.to_u64()
            }
            Self::Big(n) => {
                if !n.denom().is_one() {
                    return None;
                }
                n.to_u64()
            }
        }
    }

    fn to_i64(&self) -> Option<i64> {
        match self {
            Self::Small(n) => {
                if !n.denom().is_one() {
                    return None;
                }
                n.to_i64()
            }
            Self::Big(n) => {
                if !n.denom().is_one() {
                    return None;
                }
                n.to_i64()
            }
        }
    }
}

impl Number {
    pub(crate) fn inspect(&self) -> String {
        self.to_string(false)
    }

    pub(crate) fn to_string(&self, is_compressed: bool) -> String {
        let mut whole = self.to_integer().abs();
        let has_decimal = self.is_decimal();
        let mut frac = self.abs().fract();
        let mut dec = String::with_capacity(if has_decimal { PRECISION } else { 0 });

        let mut buf = String::new();

        if has_decimal {
            for _ in 0..(PRECISION - 1) {
                frac *= 10_i64;
                dec.push_str(&frac.to_integer().to_string());

                frac = frac.fract();
                if frac.is_zero() {
                    break;
                }
            }
            if !frac.is_zero() {
                let end = (frac * 10_i64).round().to_integer();
                if end.is_ten() {
                    loop {
                        match dec.pop() {
                            Some('9') => continue,
                            Some(c) => {
                                dec.push(char::from(c as u8 + 1));
                                break;
                            }
                            None => {
                                whole += 1;
                                break;
                            }
                        }
                    }
                } else if end.is_zero() {
                    loop {
                        match dec.pop() {
                            Some('0') => continue,
                            Some(c) => {
                                dec.push(c);
                                break;
                            }
                            None => break,
                        }
                    }
                } else {
                    dec.push_str(&end.to_string());
                }
            }
        }

        let has_decimal = !dec.is_empty();

        if self.is_negative() && (!whole.is_zero() || has_decimal) {
            buf.push('-');
        }

        // if the entire number is just zero, we always want to emit it
        if whole.is_zero() && !has_decimal {
            return "0".to_owned();

        // otherwise, if the number is not 0, or the number before the decimal
        // _is_ 0 and we aren't in compressed mode, emit the number before the
        // decimal
        } else if !(whole.is_zero() && is_compressed) {
            buf.push_str(&whole.to_string());
        }

        if has_decimal {
            buf.push('.');
            buf.push_str(&dec);
        }

        buf
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self {
            Self::Small(val1) => match other {
                Self::Small(val2) => val1.partial_cmp(val2),
                Self::Big(val2) => {
                    let tuple: (i64, i64) = (*val1).into();
                    BigRational::new_raw(BigInt::from(tuple.0), BigInt::from(tuple.1))
                        .partial_cmp(val2)
                }
            },
            Self::Big(val1) => match other {
                Self::Small(val2) => {
                    let tuple: (i64, i64) = (*val2).into();
                    (**val1).partial_cmp(&BigRational::new_raw(
                        BigInt::from(tuple.0),
                        BigInt::from(tuple.1),
                    ))
                }
                Self::Big(val2) => val1.partial_cmp(val2),
            },
        }
    }
}

impl Ord for Number {
    fn cmp(&self, other: &Self) -> Ordering {
        match self {
            Self::Small(val1) => match other {
                Self::Small(val2) => val1.cmp(val2),
                Self::Big(val2) => {
                    let tuple: (i64, i64) = (*val1).into();
                    BigRational::new_raw(BigInt::from(tuple.0), BigInt::from(tuple.1)).cmp(val2)
                }
            },
            Self::Big(val1) => match other {
                Self::Small(val2) => {
                    let tuple: (i64, i64) = (*val2).into();
                    (**val1).cmp(&BigRational::new_raw(
                        BigInt::from(tuple.0),
                        BigInt::from(tuple.1),
                    ))
                }
                Self::Big(val2) => val1.cmp(val2),
            },
        }
    }
}

impl Add for Number {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        match self {
            Self::Small(val1) => match other {
                Self::Small(val2) => match val1.checked_add(&val2) {
                    Some(v) => Self::Small(v),
                    None => {
                        let tuple1: (i64, i64) = val1.into();
                        let tuple2: (i64, i64) = val2.into();
                        Self::Big(Box::new(
                            BigRational::new_raw(BigInt::from(tuple1.0), BigInt::from(tuple1.1))
                                + BigRational::new_raw(
                                    BigInt::from(tuple2.0),
                                    BigInt::from(tuple2.1),
                                ),
                        ))
                    }
                },
                Self::Big(val2) => {
                    let tuple: (i64, i64) = val1.into();
                    Self::Big(Box::new(
                        BigRational::new_raw(BigInt::from(tuple.0), BigInt::from(tuple.1)) + *val2,
                    ))
                }
            },
            Self::Big(val1) => match other {
                Self::Big(val2) => Self::Big(Box::new(*val1 + *val2)),
                Self::Small(val2) => {
                    let tuple: (i64, i64) = val2.into();
                    Self::Big(Box::new(
                        (*val1)
                            + BigRational::new_raw(BigInt::from(tuple.0), BigInt::from(tuple.1)),
                    ))
                }
            },
        }
    }
}

impl Add<&Self> for Number {
    type Output = Self;

    fn add(self, other: &Self) -> Self {
        match self {
            Self::Small(val1) => match other {
                Self::Small(val2) => match val1.checked_add(val2) {
                    Some(v) => Self::Small(v),
                    None => {
                        let tuple1: (i64, i64) = val1.into();
                        let tuple2: (i64, i64) = (*val2).into();
                        Self::Big(Box::new(
                            BigRational::new_raw(BigInt::from(tuple1.0), BigInt::from(tuple1.1))
                                + BigRational::new_raw(
                                    BigInt::from(tuple2.0),
                                    BigInt::from(tuple2.1),
                                ),
                        ))
                    }
                },
                Self::Big(val2) => {
                    let tuple: (i64, i64) = val1.into();
                    Self::Big(Box::new(
                        BigRational::new_raw(BigInt::from(tuple.0), BigInt::from(tuple.1))
                            + *val2.clone(),
                    ))
                }
            },
            Self::Big(val1) => match other {
                Self::Big(val2) => Self::Big(Box::new(*val1 + *val2.clone())),
                Self::Small(val2) => {
                    let tuple: (i64, i64) = (*val2).into();
                    Self::Big(Box::new(
                        *val1 + BigRational::new_raw(BigInt::from(tuple.0), BigInt::from(tuple.1)),
                    ))
                }
            },
        }
    }
}

impl AddAssign for Number {
    fn add_assign(&mut self, other: Self) {
        let tmp = mem::take(self);
        *self = tmp + other;
    }
}

impl Sub for Number {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        match self {
            Self::Small(val1) => match other {
                Self::Small(val2) => match val1.checked_sub(&val2) {
                    Some(v) => Self::Small(v),
                    None => {
                        let tuple1: (i64, i64) = val1.into();
                        let tuple2: (i64, i64) = val2.into();
                        Self::Big(Box::new(
                            BigRational::new_raw(BigInt::from(tuple1.0), BigInt::from(tuple1.1))
                                - BigRational::new_raw(
                                    BigInt::from(tuple2.0),
                                    BigInt::from(tuple2.1),
                                ),
                        ))
                    }
                },
                Self::Big(val2) => {
                    let tuple: (i64, i64) = val1.into();
                    Self::Big(Box::new(
                        BigRational::new_raw(BigInt::from(tuple.0), BigInt::from(tuple.1)) - *val2,
                    ))
                }
            },
            Self::Big(val1) => match other {
                Self::Big(val2) => Self::Big(Box::new(*val1 - *val2)),
                Self::Small(val2) => {
                    let tuple: (i64, i64) = val2.into();
                    Self::Big(Box::new(
                        *val1 - BigRational::new_raw(BigInt::from(tuple.0), BigInt::from(tuple.1)),
                    ))
                }
            },
        }
    }
}

impl SubAssign for Number {
    fn sub_assign(&mut self, other: Self) {
        let tmp = mem::take(self);
        *self = tmp - other;
    }
}

impl Mul for Number {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        match self {
            Self::Small(val1) => match other {
                Self::Small(val2) => match val1.checked_mul(&val2) {
                    Some(v) => Self::Small(v),
                    None => {
                        let tuple1: (i64, i64) = val1.into();
                        let tuple2: (i64, i64) = val2.into();
                        Self::Big(Box::new(
                            BigRational::new_raw(BigInt::from(tuple1.0), BigInt::from(tuple1.1))
                                * BigRational::new_raw(
                                    BigInt::from(tuple2.0),
                                    BigInt::from(tuple2.1),
                                ),
                        ))
                    }
                },
                Self::Big(val2) => {
                    let tuple: (i64, i64) = val1.into();
                    Self::Big(Box::new(
                        BigRational::new_raw(BigInt::from(tuple.0), BigInt::from(tuple.1)) * *val2,
                    ))
                }
            },
            Self::Big(val1) => match other {
                Self::Big(val2) => Self::Big(Box::new(*val1 * *val2)),
                Self::Small(val2) => {
                    let tuple: (i64, i64) = val2.into();
                    Self::Big(Box::new(
                        *val1 * BigRational::new_raw(BigInt::from(tuple.0), BigInt::from(tuple.1)),
                    ))
                }
            },
        }
    }
}

impl Mul<i64> for Number {
    type Output = Self;

    fn mul(self, other: i64) -> Self {
        match self {
            Self::Small(val1) => Self::Small(val1 * other),
            Self::Big(val1) => Self::Big(Box::new(*val1 * BigInt::from(other))),
        }
    }
}

impl MulAssign<i64> for Number {
    fn mul_assign(&mut self, other: i64) {
        let tmp = mem::take(self);
        *self = tmp * other;
    }
}

impl MulAssign for Number {
    fn mul_assign(&mut self, other: Self) {
        let tmp = mem::take(self);
        *self = tmp * other;
    }
}

impl Div for Number {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        match self {
            Self::Small(val1) => match other {
                Self::Small(val2) => match val1.checked_div(&val2) {
                    Some(v) => Self::Small(v),
                    None => {
                        let tuple1: (i64, i64) = val1.into();
                        let tuple2: (i64, i64) = val2.into();
                        Self::Big(Box::new(
                            BigRational::new_raw(BigInt::from(tuple1.0), BigInt::from(tuple1.1))
                                / BigRational::new_raw(
                                    BigInt::from(tuple2.0),
                                    BigInt::from(tuple2.1),
                                ),
                        ))
                    }
                },
                Self::Big(val2) => {
                    let tuple: (i64, i64) = val1.into();
                    Self::Big(Box::new(
                        BigRational::new_raw(BigInt::from(tuple.0), BigInt::from(tuple.1)) / *val2,
                    ))
                }
            },
            Self::Big(val1) => match other {
                Self::Big(val2) => Self::Big(Box::new(*val1 / *val2)),
                Self::Small(val2) => {
                    let tuple: (i64, i64) = val2.into();
                    Self::Big(Box::new(
                        *val1 / BigRational::new_raw(BigInt::from(tuple.0), BigInt::from(tuple.1)),
                    ))
                }
            },
        }
    }
}

impl DivAssign for Number {
    fn div_assign(&mut self, other: Self) {
        let tmp = mem::take(self);
        *self = tmp / other;
    }
}

fn modulo(n1: BigRational, n2: BigRational) -> BigRational {
    (n1 % n2.clone() + n2.clone()) % n2
}

impl Rem for Number {
    type Output = Self;

    fn rem(self, other: Self) -> Self {
        match self {
            Self::Small(val1) => match other {
                Self::Small(val2) => {
                    let tuple1: (i64, i64) = val1.into();
                    let tuple2: (i64, i64) = val2.into();

                    Self::Big(Box::new(modulo(
                        BigRational::new_raw(BigInt::from(tuple1.0), BigInt::from(tuple1.1)),
                        BigRational::new_raw(BigInt::from(tuple2.0), BigInt::from(tuple2.1)),
                    )))
                }
                Self::Big(val2) => {
                    let tuple: (i64, i64) = val1.into();

                    Self::Big(Box::new(modulo(
                        BigRational::new_raw(BigInt::from(tuple.0), BigInt::from(tuple.1)),
                        *val2,
                    )))
                }
            },
            Self::Big(val1) => match other {
                Self::Big(val2) => Self::Big(Box::new(modulo(*val1, *val2))),
                Self::Small(val2) => {
                    let tuple: (i64, i64) = val2.into();
                    Self::Big(Box::new(modulo(
                        *val1,
                        BigRational::new_raw(BigInt::from(tuple.0), BigInt::from(tuple.1)),
                    )))
                }
            },
        }
    }
}

impl RemAssign for Number {
    fn rem_assign(&mut self, other: Self) {
        let tmp = mem::take(self);
        *self = tmp % other;
    }
}

impl Neg for Number {
    type Output = Self;

    fn neg(self) -> Self {
        match self {
            Self::Small(v) => Self::Small(-v),
            Self::Big(v) => Self::Big(Box::new(-*v)),
        }
    }
}
