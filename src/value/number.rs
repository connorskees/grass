use std::convert::From;
use std::fmt::{self, Display, Write};
use std::ops::{
    Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Rem, RemAssign, Sub, SubAssign,
};

use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::{Num, One, Signed, Zero};

use crate::error::SassError;

const PRECISION: usize = 10;

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd)]
pub(crate) struct Number {
    val: BigRational,
}

impl Number {
    pub const fn new(val: BigRational) -> Number {
        Number { val }
    }

    pub fn to_integer(&self) -> BigInt {
        self.val.to_integer()
    }

    pub fn ratio<A: Into<BigInt>, B: Into<BigInt>>(a: A, b: B) -> Self {
        Number::new(BigRational::new(a.into(), b.into()))
    }

    pub fn round(&self) -> Self {
        Number {
            val: self.val.round(),
        }
    }

    pub fn ceil(&self) -> Self {
        Number {
            val: self.val.ceil(),
        }
    }

    pub fn floor(&self) -> Self {
        Number {
            val: self.val.floor(),
        }
    }

    pub fn abs(&self) -> Self {
        Number {
            val: self.val.abs(),
        }
    }

    pub fn is_decimal(&self) -> bool {
        self.val.denom() != &BigInt::from(1)
    }

    pub fn clamp<A: Into<Number>, B: Into<Number>>(self, min: A, max: B) -> Self {
        let max = max.into();
        if self > max {
            return max;
        }

        let min = min.into();
        if self < min {
            return min;
        }

        self
    }
}

impl Zero for Number {
    fn zero() -> Self {
        Number::from(0)
    }

    fn is_zero(&self) -> bool {
        self.val.is_zero()
    }
}

impl One for Number {
    fn one() -> Self {
        Number::from(1)
    }

    fn is_one(&self) -> bool {
        self.val.is_one()
    }
}

impl Num for Number {
    type FromStrRadixErr = SassError;
    fn from_str_radix(_str: &str, _radix: u32) -> Result<Self, Self::FromStrRadixErr> {
        todo!()
    }
}

impl Signed for Number {
    fn abs(&self) -> Self {
        self.abs()
    }

    fn abs_sub(&self, other: &Self) -> Self {
        Number {
            val: self.val.abs_sub(&other.val),
        }
    }

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
        self.val.is_positive()
    }

    fn is_negative(&self) -> bool {
        self.val.is_negative()
    }
}

impl fmt::LowerHex for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:0>2x}", self.val.to_integer())
    }
}

impl From<BigInt> for Number {
    fn from(b: BigInt) -> Self {
        Number {
            val: BigRational::from_integer(b),
        }
    }
}

impl From<Number> for BigInt {
    #[inline]
    fn from(b: Number) -> Self {
        b.to_integer()
    }
}

macro_rules! from_integer {
    ($ty:ty) => {
        impl From<$ty> for Number {
            fn from(b: $ty) -> Self {
                Number {
                    val: BigRational::from_integer(BigInt::from(b)),
                }
            }
        }
    };
}

// todo: implement std::convertTryFrom instead
impl From<f64> for Number {
    fn from(b: f64) -> Self {
        Number {
            val: BigRational::from_float(b).unwrap(),
        }
    }
}

from_integer!(u16);
from_integer!(usize);
from_integer!(i32);
from_integer!(u32);
from_integer!(u8);

impl fmt::Debug for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Number {{ {} }}", self)
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut whole = self.val.to_integer().abs();
        let mut dec = String::new();

        let mut frac = self.val.fract();
        if frac != BigRational::from_integer(BigInt::from(0)) {
            dec.write_char('.')?;
            for _ in 0..(PRECISION - 1) {
                frac *= BigRational::from_integer(BigInt::from(10));
                write!(dec, "{}", frac.to_integer().abs())?;
                frac = frac.fract();
                if frac == BigRational::from_integer(BigInt::from(0)) {
                    break;
                }
            }
            if frac != BigRational::from_integer(BigInt::from(0)) {
                let end = (frac * BigRational::from_integer(BigInt::from(10)))
                    .round()
                    .abs()
                    .to_integer();
                if end == BigInt::from(10) {
                    loop {
                        match dec.pop().unwrap() {
                            '9' => continue,
                            '.' => {
                                whole += 1;
                                break;
                            }
                            c => {
                                dec.push_str(&(c.to_digit(10).unwrap() + 1).to_string());
                                break;
                            }
                        }
                    }
                } else if end == BigInt::from(0) {
                    loop {
                        match dec.pop().unwrap() {
                            '0' => continue,
                            '.' => break,
                            c => {
                                dec.push(c);
                                break;
                            }
                        }
                    }
                } else {
                    write!(dec, "{}", end)?;
                }
            }
        }

        if self.val.is_negative() && (!whole.is_zero() || !dec.is_empty()) {
            f.write_char('-')?;
        }
        write!(f, "{}", whole)?;
        write!(f, "{}", dec)?;
        Ok(())
    }
}

impl Add for Number {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Number {
            val: self.val + other.val,
        }
    }
}

impl AddAssign for Number {
    fn add_assign(&mut self, other: Self) {
        self.val += other.val
    }
}

impl Sub for Number {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Number {
            val: self.val - other.val,
        }
    }
}

impl SubAssign for Number {
    fn sub_assign(&mut self, other: Self) {
        self.val -= other.val
    }
}

impl Mul for Number {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        Number {
            val: self.val * other.val,
        }
    }
}

impl MulAssign for Number {
    fn mul_assign(&mut self, other: Self) {
        self.val *= other.val
    }
}

impl Div for Number {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        Number {
            val: self.val / other.val,
        }
    }
}

impl DivAssign for Number {
    fn div_assign(&mut self, other: Self) {
        self.val /= other.val
    }
}

impl Rem for Number {
    type Output = Self;

    fn rem(self, other: Self) -> Self {
        Number {
            val: self.val % other.val,
        }
    }
}

impl RemAssign for Number {
    fn rem_assign(&mut self, other: Self) {
        self.val %= other.val
    }
}

impl Neg for Number {
    type Output = Self;

    fn neg(self) -> Self {
        Number { val: -self.val }
    }
}
