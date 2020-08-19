use std::{
    fmt::{self, Display, UpperHex},
    mem,
    ops::{Add, AddAssign},
};

use num_bigint::BigInt;
use num_traits::{Signed, ToPrimitive, Zero};

pub(crate) enum Integer {
    Small(i64),
    Big(BigInt),
}

impl Integer {
    pub fn abs(&self) -> Self {
        match self {
            Self::Small(v) => Self::Small(v.abs()),
            Self::Big(v) => Self::Big(v.abs()),
        }
    }

    pub fn is_ten(&self) -> bool {
        match self {
            Self::Small(10) => true,
            Self::Small(..) => false,
            Self::Big(v) => v == &BigInt::from(10),
        }
    }
}

impl Default for Integer {
    fn default() -> Self {
        Self::zero()
    }
}

impl UpperHex for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Small(v) => write!(f, "{:02X}", v),
            Self::Big(v) => write!(f, "{:02X}", v),
        }
    }
}

impl Add for Integer {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        match self {
            Self::Small(val1) => match rhs {
                Self::Small(val2) => match val1.checked_add(val2) {
                    Some(v) => Self::Small(v),
                    None => Self::Big(BigInt::from(val1) + val2),
                },
                Self::Big(val2) => Self::Big(BigInt::from(val1) + val2),
            },
            Self::Big(val1) => match rhs {
                Self::Big(val2) => Self::Big(val1 + val2),
                Self::Small(val2) => Self::Big(val1 + BigInt::from(val2)),
            },
        }
    }
}

impl Add<BigInt> for Integer {
    type Output = Self;
    fn add(self, rhs: BigInt) -> Self::Output {
        match self {
            Self::Small(v) => Self::Big(BigInt::from(v) + rhs),
            Self::Big(v) => Self::Big(v + rhs),
        }
    }
}

impl Add<i32> for Integer {
    type Output = Self;
    fn add(self, rhs: i32) -> Self::Output {
        match self {
            Self::Small(v) => match v.checked_add(i64::from(rhs)) {
                Some(v) => Self::Small(v),
                None => Self::Big(BigInt::from(v) + rhs),
            },
            Self::Big(v) => Self::Big(v + rhs),
        }
    }
}

impl AddAssign<i32> for Integer {
    fn add_assign(&mut self, rhs: i32) {
        let tmp = mem::take(self);
        *self = tmp + rhs;
    }
}

impl AddAssign for Integer {
    fn add_assign(&mut self, rhs: Self) {
        let tmp = mem::take(self);
        *self = tmp + rhs;
    }
}

impl Zero for Integer {
    fn zero() -> Self {
        Self::Small(0)
    }

    fn is_zero(&self) -> bool {
        match self {
            Self::Small(0) => true,
            Self::Small(..) => false,
            Self::Big(v) => v.is_zero(),
        }
    }
}

impl Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Small(v) => write!(f, "{}", v),
            Self::Big(v) => write!(f, "{}", v),
        }
    }
}

impl ToPrimitive for Integer {
    fn to_u8(&self) -> Option<u8> {
        match self {
            Self::Small(v) => v.to_u8(),
            Self::Big(v) => v.to_u8(),
        }
    }

    fn to_u64(&self) -> Option<u64> {
        match self {
            Self::Small(v) => v.to_u64(),
            Self::Big(v) => v.to_u64(),
        }
    }

    fn to_i64(&self) -> std::option::Option<i64> {
        match self {
            Self::Small(v) => Some(*v),
            Self::Big(v) => v.to_i64(),
        }
    }
}
