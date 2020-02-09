use std::fmt::{self, Display};

use crate::value::Number;
pub(crate) use name::ColorName;

mod name;

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct Color {
    red: u16,
    green: u16,
    blue: u16,
    alpha: Number,
    repr: String,
}

impl Color {
    pub fn new(red: u16, green: u16, blue: u16, alpha: u16, repr: String) -> Self {
        Color {
            red,
            green,
            blue,
            alpha: alpha.into(),
            repr,
        }
    }

    pub const fn red(&self) -> u16 {
        self.red
    }

    pub const fn blue(&self) -> u16 {
        self.blue
    }

    pub const fn green(&self) -> u16 {
        self.green
    }

    pub fn alpha(&self) -> Number {
        self.alpha.clone()
    }

    pub fn from_values(red: u16, green: u16, blue: u16, alpha: Number) -> Self {
        let repr = if alpha >= Number::from(1) {
            format!("#{:0>2x}{:0>2x}{:0>2x}", red, green, blue)
        } else {
            format!("rgba({}, {}, {}, {})", red, green, blue, alpha)
        };
        Color {
            red,
            green,
            blue,
            alpha,
            repr,
        }
    }
}

impl Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.repr)
    }
}
