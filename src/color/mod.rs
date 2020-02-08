use std::fmt::{self, Display};

pub(crate) use name::ColorName;

mod name;

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct Color {
    red: u16,
    green: u16,
    blue: u16,
    alpha: u16,
    repr: String,
}

impl Color {
    pub const fn new(red: u16, green: u16, blue: u16, alpha: u16, repr: String) -> Self {
        Color {
            red,
            green,
            blue,
            alpha,
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

    pub const fn alpha(&self) -> u16 {
        self.alpha
    }

    pub fn from_values(red: u16, green: u16, blue: u16, alpha: u16) -> Self {
        let repr = if alpha >= 1 {
            format!("#{:0>2x}{:0>2x}{:0>2x}", red, green, blue)
        } else {
            format!("#{:0>2x}{:0>2x}{:0>2x}{:0>2x}", red, green, blue, alpha)
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
