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

    pub fn from_values(red: u16, green: u16, blue: u16, alpha: u16) -> Self {
        let repr = if alpha >= 1 {
            format!("#{:X}{:X}{:X}", red, green, blue)
        } else {
            format!("#{:X}{:X}{:X}{:X}", red, green, blue, alpha)
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
