use std::fmt::{self, Display};

use name::ColorName;

mod name;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub(crate) struct Color {
    red: u16,
    green: u16,
    blue: u16,
    alpha: u16,
    repr: ColorRepr,
}

impl Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.repr {
            ColorRepr::Name(n) => write!(f, "{}", n),
            ColorRepr::Hex3 => todo!(), //write!(f, "#{}{}{}", self.red, self.green, self.blue),
            ColorRepr::Hex6 => write!(f, "#{:X}{:X}{:X}", self.red, self.green, self.blue),
            ColorRepr::Hex8 => write!(
                f,
                "#{:X}{:X}{:X}{:X}",
                self.red, self.green, self.blue, self.alpha
            ),
            ColorRepr::Function if self.alpha < 1 => write!(
                f,
                "rgba({}, {}, {}, {})",
                self.red, self.green, self.blue, self.alpha
            ),
            ColorRepr::Function => write!(f, "#{:X}{:X}{:X}", self.red, self.green, self.blue),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum ColorRepr {
    Name(ColorName),
    Hex3,
    Hex6,
    Hex8,
    Function,
}
