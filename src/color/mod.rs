use std::convert::TryFrom;
use std::fmt::{self, Display};

use crate::value::Number;
pub(crate) use name::ColorName;

use num_traits::cast::ToPrimitive;

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

    /// Create RGBA representation from HSLA values
    /// Algorithm adapted from http://www.niwa.nu/2013/05/math-behind-colorspace-conversions-rgb-hsl/
    pub fn from_hsla(
        mut hue: Number,
        saturation: Number,
        luminance: Number,
        alpha: Number,
    ) -> Self {
        if saturation.clone() == Number::from(0) {
            let luminance = if luminance > Number::from(100) {
                Number::from(100)
            } else {
                luminance
            };
            let val = (luminance.clone() * Number::from(255))
                .to_integer()
                .to_u16()
                .unwrap();
            let repr = if alpha < Number::from(1) {
                format!("rgba({}, {}, {}, {})", val, val, val, alpha)
            } else if let Ok(c) = ColorName::try_from([val, val, val]) {
                format!("{}", c)
            } else {
                format!("#{:0>2x}{:0>2x}{:0>2x}", val, val, val)
            };
            return Color {
                red: val,
                green: val,
                blue: val,
                alpha: Number::from(alpha),
                repr,
            };
        }
        let temporary_1 = if luminance.clone() < Number::ratio(1, 2) {
            luminance.clone() * (Number::from(1) + saturation)
        } else {
            luminance.clone() + saturation.clone() - luminance.clone() * saturation.clone()
        };
        let temporary_2 = Number::from(2) * luminance.clone() - temporary_1.clone();
        hue = hue / Number::from(360);
        let mut temporary_r = hue.clone() + Number::ratio(1, 3);
        let mut temporary_g = hue.clone();
        let mut temporary_b = hue.clone() - Number::ratio(1, 3);

        macro_rules! clamp {
            ($temp:ident) => {
                if $temp > Number::from(1) {
                    $temp -= Number::from(1);
                } else if $temp < Number::from(0) {
                    $temp += Number::from(1);
                }
            };
        }

        clamp!(temporary_r);
        clamp!(temporary_g);
        clamp!(temporary_b);

        macro_rules! channel {
            ($name:ident, $temp:ident, $temp1:ident, $temp2:ident) => {
                let $name = (if Number::from(6) * $temp.clone() < Number::from(1) {
                    $temp2.clone()
                        + ($temp1.clone() - $temp2.clone()) * Number::from(6) * $temp.clone()
                } else if Number::from(2) * $temp.clone() < Number::from(1) {
                    $temp1.clone()
                } else if Number::from(3) * $temp.clone() < Number::from(2) {
                    $temp2.clone()
                        + ($temp1.clone() - $temp2.clone())
                            * (Number::ratio(2, 3) - $temp)
                            * Number::from(6)
                } else {
                    $temp2.clone()
                } * Number::from(255))
                .round()
                .to_integer()
                .to_u16()
                .expect("expected channel to fit inside u16");
            };
        }

        channel!(red, temporary_r, temporary_1, temporary_2);
        channel!(green, temporary_g, temporary_1, temporary_2);
        channel!(blue, temporary_b, temporary_1, temporary_2);

        let repr = if alpha < Number::from(1) {
            format!("rgba({}, {}, {}, {})", red, green, blue, alpha)
        } else if let Ok(c) = ColorName::try_from([red, green, blue]) {
            format!("{}", c)
        } else {
            format!("#{:0>2x}{:0>2x}{:0>2x}", red, green, blue)
        };
        Color {
            red,
            green,
            blue,
            alpha,
            repr,
        }
    }

    pub fn from_values(red: u16, green: u16, blue: u16, alpha: Number) -> Self {
        let repr = if alpha < Number::from(1) {
            format!("rgba({}, {}, {}, {})", red, green, blue, alpha)
        } else if let Ok(c) = ColorName::try_from([red, green, blue]) {
            format!("{}", c)
        } else {
            format!("#{:0>2x}{:0>2x}{:0>2x}", red, green, blue)
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
