use std::convert::TryFrom;
use std::fmt::{self, Display};

use crate::value::Number;
pub(crate) use name::ColorName;

use num_traits::cast::ToPrimitive;

mod name;

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct Color {
    red: u8,
    green: u8,
    blue: u8,
    alpha: Number,
    repr: String,
}

impl Color {
    pub fn new(red: u8, green: u8, blue: u8, alpha: u8, repr: String) -> Self {
        Color {
            red,
            green,
            blue,
            alpha: alpha.into(),
            repr,
        }
    }

    pub const fn red(&self) -> u8 {
        self.red
    }

    pub const fn blue(&self) -> u8 {
        self.blue
    }

    pub const fn green(&self) -> u8 {
        self.green
    }

    /// Calculate hue from RGBA values
    /// Algorithm adapted from http://www.niwa.nu/2013/05/math-behind-colorspace-conversions-rgb-hsl/
    pub fn hue(&self) -> Number {
        let red = Number::ratio(self.red, 255);
        let green = Number::ratio(self.green, 255);
        let blue = Number::ratio(self.blue, 255);
        let min = red.clone().min(green.clone().min(blue.clone()));
        let max = red.clone().max(green.clone().max(blue.clone()));
        if &min == &max {
            return Number::from(0);
        }

        let mut hue = if &red == &max {
            (green - blue) / (max - min)
        } else if &green == &max {
            Number::from(2) + (blue - red) / (max - min)
        } else {
            Number::from(4) + (red - green) / (max - min)
        };

        if hue < Number::from(0) {
            hue += Number::from(360);
        }

        (hue * Number::from(60)).round()
    }

    /// Calculate saturation from RGBA values
    /// Algorithm adapted from http://www.niwa.nu/2013/05/math-behind-colorspace-conversions-rgb-hsl/
    pub fn saturation(&self) -> Number {
        let red = Number::ratio(self.red, 255);
        let green = Number::ratio(self.green, 255);
        let blue = Number::ratio(self.blue, 255);
        let mut min = red.clone().min(green.clone().min(blue.clone()));
        min = Number::ratio((min * Number::from(100)).to_integer(), 100);
        let mut max = red.max(green.max(blue));
        max = Number::ratio((max * Number::from(100)).to_integer(), 100);
        let luminance = (min.clone() + max.clone()) / Number::from(2);
        if &min == &max {
            return Number::from(0);
        }

        let saturation = if luminance < Number::ratio(1, 2) {
            (max.clone() - min.clone()) / (max + min)
        } else {
            (max.clone() - min.clone()) / (Number::from(2) - max - min)
        } * Number::from(100);

        saturation.round()
    }

    /// Calculate luminance from RGBA values
    /// Algorithm adapted from http://www.niwa.nu/2013/05/math-behind-colorspace-conversions-rgb-hsl/
    pub fn lightness(&self) -> Number {
        let red = Number::ratio(self.red, 255);
        let green = Number::ratio(self.green, 255);
        let blue = Number::ratio(self.blue, 255);
        let min = red.clone().min(green.clone().min(blue.clone()));
        let max = red.max(green.max(blue));
        (((min + max) / Number::from(2)) * Number::from(100)).round()
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
                .to_u8()
                .unwrap();
            let repr = repr(val, val, val, &alpha);
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
        hue /= Number::from(360);
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
                .to_u8()
                .expect("expected channel to fit inside u8");
            };
        }

        channel!(red, temporary_r, temporary_1, temporary_2);
        channel!(green, temporary_g, temporary_1, temporary_2);
        channel!(blue, temporary_b, temporary_1, temporary_2);

        let repr = repr(red, green, blue, &alpha);
        Color {
            red,
            green,
            blue,
            alpha,
            repr,
        }
    }

    pub fn from_rgba(red: Number, green: Number, blue: Number, alpha: Number) -> Self {
        macro_rules! clamp {
            ($channel:ident) => {
                let $channel = if $channel > Number::from(255) {
                    255_u8
                } else if $channel < Number::from(0) {
                    0_u8
                } else {
                    $channel.round().to_integer().to_u8().unwrap()
                };
            };
        }

        clamp!(red);
        clamp!(green);
        clamp!(blue);

        let alpha = if alpha > Number::from(1) {
            Number::from(1)
        } else if alpha < Number::from(0) {
            Number::from(0)
        } else {
            alpha
        };

        let repr = repr(red, green, blue, &alpha);
        Color {
            red,
            green,
            blue,
            alpha,
            repr,
        }
    }

    pub fn invert(&self) -> Self {
        let red = std::u8::MAX - self.red;
        let green = std::u8::MAX - self.green;
        let blue = std::u8::MAX - self.blue;
        let repr = repr(red, green, blue, &self.alpha);
        Color {
            red,
            green,
            blue,
            alpha: self.alpha.clone(),
            repr,
        }
    }
}

/// Get the proper representation from RGBA values
fn repr(red: u8, green: u8, blue: u8, alpha: &Number) -> String {
    if alpha < &Number::from(1) {
        format!("rgba({}, {}, {}, {})", red, green, blue, alpha)
    } else if let Ok(c) = ColorName::try_from([red, green, blue]) {
        format!("{}", c)
    } else {
        format!("#{:0>2x}{:0>2x}{:0>2x}", red, green, blue)
    }
}

impl Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.repr)
    }
}
