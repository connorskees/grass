use std::convert::TryFrom;
use std::fmt::{self, Display};

use crate::value::Number;
pub(crate) use name::ColorName;

use num_traits::cast::ToPrimitive;

mod name;

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct Color {
    red: Number,
    green: Number,
    blue: Number,
    alpha: Number,
    repr: String,
}

impl Color {
    pub fn new(red: u8, green: u8, blue: u8, alpha: u8, repr: String) -> Self {
        Color {
            red: red.into(),
            green: green.into(),
            blue: blue.into(),
            alpha: alpha.into(),
            repr,
        }
    }

    pub fn red(&self) -> Number {
        self.red.clone()
    }

    pub fn blue(&self) -> Number {
        self.blue.clone()
    }

    pub fn green(&self) -> Number {
        self.green.clone()
    }

    /// Calculate hue from RGBA values
    /// Algorithm adapted from <http://www.niwa.nu/2013/05/math-behind-colorspace-conversions-rgb-hsl/>
    pub fn hue(&self) -> Number {
        let red = self.red.clone() / Number::from(255);
        let green = self.green.clone() / Number::from(255);
        let blue = self.blue.clone() / Number::from(255);
        let min = red.clone().min(green.clone().min(blue.clone()));
        let max = red.clone().max(green.clone().max(blue.clone()));
        if min == max {
            return Number::from(0);
        }

        let mut hue = if blue == max {
            Number::from(4) + (red - green) / (max - min)
        } else if green == max {
            Number::from(2) + (blue - red) / (max - min)
        } else {
            (green - blue) / (max - min)
        };

        if hue < Number::from(0) {
            hue += Number::from(360);
        }

        (hue * Number::from(60)).round()
    }

    /// Calculate saturation from RGBA values
    /// Algorithm adapted from <http://www.niwa.nu/2013/05/math-behind-colorspace-conversions-rgb-hsl/>
    pub fn saturation(&self) -> Number {
        let red = self.red.clone() / Number::from(255);
        let green = self.green.clone() / Number::from(255);
        let blue = self.blue.clone() / Number::from(255);

        let min = red.clone().min(green.clone().min(blue.clone()));
        let max = red.max(green.max(blue));

        if min == max {
            return Number::from(0);
        }

        let d = max.clone() - min.clone();
        let mm = max + min;
        let s = d / if mm > Number::from(1) {
            Number::from(2) - mm
        } else {
            mm
        };
        (s * Number::from(100)).round()
    }

    /// Calculate luminance from RGBA values
    /// Algorithm adapted from <http://www.niwa.nu/2013/05/math-behind-colorspace-conversions-rgb-hsl/>
    pub fn lightness(&self) -> Number {
        let red = self.red.clone() / Number::from(255);
        let green = self.green.clone() / Number::from(255);
        let blue = self.blue.clone() / Number::from(255);
        let min = red.clone().min(green.clone().min(blue.clone()));
        let max = red.max(green.max(blue));
        (((min + max) / Number::from(2)) * Number::from(100)).round()
    }

    fn as_hsla(&self) -> (Number, Number, Number, Number) {
        let red = self.red.clone() / Number::from(255);
        let green = self.green.clone() / Number::from(255);
        let blue = self.blue.clone() / Number::from(255);
        let min = red.clone().min(green.clone().min(blue.clone()));
        let max = red.clone().max(green.clone().max(blue.clone()));

        let lightness = (min.clone() + max.clone()) / Number::from(2);

        let saturation = if &min == &max {
            Number::from(0)
        } else {
            let d = max.clone() - min.clone();
            let mm = max.clone() + min.clone();
            d / if mm > Number::from(1) {
                Number::from(2) - mm
            } else {
                mm
            }
        };

        let mut hue = if min == max {
            Number::from(0)
        } else if blue == max {
            Number::from(4) + (red - green) / (max - min)
        } else if green == max {
            Number::from(2) + (blue - red) / (max - min)
        } else {
            (green - blue) / (max - min)
        };

        if hue < Number::from(0) {
            hue += Number::from(360);
        }

        hue *= Number::from(60);

        (hue, saturation, lightness, self.alpha())
    }

    pub fn adjust_hue(&self, degrees: Number) -> Self {
        let (hue, saturation, luminance, alpha) = self.as_hsla();
        Color::from_hsla(hue + degrees, saturation, luminance, alpha)
    }

    pub fn lighten(&self, amount: Number) -> Self {
        let (hue, saturation, luminance, alpha) = self.as_hsla();
        Color::from_hsla(hue, saturation, luminance + amount, alpha)
    }

    pub fn darken(&self, amount: Number) -> Self {
        let (hue, saturation, luminance, alpha) = self.as_hsla();
        Color::from_hsla(hue, saturation, luminance - amount, alpha)
    }

    pub fn saturate(&self, amount: Number) -> Self {
        let (hue, saturation, luminance, alpha) = self.as_hsla();
        Color::from_hsla(hue, saturation + amount, luminance, alpha)
    }

    pub fn desaturate(&self, amount: Number) -> Self {
        let (hue, saturation, luminance, alpha) = self.as_hsla();
        Color::from_hsla(hue, saturation - amount, luminance, alpha)
    }

    pub fn alpha(&self) -> Number {
        self.alpha.clone()
    }

    pub fn with_alpha(self, alpha: Number) -> Self {
        Color::from_rgba(self.red, self.green, self.blue, alpha)
    }

    /// Makes a color more opaque.
    /// Takes a color and a number between 0 and 1,
    /// and returns a color with the opacity increased by that amount.
    pub fn fade_in(self, amount: Number) -> Self {
        Color::from_rgba(self.red, self.green, self.blue, self.alpha + amount)
    }

    /// Makes a color more transparent.
    /// Takes a color and a number between 0 and 1,
    /// and returns a color with the opacity decreased by that amount.
    pub fn fade_out(self, amount: Number) -> Self {
        Color::from_rgba(self.red, self.green, self.blue, self.alpha - amount)
    }

    /// Create RGBA representation from HSLA values
    /// Algorithm adapted from <http://www.niwa.nu/2013/05/math-behind-colorspace-conversions-rgb-hsl/>
    pub fn from_hsla(
        mut hue: Number,
        mut saturation: Number,
        mut luminance: Number,
        mut alpha: Number,
    ) -> Self {
        macro_rules! clamp {
            ($c:ident, $min:literal, $max:literal) => {
                if $c > Number::from($max) {
                    $c = Number::from($max)
                } else if $c < Number::from($min) {
                    $c = Number::from($min)
                }
            };
        }

        clamp!(hue, 0, 360);
        clamp!(saturation, 0, 1);
        clamp!(luminance, 0, 1);
        clamp!(alpha, 0, 1);

        if saturation.clone() == Number::from(0) {
            let luminance = if luminance > Number::from(100) {
                Number::from(100)
            } else {
                luminance
            };
            let val = luminance * Number::from(255);
            let repr = repr(&val, &val, &val, &alpha);
            return Color {
                red: val.clone(),
                green: val.clone(),
                blue: val,
                alpha,
                repr,
            };
        }
        let temporary_1 = if luminance.clone() < Number::ratio(1, 2) {
            luminance.clone() * (Number::from(1) + saturation)
        } else {
            luminance.clone() + saturation.clone() - luminance.clone() * saturation
        };
        let temporary_2 = Number::from(2) * luminance - temporary_1.clone();
        hue /= Number::from(360);
        let mut temporary_r = hue.clone() + Number::ratio(1, 3);
        let mut temporary_g = hue.clone();
        let mut temporary_b = hue - Number::ratio(1, 3);

        macro_rules! clamp_temp {
            ($temp:ident) => {
                if $temp > Number::from(1) {
                    $temp -= Number::from(1);
                } else if $temp < Number::from(0) {
                    $temp += Number::from(1);
                }
            };
        }

        clamp_temp!(temporary_r);
        clamp_temp!(temporary_g);
        clamp_temp!(temporary_b);

        macro_rules! channel {
            ($name:ident, $temp:ident, $temp1:ident, $temp2:ident) => {
                let $name = if Number::from(6) * $temp.clone() < Number::from(1) {
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
                } * Number::from(255);
            };
        }

        channel!(red, temporary_r, temporary_1, temporary_2);
        channel!(green, temporary_g, temporary_1, temporary_2);
        channel!(blue, temporary_b, temporary_1, temporary_2);

        let repr = repr(&red, &green, &blue, &alpha);
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
                    Number::from(255)
                } else if $channel < Number::from(0) {
                    Number::from(0)
                } else {
                    $channel
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

        let repr = repr(&red, &green, &blue, &alpha);
        Color {
            red,
            green,
            blue,
            alpha,
            repr,
        }
    }

    pub fn invert(&self, weight: Number) -> Self {
        let weight = if weight > Number::from(1) {
            Number::from(1)
        } else if weight < Number::from(0) {
            Number::from(0)
        } else {
            weight
        };
        let red = Number::from(u8::max_value()) - self.red.clone() * weight.clone();
        let green = Number::from(u8::max_value()) - self.green.clone() * weight.clone();
        let blue = Number::from(u8::max_value()) - self.blue.clone() * weight;
        let repr = repr(&red, &green, &blue, &self.alpha);
        Color {
            red,
            green,
            blue,
            alpha: self.alpha.clone(),
            repr,
        }
    }

    pub fn complement(&self) -> Self {
        let (hue, saturation, luminance, alpha) = self.as_hsla();
        let hue = if hue > Number::from(180) {
            Number::from(360) - hue
        } else {
            hue + Number::from(180)
        };
        Color::from_hsla(hue, saturation, luminance, alpha)
    }
}

/// Get the proper representation from RGBA values
fn repr(red: &Number, green: &Number, blue: &Number, alpha: &Number) -> String {
    macro_rules! into_u8 {
        ($channel:ident) => {
            let $channel = if $channel > &Number::from(255) {
                255_u8
            } else if $channel < &Number::from(0) {
                0_u8
            } else {
                $channel.clone().round().to_integer().to_u8().unwrap()
            };
        };
    }

    into_u8!(red);
    into_u8!(green);
    into_u8!(blue);

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
