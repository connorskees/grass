use std::convert::TryFrom;
use std::fmt::{self, Display};

use crate::value::Number;
pub(crate) use name::ColorName;

use num_traits::cast::ToPrimitive;

mod name;

macro_rules! clamp {
    ($c:expr, $min:literal, $max:literal) => {
        if $c > Number::from($max) {
            Number::from($max)
        } else if $c < Number::from($min) {
            Number::from($min)
        } else {
            $c
        }
    };
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct Color {
    red: Number,
    green: Number,
    blue: Number,
    alpha: Number,
    repr: String,
}

// RGBA color functions
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

    /// Create a new `Color` with just RGBA values.
    /// Color representation is created automatically.
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

    pub fn red(&self) -> Number {
        self.red.clone()
    }

    pub fn blue(&self) -> Number {
        self.blue.clone()
    }

    pub fn green(&self) -> Number {
        self.green.clone()
    }

    /// Mix two colors together with weight
    /// Algorithm adapted from
    /// <https://github.com/sass/dart-sass/blob/0d0270cb12a9ac5cce73a4d0785fecb00735feee/lib/src/functions/color.dart#L718>
    pub fn mix(self, other: Color, weight: Number) -> Self {
        let weight = clamp!(weight, 0, 100);
        let normalized_weight = weight.clone() * Number::from(2) - Number::from(1);
        let alpha_distance = self.alpha.clone() - other.alpha.clone();

        let combined_weight1 =
            if normalized_weight.clone() * alpha_distance.clone() == Number::from(-1) {
                normalized_weight
            } else {
                (normalized_weight.clone() + alpha_distance.clone())
                    / (Number::from(1) + normalized_weight * alpha_distance)
            };
        let weight1 = (combined_weight1 + Number::from(1)) / Number::from(2);
        let weight2 = Number::from(1) - weight1.clone();

        Color::from_rgba(
            self.red * weight1.clone() + other.red * weight2.clone(),
            self.green * weight1.clone() + other.green * weight2.clone(),
            self.blue * weight1 + other.blue * weight2,
            self.alpha * weight.clone() + other.alpha * (Number::from(1) - weight),
        )
    }
}

/// HSLA color functions
/// Algorithms adapted from <http://www.niwa.nu/2013/05/math-behind-colorspace-conversions-rgb-hsl/>
impl Color {
    /// Calculate hue from RGBA values
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
    pub fn lightness(&self) -> Number {
        let red = self.red.clone() / Number::from(255);
        let green = self.green.clone() / Number::from(255);
        let blue = self.blue.clone() / Number::from(255);
        let min = red.clone().min(green.clone().min(blue.clone()));
        let max = red.max(green.max(blue));
        (((min + max) / Number::from(2)) * Number::from(100)).round()
    }

    pub fn as_hsla(&self) -> (Number, Number, Number, Number) {
        let red = self.red.clone() / Number::from(255);
        let green = self.green.clone() / Number::from(255);
        let blue = self.blue.clone() / Number::from(255);
        let min = red.clone().min(green.clone().min(blue.clone()));
        let max = red.clone().max(green.clone().max(blue.clone()));

        let lightness = (min.clone() + max.clone()) / Number::from(2);

        let saturation = if min == max {
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

    /// Create RGBA representation from HSLA values
    pub fn from_hsla(hue: Number, saturation: Number, luminance: Number, alpha: Number) -> Self {
        let mut hue = if hue > Number::from(360) {
            hue % Number::from(360)
        } else if hue < Number::from(-360) {
            Number::from(360) + hue % Number::from(360)
        } else if hue < Number::from(0) {
            Number::from(360) + clamp!(hue, -360, 360)
        } else {
            hue
        };

        let saturation = clamp!(saturation, 0, 1);
        let luminance = clamp!(luminance, 0, 1);
        let alpha = clamp!(alpha, 0, 1);

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

/// Opacity color functions
impl Color {
    pub fn alpha(&self) -> Number {
        let a = self.alpha.clone();
        if a > Number::from(1) {
            a / Number::from(255)
        } else {
            a
        }
    }

    /// Change `alpha` to value given
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
}

/// Other color functions
impl Color {
    pub fn to_ie_hex_str(&self) -> String {
        format!(
            "#{:02X}{:02X}{:02X}{:02X}",
            (self.alpha.clone() * Number::from(255))
                .round()
                .to_integer(),
            self.red.to_integer(),
            self.green.to_integer(),
            self.blue.to_integer()
        )
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
