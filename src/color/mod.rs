//! A color is internally represented as either RGBA or HSLA.
//! Colors can be constructed in SASS through names (e.g. red, blue, aqua)
//! or the builtin functions `rgb()`, `rgba()`, `hsl()`, and `hsla()`,
//! all of which can accept 1-4 arguments.
//!
//! It is necessary to retain the original values with which the
//! color was constructed.
//! E.g. `hsla(.999999999999, 100, 100, 1)` should retain its full
//! values to an arbitrary precision.
//!
//! Named colors are created using RGB values
//! E.g. `red` = `rgba(255, 0, 0, 1)`
//!
//! In addition, named colors retain their original casing,
//! so `rEd` should be emitted as `rEd`.

use std::convert::TryFrom;
use std::fmt::{self, Display};

use crate::value::Number;
pub(crate) use name::ColorName;

use num_traits::{One, Signed, ToPrimitive, Zero};

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
    rgba: Rgba,
    hsla: Option<Hsla>,
    repr: String,
}

impl Color {
    pub const fn new_rgba(
        red: Number,
        green: Number,
        blue: Number,
        alpha: Number,
        repr: String,
    ) -> Color {
        Color {
            rgba: Rgba::new(red, green, blue, alpha),
            hsla: None,
            repr,
        }
    }

    fn new_hsla(
        red: Number,
        green: Number,
        blue: Number,
        alpha: Number,
        hsla: Hsla,
        repr: String,
    ) -> Color {
        Color {
            rgba: Rgba::new(red, green, blue, alpha),
            hsla: Some(hsla),
            repr,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct Rgba {
    red: Number,
    green: Number,
    blue: Number,
    alpha: Number,
}

impl Rgba {
    pub const fn new(red: Number, green: Number, blue: Number, alpha: Number) -> Self {
        Rgba {
            red,
            green,
            blue,
            alpha,
        }
    }

    pub fn alpha(&self) -> Number {
        self.alpha.clone()
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct Hsla {
    hue: Number,
    saturation: Number,
    luminance: Number,
    alpha: Number,
}

impl Hsla {
    pub const fn new(hue: Number, saturation: Number, luminance: Number, alpha: Number) -> Self {
        Hsla {
            hue,
            saturation,
            luminance,
            alpha,
        }
    }

    pub fn hue(&self) -> Number {
        self.hue.clone()
    }

    pub fn saturation(&self) -> Number {
        self.saturation.clone()
    }

    pub fn luminance(&self) -> Number {
        self.luminance.clone()
    }

    pub fn alpha(&self) -> Number {
        self.alpha.clone()
    }
}

// RGBA color functions
impl Color {
    pub fn new(red: u8, green: u8, blue: u8, alpha: u8, repr: String) -> Self {
        Color {
            rgba: Rgba::new(red.into(), green.into(), blue.into(), alpha.into()),
            hsla: None,
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
                } else if $channel.is_negative() {
                    Number::zero()
                } else {
                    $channel
                };
            };
        }

        clamp!(red);
        clamp!(green);
        clamp!(blue);

        let alpha = if alpha > Number::one() {
            Number::one()
        } else if alpha.is_negative() {
            Number::zero()
        } else {
            alpha
        };

        let repr = repr(&red, &green, &blue, &alpha);
        Color::new_rgba(red, green, blue, alpha, repr)
    }

    pub fn red(&self) -> Number {
        self.rgba.red.clone()
    }

    pub fn blue(&self) -> Number {
        self.rgba.blue.clone()
    }

    pub fn green(&self) -> Number {
        self.rgba.green.clone()
    }

    /// Mix two colors together with weight
    /// Algorithm adapted from
    /// <https://github.com/sass/dart-sass/blob/0d0270cb12a9ac5cce73a4d0785fecb00735feee/lib/src/functions/color.dart#L718>
    pub fn mix(self, other: &Color, weight: Number) -> Self {
        let weight = clamp!(weight, 0, 100);
        let normalized_weight = weight.clone() * Number::from(2) - Number::one();
        let alpha_distance = self.alpha() - other.alpha();

        let combined_weight1 =
            if normalized_weight.clone() * alpha_distance.clone() == Number::from(-1) {
                normalized_weight
            } else {
                (normalized_weight.clone() + alpha_distance.clone())
                    / (Number::one() + normalized_weight * alpha_distance)
            };
        let weight1 = (combined_weight1 + Number::one()) / Number::from(2);
        let weight2 = Number::one() - weight1.clone();

        Color::from_rgba(
            self.red() * weight1.clone() + other.red() * weight2.clone(),
            self.green() * weight1.clone() + other.green() * weight2.clone(),
            self.blue() * weight1 + other.blue() * weight2,
            self.alpha() * weight.clone() + other.alpha() * (Number::one() - weight),
        )
    }
}

/// HSLA color functions
/// Algorithms adapted from <http://www.niwa.nu/2013/05/math-behind-colorspace-conversions-rgb-hsl/>
impl Color {
    /// Calculate hue from RGBA values
    pub fn hue(&self) -> Number {
        if let Some(h) = &self.hsla {
            return h.hue();
        }

        let red = self.red() / Number::from(255);
        let green = self.green() / Number::from(255);
        let blue = self.blue() / Number::from(255);
        let min = red.clone().min(green.clone().min(blue.clone()));
        let max = red.clone().max(green.clone().max(blue.clone()));
        if min == max {
            return Number::zero();
        }

        let mut hue = if blue == max {
            Number::from(4) + (red - green) / (max - min)
        } else if green == max {
            Number::from(2) + (blue - red) / (max - min)
        } else {
            (green - blue) / (max - min)
        };

        if hue.is_negative() {
            hue += Number::from(360);
        }

        (hue * Number::from(60)).round()
    }

    /// Calculate saturation from RGBA values
    pub fn saturation(&self) -> Number {
        if let Some(h) = &self.hsla {
            return h.saturation() * Number::from(100);
        }

        let red = self.red() / Number::from(255);
        let green = self.green() / Number::from(255);
        let blue = self.blue() / Number::from(255);

        let min = red.clone().min(green.clone().min(blue.clone()));
        let max = red.max(green.max(blue));

        if min == max {
            return Number::zero();
        }

        let d = max.clone() - min.clone();
        let mm = max + min;
        let s = d / if mm > Number::one() {
            Number::from(2) - mm
        } else {
            mm
        };
        (s * Number::from(100)).round()
    }

    /// Calculate luminance from RGBA values
    pub fn lightness(&self) -> Number {
        if let Some(h) = &self.hsla {
            return h.luminance() * Number::from(100);
        }

        let red = self.red() / Number::from(255);
        let green = self.green() / Number::from(255);
        let blue = self.blue() / Number::from(255);
        let min = red.clone().min(green.clone().min(blue.clone()));
        let max = red.max(green.max(blue));
        (((min + max) / Number::from(2)) * Number::from(100)).round()
    }

    pub fn as_hsla(&self) -> (Number, Number, Number, Number) {
        if let Some(h) = &self.hsla {
            return (h.hue(), h.saturation(), h.luminance(), h.alpha());
        }

        let red = self.red() / Number::from(255);
        let green = self.green() / Number::from(255);
        let blue = self.blue() / Number::from(255);
        let min = red.clone().min(green.clone().min(blue.clone()));
        let max = red.clone().max(green.clone().max(blue.clone()));

        let lightness = (min.clone() + max.clone()) / Number::from(2);

        let saturation = if min == max {
            Number::zero()
        } else {
            let d = max.clone() - min.clone();
            let mm = max.clone() + min.clone();
            d / if mm > Number::one() {
                Number::from(2) - mm
            } else {
                mm
            }
        };

        let mut hue = if min == max {
            Number::zero()
        } else if blue == max {
            Number::from(4) + (red - green) / (max - min)
        } else if green == max {
            Number::from(2) + (blue - red) / (max - min)
        } else {
            (green - blue) / (max - min)
        };

        if hue.is_negative() {
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
        } else if hue.is_negative() {
            Number::from(360) + clamp!(hue, -360, 360)
        } else {
            hue
        };

        let saturation = clamp!(saturation, 0, 1);
        let luminance = clamp!(luminance, 0, 1);
        let alpha = clamp!(alpha, 0, 1);

        let hsla = Hsla::new(
            hue.clone(),
            saturation.clone(),
            luminance.clone(),
            alpha.clone(),
        );

        if saturation.is_zero() {
            let luminance = if luminance > Number::from(100) {
                Number::from(100)
            } else {
                luminance
            };
            let val = luminance * Number::from(255);
            let repr = repr(&val, &val, &val, &alpha);
            return Color::new_hsla(val.clone(), val.clone(), val, alpha, hsla, repr);
        }
        let temporary_1 = if luminance < Number::ratio(1, 2) {
            luminance.clone() * (Number::one() + saturation)
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
                if $temp > Number::one() {
                    $temp -= Number::one();
                } else if $temp.is_negative() {
                    $temp += Number::one();
                }
            };
        }

        clamp_temp!(temporary_r);
        clamp_temp!(temporary_g);
        clamp_temp!(temporary_b);

        macro_rules! channel {
            ($name:ident, $temp:ident, $temp1:ident, $temp2:ident) => {
                let $name = if Number::from(6) * $temp.clone() < Number::one() {
                    $temp2.clone()
                        + ($temp1.clone() - $temp2.clone()) * Number::from(6) * $temp.clone()
                } else if Number::from(2) * $temp.clone() < Number::one() {
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
        Color::new_hsla(red, green, blue, alpha, hsla, repr)
    }

    pub fn invert(&self, weight: Number) -> Self {
        if weight.is_zero() {
            return self.clone();
        }
        let red = Number::from(u8::max_value()) - self.red();
        let green = Number::from(u8::max_value()) - self.green();
        let blue = Number::from(u8::max_value()) - self.blue();
        let repr = repr(&red, &green, &blue, &self.alpha());
        let inverse = Color::new_rgba(red, green, blue, self.alpha(), repr);
        inverse.mix(self, weight)
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
        let a = self.rgba.alpha();
        if a > Number::one() {
            a / Number::from(255)
        } else {
            a
        }
    }

    /// Change `alpha` to value given
    pub fn with_alpha(self, alpha: Number) -> Self {
        Color::from_rgba(self.red(), self.green(), self.blue(), alpha)
    }

    /// Makes a color more opaque.
    /// Takes a color and a number between 0 and 1,
    /// and returns a color with the opacity increased by that amount.
    pub fn fade_in(self, amount: Number) -> Self {
        Color::from_rgba(self.red(), self.green(), self.blue(), self.alpha() + amount)
    }

    /// Makes a color more transparent.
    /// Takes a color and a number between 0 and 1,
    /// and returns a color with the opacity decreased by that amount.
    pub fn fade_out(self, amount: Number) -> Self {
        Color::from_rgba(self.red(), self.green(), self.blue(), self.alpha() - amount)
    }
}

/// Other color functions
impl Color {
    pub fn to_ie_hex_str(&self) -> String {
        format!(
            "#{:02X}{:02X}{:02X}{:02X}",
            (self.alpha() * Number::from(255)).round().to_integer(),
            self.red().to_integer(),
            self.green().to_integer(),
            self.blue().to_integer()
        )
    }
}

/// Get the proper representation from RGBA values
fn repr(red: &Number, green: &Number, blue: &Number, alpha: &Number) -> String {
    macro_rules! into_u8 {
        ($channel:ident) => {
            let $channel = if $channel > &Number::from(255) {
                255_u8
            } else if $channel.is_negative() {
                0_u8
            } else {
                $channel.clone().round().to_integer().to_u8().unwrap()
            };
        };
    }

    into_u8!(red);
    into_u8!(green);
    into_u8!(blue);

    if alpha < &Number::one() {
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
