//! A color is internally represented as either RGBA or HSLA.
//!
//! Colors can be constructed in Sass through names (e.g. red, blue, aqua)
//! or the builtin functions `rgb()`, `rgba()`, `hsl()`, and `hsla()`,
//! all of which can accept 1-4 arguments.
//!
//! It is necessary to retain the original values with which the
//! color was constructed.
//! E.g. `hsla(.999999999999, 100, 100, 1)` should retain its full HSLA
//! values to an arbitrary precision.
//!
//! Color values matching named colors are implicitly converted to named colors
//! E.g. `rgba(255, 0, 0, 1)` => `red`
//!
//! Named colors retain their original casing,
//! so `rEd` should be emitted as `rEd`.

use std::{
    cmp::{max, min},
    fmt::{self, Display},
};

use crate::value::Number;
pub(crate) use name::NAMED_COLORS;

use num_traits::{One, Signed, ToPrimitive, Zero};

mod name;

#[derive(Debug, Clone)]
pub(crate) struct Color {
    rgba: Rgba,
    hsla: Option<Hsla>,
    repr: String,
}

impl PartialEq for Color {
    fn eq(&self, other: &Self) -> bool {
        self.rgba == other.rgba
    }
}

impl Eq for Color {}

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

    const fn new_hsla(
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

#[derive(Debug, Clone)]
struct Rgba {
    red: Number,
    green: Number,
    blue: Number,
    alpha: Number,
}

impl PartialEq for Rgba {
    fn eq(&self, other: &Self) -> bool {
        if self.red != other.red
            && !(self.red >= Number::from(255) && other.red >= Number::from(255))
        {
            return false;
        }
        if self.green != other.green
            && !(self.green >= Number::from(255) && other.green >= Number::from(255))
        {
            return false;
        }
        if self.blue != other.blue
            && !(self.blue >= Number::from(255) && other.blue >= Number::from(255))
        {
            return false;
        }
        if self.alpha != other.alpha
            && !(self.alpha >= Number::one() && other.alpha >= Number::one())
        {
            return false;
        }
        true
    }
}

impl Eq for Rgba {}

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

#[derive(Debug, Clone)]
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
    pub fn from_rgba(
        mut red: Number,
        mut green: Number,
        mut blue: Number,
        mut alpha: Number,
    ) -> Self {
        red = red.clamp(0, 255);
        green = green.clamp(0, 255);
        blue = blue.clamp(0, 255);
        alpha = alpha.clamp(0, 1);

        let repr = repr(&red, &green, &blue, &alpha);
        Color::new_rgba(red, green, blue, alpha, repr)
    }

    pub fn red(&self) -> Number {
        self.rgba.red.clone().round()
    }

    pub fn blue(&self) -> Number {
        self.rgba.blue.clone().round()
    }

    pub fn green(&self) -> Number {
        self.rgba.green.clone().round()
    }

    /// Mix two colors together with weight
    /// Algorithm adapted from
    /// <https://github.com/sass/dart-sass/blob/0d0270cb12a9ac5cce73a4d0785fecb00735feee/lib/src/functions/color.dart#L718>
    pub fn mix(self, other: &Color, weight: Number) -> Self {
        let weight = weight.clamp(0, 100);
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

        let min = min(&red, min(&green, &blue)).clone();
        let max = max(&red, max(&green, &blue)).clone();

        let delta = max.clone() - min.clone();

        let hue = if min == max {
            Number::zero()
        } else if max == red {
            Number::from(60_u8) * (green - blue) / delta
        } else if max == green {
            Number::from(120_u8) + Number::from(60_u8) * (blue - red) / delta
        } else {
            Number::from(240_u8) + Number::from(60_u8) * (red - green) / delta
        };

        hue % Number::from(360)
    }

    /// Calculate saturation from RGBA values
    pub fn saturation(&self) -> Number {
        if let Some(h) = &self.hsla {
            return h.saturation() * Number::from(100);
        }

        let red: Number = self.red() / Number::from(255);
        let green = self.green() / Number::from(255);
        let blue = self.blue() / Number::from(255);

        let min = min(&red, min(&green, &blue)).clone();
        let max = red.max(green.max(blue));

        if min == max {
            return Number::zero();
        }

        let delta = max.clone() - min.clone();

        let sum = max + min;

        let s = delta
            / if sum > Number::one() {
                Number::from(2) - sum
            } else {
                sum
            };

        s * Number::from(100)
    }

    /// Calculate luminance from RGBA values
    pub fn lightness(&self) -> Number {
        if let Some(h) = &self.hsla {
            return h.luminance() * Number::from(100);
        }

        let red: Number = self.red() / Number::from(255);
        let green = self.green() / Number::from(255);
        let blue = self.blue() / Number::from(255);
        let min = min(&red, min(&green, &blue)).clone();
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
        let min = min(&red, min(&green, &blue)).clone();
        let max = max(&red, max(&green, &blue)).clone();

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
        let mut hue = if hue >= Number::from(360) {
            hue % Number::from(360)
        } else if hue < Number::from(-360) {
            Number::from(360) + hue % Number::from(360)
        } else if hue.is_negative() {
            Number::from(360) + hue.clamp(-360, 360)
        } else {
            hue
        };

        let saturation = saturation.clamp(0, 1);
        let luminance = luminance.clamp(0, 1);
        let alpha = alpha.clamp(0, 1);

        let hsla = Hsla::new(
            hue.clone(),
            saturation.clone(),
            luminance.clone(),
            alpha.clone(),
        );

        if saturation.is_zero() {
            let val = luminance * Number::from(255);
            let repr = repr(&val, &val, &val, &alpha);
            return Color::new_hsla(val.clone(), val.clone(), val, alpha, hsla, repr);
        }

        let temporary_1 = if luminance < Number::small_ratio(1, 2) {
            luminance.clone() * (Number::one() + saturation)
        } else {
            luminance.clone() + saturation.clone() - luminance.clone() * saturation
        };
        let temporary_2 = Number::from(2) * luminance - temporary_1.clone();
        hue /= Number::from(360);
        let mut temporary_r = hue.clone() + Number::small_ratio(1, 3);
        let mut temporary_g = hue.clone();
        let mut temporary_b = hue - Number::small_ratio(1, 3);

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

        fn channel(temp: Number, temp1: &Number, temp2: &Number) -> Number {
            Number::from(255)
                * if Number::from(6) * temp.clone() < Number::one() {
                    temp2.clone() + (temp1.clone() - temp2.clone()) * Number::from(6) * temp
                } else if Number::from(2) * temp.clone() < Number::one() {
                    temp1.clone()
                } else if Number::from(3) * temp.clone() < Number::from(2) {
                    temp2.clone()
                        + (temp1.clone() - temp2.clone())
                            * (Number::small_ratio(2, 3) - temp)
                            * Number::from(6)
                } else {
                    temp2.clone()
                }
        }

        let red = channel(temporary_r, &temporary_1, &temporary_2);
        let green = channel(temporary_g, &temporary_1, &temporary_2);
        let blue = channel(temporary_b, &temporary_1, &temporary_2);

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

        Color::from_hsla(hue + Number::from(180), saturation, luminance, alpha)
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
            "#{:X}{:X}{:X}{:X}",
            (self.alpha() * Number::from(255)).round().to_integer(),
            self.red().to_integer(),
            self.green().to_integer(),
            self.blue().to_integer()
        )
    }
}

/// HWB color functions
impl Color {
    pub fn from_hwb(
        mut hue: Number,
        mut white: Number,
        mut black: Number,
        mut alpha: Number,
    ) -> Color {
        hue %= Number::from(360);
        hue /= Number::from(360);
        white /= Number::from(100);
        black /= Number::from(100);
        alpha = alpha.clamp(Number::zero(), Number::one());

        let white_black_sum = white.clone() + black.clone();

        if white_black_sum > Number::one() {
            white /= white_black_sum.clone();
            black /= white_black_sum;
        }

        let factor = Number::one() - white.clone() - black;

        fn channel(m1: Number, m2: Number, mut hue: Number) -> Number {
            if hue < Number::zero() {
                hue += Number::one();
            }

            if hue > Number::one() {
                hue -= Number::one();
            }

            if hue < Number::small_ratio(1, 6) {
                m1.clone() + (m2 - m1) * hue * Number::from(6)
            } else if hue < Number::small_ratio(1, 2) {
                m2
            } else if hue < Number::small_ratio(2, 3) {
                m1.clone() + (m2 - m1) * (Number::small_ratio(2, 3) - hue) * Number::from(6)
            } else {
                m1
            }
        }

        let to_rgb = |hue: Number| -> Number {
            let channel =
                channel(Number::zero(), Number::one(), hue) * factor.clone() + white.clone();
            channel * Number::from(255)
        };

        let red = to_rgb(hue.clone() + Number::small_ratio(1, 3));
        let green = to_rgb(hue.clone());
        let blue = to_rgb(hue - Number::small_ratio(1, 3));

        let repr = repr(&red, &green, &blue, &alpha);

        Color::new_rgba(red, green, blue, alpha, repr)
    }
}

/// Get the proper representation from RGBA values
fn repr(red: &Number, green: &Number, blue: &Number, alpha: &Number) -> String {
    fn into_u8(channel: &Number) -> u8 {
        if channel > &Number::from(255) {
            255_u8
        } else if channel.is_negative() {
            0_u8
        } else {
            channel.round().to_integer().to_u8().unwrap_or(255)
        }
    }

    let red_u8 = into_u8(red);
    let green_u8 = into_u8(green);
    let blue_u8 = into_u8(blue);

    if alpha < &Number::one() {
        format!(
            "rgba({}, {}, {}, {})",
            red_u8,
            green_u8,
            blue_u8,
            // todo: is_compressed
            alpha.inspect()
        )
    } else if let Some(c) = NAMED_COLORS.get_by_rgba([red_u8, green_u8, blue_u8]) {
        (*c).to_owned()
    } else {
        format!("#{:0>2x}{:0>2x}{:0>2x}", red_u8, green_u8, blue_u8)
    }
}

impl Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.repr)
    }
}
