use std::collections::BTreeMap;

use super::Builtin;
use crate::color::Color;
use crate::common::QuoteKind;
use crate::units::Unit;
use crate::value::{Number, Value};

macro_rules! opt_arg {
    ($args:ident, $name:ident, $arg:literal) => {
        let $name = match arg!($args, -1, $arg = Value::Null).eval() {
            Value::Dimension(n, Unit::None) => Some(n),
            Value::Dimension(n, Unit::Percent) => Some(n / Number::from(100)),
            Value::Null => None,
            _ => todo!(concat!("expected either unitless or % number for $", $arg)),
        };
    };
    (hsl: $args:ident, $name:ident, $arg:literal) => {
        let $name = match arg!($args, -1, $arg = Value::Null).eval() {
            Value::Dimension(n, Unit::None) | Value::Dimension(n, Unit::Percent) => {
                Some(n / Number::from(100))
            }
            Value::Null => None,
            _ => todo!(concat!("expected either unitless or % number for $", $arg)),
        };
    };
}

pub(crate) fn register(f: &mut BTreeMap<String, Builtin>) {
    decl!(f "change-color", |args, _| {
        let color = match arg!(args, 0, "color").eval() {
            Value::Color(c) => c,
            _ => todo!("non-color given to builtin function `change-color()`")
        };

        opt_arg!(args, alpha, "alpha");
        opt_arg!(args, red, "red");
        opt_arg!(args, green, "green");
        opt_arg!(args, blue, "blue");

        if red.is_some() || green.is_some() || blue.is_some() {
            return Ok(Value::Color(Color::from_rgba(red.unwrap_or(color.red()), green.unwrap_or(color.green()), blue.unwrap_or(color.blue()), alpha.unwrap_or(color.alpha()))))
        }

        let hue = match arg!(args, -1, "hue"=Value::Null).eval() {
            Value::Dimension(n, Unit::None)
            | Value::Dimension(n, Unit::Percent)
            | Value::Dimension(n, Unit::Deg) => Some(n),
            Value::Null => None,
            _ => todo!("expected either unitless or % number for hue"),
        };

        opt_arg!(hsl: args, saturation, "saturation");
        opt_arg!(hsl: args, luminance, "lightness");

        if hue.is_some() || saturation.is_some() || luminance.is_some() {
            // Color::as_hsla() returns more exact values than Color::hue(), etc.
            let (this_hue, this_saturation, this_luminance, this_alpha) = color.as_hsla();
            return Ok(Value::Color(Color::from_hsla(hue.unwrap_or(this_hue), saturation.unwrap_or(this_saturation), luminance.unwrap_or(this_luminance), alpha.unwrap_or(this_alpha))))
        }

        Ok(Value::Color(if let Some(a) = alpha {
            color.with_alpha(a)
        } else {
            color
        }))
    });
    decl!(f "adjust-color", |args, _| {
        let color = match arg!(args, 0, "color").eval() {
            Value::Color(c) => c.clone(),
            _ => todo!("non-color given to builtin function `adjust-color()`")
        };

        opt_arg!(args, alpha, "alpha");
        opt_arg!(args, red, "red");
        opt_arg!(args, green, "green");
        opt_arg!(args, blue, "blue");

        if red.is_some() || green.is_some() || blue.is_some() {
            return
            Ok(Value::Color(
                Color::from_rgba(
                    color.red() + red.unwrap_or(Number::from(0)),
                    color.green() + green.unwrap_or(Number::from(0)),
                    color.blue() + blue.unwrap_or(Number::from(0)),
                    color.alpha() + alpha.unwrap_or(Number::from(0))
                )
            ))
        }

        let hue = match arg!(args, -1, "hue"=Value::Null).eval() {
            Value::Dimension(n, Unit::None)
            | Value::Dimension(n, Unit::Percent)
            | Value::Dimension(n, Unit::Deg) => Some(n),
            Value::Null => None,
            _ => todo!("expected either unitless or % number for hue"),
        };

        opt_arg!(hsl: args, saturation, "saturation");
        opt_arg!(hsl: args, luminance, "lightness");

        if hue.is_some() || saturation.is_some() || luminance.is_some() {
            // Color::as_hsla() returns more exact values than Color::hue(), etc.
            let (this_hue, this_saturation, this_luminance, this_alpha) = color.as_hsla();
            return Ok(Value::Color(
                Color::from_hsla(
                    this_hue + hue.unwrap_or(Number::from(0)),
                    this_saturation + saturation.unwrap_or(Number::from(0)),
                    this_luminance + luminance.unwrap_or(Number::from(0)),
                    this_alpha + alpha.unwrap_or(Number::from(0))
                )
            ))
        }

        Ok(Value::Color(if let Some(a) = alpha {
            let temp_alpha = color.alpha();
            color.with_alpha(temp_alpha + a)
        } else {
            color
        }))
    });
    decl!(f "scale-color", |args, _| {
        let color = match arg!(args, 0, "color").eval() {
            Value::Color(c) => c.clone(),
            _ => todo!("non-color given to builtin function `scale-color()`")
        };

        opt_arg!(args, alpha, "alpha");
        opt_arg!(args, red, "red");
        opt_arg!(args, green, "green");
        opt_arg!(args, blue, "blue");

        if red.is_some() || green.is_some() || blue.is_some() {
            return
            Ok(Value::Color(
                Color::from_rgba(
                    scale(color.red(), red.unwrap_or(Number::from(0)), Number::from(255)),
                    scale(color.green(), green.unwrap_or(Number::from(0)), Number::from(255)),
                    scale(color.blue(), blue.unwrap_or(Number::from(0)), Number::from(255)),
                    scale(color.alpha(), alpha.unwrap_or(Number::from(0)), Number::from(1)),
                )
            ))
        }

        let hue = match arg!(args, -1, "hue"=Value::Null).eval() {
            Value::Dimension(n, Unit::None)
            | Value::Dimension(n, Unit::Percent)
            | Value::Dimension(n, Unit::Deg) => Some(n),
            Value::Null => None,
            _ => todo!("expected either unitless or % number for hue"),
        };

        opt_arg!(hsl: args, saturation, "saturation");
        opt_arg!(hsl: args, luminance, "lightness");

        if hue.is_some() || saturation.is_some() || luminance.is_some() {
            // Color::as_hsla() returns more exact values than Color::hue(), etc.
            let (this_hue, this_saturation, this_luminance, this_alpha) = color.as_hsla();
            return Ok(Value::Color(
                Color::from_hsla(
                    scale(this_hue, hue.unwrap_or(Number::from(0)), Number::from(360)),
                    scale(this_saturation, saturation.unwrap_or(Number::from(0)), Number::from(1)),
                    scale(this_luminance, luminance.unwrap_or(Number::from(0)), Number::from(1)),
                    scale(this_alpha, alpha.unwrap_or(Number::from(0)), Number::from(1)),
                )
            ))
        }

        Ok(Value::Color(if let Some(a) = alpha {
            let temp_alpha = color.alpha();
            color.with_alpha(scale(temp_alpha, a, Number::from(1)))
        } else {
            color
        }))
    });
    decl!(f "ie-hex-str", |args, _| {
        let color = match arg!(args, 0, "color").eval() {
            Value::Color(c) => c.clone(),
            _ => todo!("non-color given to builtin function `ie-hex-str()`")
        };
        Ok(Value::Ident(color.to_ie_hex_str(), QuoteKind::None))
    });
}

fn scale(val: Number, by: Number, max: Number) -> Number {
    if by == Number::from(0) {
        return val;
    }
    val.clone() + (if by > Number::from(0) { max - val } else { val }) * by
}
