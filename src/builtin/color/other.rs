use std::collections::BTreeMap;

use super::Builtin;
use crate::color::Color;
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
            return Some(Value::Color(Color::from_rgba(red.unwrap_or(color.red()), green.unwrap_or(color.green()), blue.unwrap_or(color.blue()), alpha.unwrap_or(color.alpha()))))
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
            return Some(Value::Color(Color::from_hsla(hue.unwrap_or(this_hue), saturation.unwrap_or(this_saturation), luminance.unwrap_or(this_luminance), alpha.unwrap_or(this_alpha))))
        }

        Some(Value::Color(if let Some(a) = alpha {
            color.with_alpha(a)
        } else {
            color
        }))
    });
}
