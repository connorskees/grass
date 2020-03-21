use std::collections::HashMap;

use super::Builtin;
use crate::color::Color;
use crate::common::QuoteKind;
use crate::unit::Unit;
use crate::value::{Number, Value};

macro_rules! opt_rgba {
    ($args:ident, $name:ident, $arg:literal, $low:literal, $high:literal) => {
        let x = $low;
        let $name = match arg!($args, -1, $arg = Value::Null) {
            Value::Dimension(n, u) => Some(bound!($arg, n, u, x, $high)),
            Value::Null => None,
            v => return Err(format!("${}: {} is not a number.", $arg, v).into()),
        };
    };
}

macro_rules! opt_hsl {
    ($args:ident, $name:ident, $arg:literal, $low:literal, $high:literal) => {
        let x = $low;
        let $name = match arg!($args, -1, $arg = Value::Null) {
            Value::Dimension(n, u) => Some(bound!($arg, n, u, x, $high) / Number::from(100)),
            Value::Null => None,
            v => return Err(format!("${}: {} is not a number.", $arg, v).into()),
        };
    };
}

pub(crate) fn register(f: &mut HashMap<String, Builtin>) {
    f.insert("change-color".to_owned(), Box::new(|args, _| {
        if args.get("1").is_some() {
            return Err("Only one positional argument is allowed. All other arguments must be passed by name.".into());
        }

        let color = match arg!(args, 0, "color") {
            Value::Color(c) => c,
            v => return Err(format!("$color: {} is not a color.", v).into()),
        };

        opt_rgba!(args, alpha, "alpha", 0, 1);
        opt_rgba!(args, red, "red", 0, 255);
        opt_rgba!(args, green, "green", 0, 255);
        opt_rgba!(args, blue, "blue", 0, 255);

        if red.is_some() || green.is_some() || blue.is_some() {
            return Ok(Value::Color(Color::from_rgba(red.unwrap_or(color.red()), green.unwrap_or(color.green()), blue.unwrap_or(color.blue()), alpha.unwrap_or(color.alpha()))))
        }

        let hue = match arg!(args, -1, "hue"=Value::Null) {
            Value::Dimension(n, _) => Some(n),
            Value::Null => None,
            v => return Err(format!("$hue: {} is not a number.", v).into()),
        };

        opt_hsl!(args, saturation, "saturation", 0, 100);
        opt_hsl!(args, luminance, "lightness", 0, 100);

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
    }));
    f.insert(
        "adjust-color".to_owned(),
        Box::new(|args, _| {
            let color = match arg!(args, 0, "color") {
                Value::Color(c) => c,
                v => return Err(format!("$color: {} is not a color.", v).into()),
            };

            opt_rgba!(args, alpha, "alpha", -1, 1);
            opt_rgba!(args, red, "red", -255, 255);
            opt_rgba!(args, green, "green", -255, 255);
            opt_rgba!(args, blue, "blue", -255, 255);

            if red.is_some() || green.is_some() || blue.is_some() {
                return Ok(Value::Color(Color::from_rgba(
                    color.red() + red.unwrap_or(Number::from(0)),
                    color.green() + green.unwrap_or(Number::from(0)),
                    color.blue() + blue.unwrap_or(Number::from(0)),
                    color.alpha() + alpha.unwrap_or(Number::from(0)),
                )));
            }

            let hue = match arg!(args, -1, "hue" = Value::Null) {
                Value::Dimension(n, _) => Some(n),
                Value::Null => None,
                v => return Err(format!("$hue: {} is not a number.", v).into()),
            };

            opt_hsl!(args, saturation, "saturation", -100, 100);
            opt_hsl!(args, luminance, "lightness", -100, 100);

            if hue.is_some() || saturation.is_some() || luminance.is_some() {
                // Color::as_hsla() returns more exact values than Color::hue(), etc.
                let (this_hue, this_saturation, this_luminance, this_alpha) = color.as_hsla();
                return Ok(Value::Color(Color::from_hsla(
                    this_hue + hue.unwrap_or(Number::from(0)),
                    this_saturation + saturation.unwrap_or(Number::from(0)),
                    this_luminance + luminance.unwrap_or(Number::from(0)),
                    this_alpha + alpha.unwrap_or(Number::from(0)),
                )));
            }

            Ok(Value::Color(if let Some(a) = alpha {
                let temp_alpha = color.alpha();
                color.with_alpha(temp_alpha + a)
            } else {
                color
            }))
        }),
    );
    f.insert(
        "scale-color".to_owned(),
        Box::new(|args, _| {
            let color = match arg!(args, 0, "color") {
                Value::Color(c) => c,
                v => return Err(format!("$color: {} is not a color.", v).into()),
            };

            macro_rules! opt_scale_arg {
                ($args:ident, $name:ident, $arg:literal, $low:literal, $high:literal) => {
                    let x = $low;
                    let $name = match arg!($args, -1, $arg = Value::Null) {
                        Value::Dimension(n, Unit::Percent) => {
                            Some(bound!($arg, n, Unit::Percent, x, $high) / Number::from(100))
                        }
                        v @ Value::Dimension(..) => {
                            return Err(
                                format!("${}: Expected {} to have unit \"%\".", $arg, v).into()
                            )
                        }
                        Value::Null => None,
                        v => return Err(format!("${}: {} is not a number.", $arg, v).into()),
                    };
                };
            }

            opt_scale_arg!(args, alpha, "alpha", -100, 100);
            opt_scale_arg!(args, red, "red", -100, 100);
            opt_scale_arg!(args, green, "green", -100, 100);
            opt_scale_arg!(args, blue, "blue", -100, 100);

            if red.is_some() || green.is_some() || blue.is_some() {
                return Ok(Value::Color(Color::from_rgba(
                    scale(
                        color.red(),
                        red.unwrap_or(Number::from(0)),
                        Number::from(255),
                    ),
                    scale(
                        color.green(),
                        green.unwrap_or(Number::from(0)),
                        Number::from(255),
                    ),
                    scale(
                        color.blue(),
                        blue.unwrap_or(Number::from(0)),
                        Number::from(255),
                    ),
                    scale(
                        color.alpha(),
                        alpha.unwrap_or(Number::from(0)),
                        Number::from(1),
                    ),
                )));
            }

            opt_scale_arg!(args, saturation, "saturation", -100, 100);
            opt_scale_arg!(args, luminance, "lightness", -100, 100);

            if saturation.is_some() || luminance.is_some() {
                // Color::as_hsla() returns more exact values than Color::hue(), etc.
                let (this_hue, this_saturation, this_luminance, this_alpha) = color.as_hsla();
                return Ok(Value::Color(Color::from_hsla(
                    scale(this_hue, Number::from(0), Number::from(360)),
                    scale(
                        this_saturation,
                        saturation.unwrap_or(Number::from(0)),
                        Number::from(1),
                    ),
                    scale(
                        this_luminance,
                        luminance.unwrap_or(Number::from(0)),
                        Number::from(1),
                    ),
                    scale(
                        this_alpha,
                        alpha.unwrap_or(Number::from(0)),
                        Number::from(1),
                    ),
                )));
            }

            Ok(Value::Color(if let Some(a) = alpha {
                let temp_alpha = color.alpha();
                color.with_alpha(scale(temp_alpha, a, Number::from(1)))
            } else {
                color
            }))
        }),
    );
    f.insert(
        "ie-hex-str".to_owned(),
        Box::new(|args, _| {
            max_args!(args, 1);
            let color = match arg!(args, 0, "color") {
                Value::Color(c) => c,
                v => return Err(format!("$color: {} is not a color.", v).into()),
            };
            Ok(Value::Ident(color.to_ie_hex_str(), QuoteKind::None))
        }),
    );
}

fn scale(val: Number, by: Number, max: Number) -> Number {
    if by == Number::from(0) {
        return val;
    }
    val.clone() + (if by > Number::from(0) { max - val } else { val }) * by
}
