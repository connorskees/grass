use std::collections::HashMap;

use num_traits::{One, Signed, Zero};

use super::Builtin;
use crate::color::Color;
use crate::common::QuoteKind;
use crate::unit::Unit;
use crate::value::{Number, Value};

macro_rules! opt_rgba {
    ($args:ident, $name:ident, $arg:literal, $low:literal, $high:literal, $scope:ident, $super_selector:ident) => {
        let $name = match named_arg!($args, $scope, $super_selector, $arg = Value::Null) {
            Value::Dimension(n, u) => Some(bound!($args, $arg, n, u, $low, $high)),
            Value::Null => None,
            v => {
                return Err((
                    format!(
                        "${}: {} is not a number.",
                        $arg,
                        v.to_css_string($args.span())?
                    ),
                    $args.span(),
                )
                    .into())
            }
        };
    };
}

macro_rules! opt_hsl {
    ($args:ident, $name:ident, $arg:literal, $low:literal, $high:literal, $scope:ident, $super_selector:ident) => {
        let $name = match named_arg!($args, $scope, $super_selector, $arg = Value::Null) {
            Value::Dimension(n, u) => {
                Some(bound!($args, $arg, n, u, $low, $high) / Number::from(100))
            }
            Value::Null => None,
            v => {
                return Err((
                    format!(
                        "${}: {} is not a number.",
                        $arg,
                        v.to_css_string($args.span())?
                    ),
                    $args.span(),
                )
                    .into())
            }
        };
    };
}

pub(crate) fn register(f: &mut HashMap<String, Builtin>) {
    f.insert("change-color".to_owned(), Builtin::new(|mut args, scope, super_selector| {
        if args.get_positional(1, scope, super_selector).is_some() {
            return Err(
                ("Only one positional argument is allowed. All other arguments must be passed by name.", args.span()
            ).into());
        }

        let color = match arg!(args, scope, super_selector, 0, "color") {
            Value::Color(c) => c,
            v => return Err(
                (format!("$color: {} is not a color.", v.to_css_string(args.span())?), args.span()
            ).into()),
        };

        opt_rgba!(args, alpha, "alpha", 0, 1, scope, super_selector);
        opt_rgba!(args, red, "red", 0, 255, scope, super_selector);
        opt_rgba!(args, green, "green", 0, 255, scope, super_selector);
        opt_rgba!(args, blue, "blue", 0, 255, scope, super_selector);

        if red.is_some() || green.is_some() || blue.is_some() {
            return Ok(Value::Color(Box::new(Color::from_rgba(red.unwrap_or(color.red()), green.unwrap_or(color.green()), blue.unwrap_or(color.blue()), alpha.unwrap_or(color.alpha())))))
        }

        let hue = match named_arg!(args, scope, super_selector, "hue"=Value::Null) {
            Value::Dimension(n, _) => Some(n),
            Value::Null => None,
            v => return Err(
                (format!("$hue: {} is not a number.", v.to_css_string(args.span())?), args.span()
            ).into()),
        };

        opt_hsl!(args, saturation, "saturation", 0, 100, scope, super_selector);
        opt_hsl!(args, luminance, "lightness", 0, 100, scope, super_selector);

        if hue.is_some() || saturation.is_some() || luminance.is_some() {
            // Color::as_hsla() returns more exact values than Color::hue(), etc.
            let (this_hue, this_saturation, this_luminance, this_alpha) = color.as_hsla();
            return Ok(Value::Color(Box::new(Color::from_hsla(hue.unwrap_or(this_hue), saturation.unwrap_or(this_saturation), luminance.unwrap_or(this_luminance), alpha.unwrap_or(this_alpha)))))
        }

        Ok(Value::Color(if let Some(a) = alpha {
            Box::new(color.with_alpha(a))
        } else {
            color
        }))
    }));
    f.insert(
        "adjust-color".to_owned(),
        Builtin::new(|mut args, scope, super_selector| {
            let color = match arg!(args, scope, super_selector, 0, "color") {
                Value::Color(c) => c,
                v => {
                    return Err((
                        format!("$color: {} is not a color.", v.to_css_string(args.span())?),
                        args.span(),
                    )
                        .into())
                }
            };

            opt_rgba!(args, alpha, "alpha", -1, 1, scope, super_selector);
            opt_rgba!(args, red, "red", -255, 255, scope, super_selector);
            opt_rgba!(args, green, "green", -255, 255, scope, super_selector);
            opt_rgba!(args, blue, "blue", -255, 255, scope, super_selector);

            if red.is_some() || green.is_some() || blue.is_some() {
                return Ok(Value::Color(Box::new(Color::from_rgba(
                    color.red() + red.unwrap_or(Number::zero()),
                    color.green() + green.unwrap_or(Number::zero()),
                    color.blue() + blue.unwrap_or(Number::zero()),
                    color.alpha() + alpha.unwrap_or(Number::zero()),
                ))));
            }

            let hue = match named_arg!(args, scope, super_selector, "hue" = Value::Null) {
                Value::Dimension(n, _) => Some(n),
                Value::Null => None,
                v => {
                    return Err((
                        format!("$hue: {} is not a number.", v.to_css_string(args.span())?),
                        args.span(),
                    )
                        .into())
                }
            };

            opt_hsl!(
                args,
                saturation,
                "saturation",
                -100,
                100,
                scope,
                super_selector
            );
            opt_hsl!(
                args,
                luminance,
                "lightness",
                -100,
                100,
                scope,
                super_selector
            );

            if hue.is_some() || saturation.is_some() || luminance.is_some() {
                // Color::as_hsla() returns more exact values than Color::hue(), etc.
                let (this_hue, this_saturation, this_luminance, this_alpha) = color.as_hsla();
                return Ok(Value::Color(Box::new(Color::from_hsla(
                    this_hue + hue.unwrap_or(Number::zero()),
                    this_saturation + saturation.unwrap_or(Number::zero()),
                    this_luminance + luminance.unwrap_or(Number::zero()),
                    this_alpha + alpha.unwrap_or(Number::zero()),
                ))));
            }

            Ok(Value::Color(if let Some(a) = alpha {
                let temp_alpha = color.alpha();
                Box::new(color.with_alpha(temp_alpha + a))
            } else {
                color
            }))
        }),
    );
    f.insert(
        "scale-color".to_owned(),
        Builtin::new(|mut args, scope, super_selector| {
            let span = args.span();
            let color = match arg!(args, scope, super_selector, 0, "color") {
                Value::Color(c) => c,
                v => return Err(
                    (format!("$color: {} is not a color.", v.to_css_string(span)?), span
                ).into()),
            };

            macro_rules! opt_scale_arg {
                ($args:ident, $name:ident, $arg:literal, $low:literal, $high:literal, $scope:ident, $super_selector:ident) => {
                    let $name = match named_arg!($args, $scope, $super_selector, $arg = Value::Null) {
                        Value::Dimension(n, Unit::Percent) => {
                            Some(bound!($args, $arg, n, Unit::Percent, $low, $high) / Number::from(100))
                        }
                        v @ Value::Dimension(..) => {
                            return Err(
                                (format!("${}: Expected {} to have unit \"%\".", $arg, v.to_css_string($args.span())?), $args.span()).into()
                            )
                        }
                        Value::Null => None,
                        v => return Err(
                            (format!("${}: {} is not a number.", $arg, v.to_css_string($args.span())?), $args.span()
                        ).into()),
                    };
                };
            }

            opt_scale_arg!(args, alpha, "alpha", -100, 100, scope, super_selector);
            opt_scale_arg!(args, red, "red", -100, 100, scope, super_selector);
            opt_scale_arg!(args, green, "green", -100, 100, scope, super_selector);
            opt_scale_arg!(args, blue, "blue", -100, 100, scope, super_selector);

            if red.is_some() || green.is_some() || blue.is_some() {
                return Ok(Value::Color(Box::new(Color::from_rgba(
                    scale(
                        color.red(),
                        red.unwrap_or(Number::zero()),
                        Number::from(255),
                    ),
                    scale(
                        color.green(),
                        green.unwrap_or(Number::zero()),
                        Number::from(255),
                    ),
                    scale(
                        color.blue(),
                        blue.unwrap_or(Number::zero()),
                        Number::from(255),
                    ),
                    scale(
                        color.alpha(),
                        alpha.unwrap_or(Number::zero()),
                        Number::one(),
                    ),
                ))));
            }

            opt_scale_arg!(args, saturation, "saturation", -100, 100, scope, super_selector);
            opt_scale_arg!(args, luminance, "lightness", -100, 100, scope, super_selector);

            if saturation.is_some() || luminance.is_some() {
                // Color::as_hsla() returns more exact values than Color::hue(), etc.
                let (this_hue, this_saturation, this_luminance, this_alpha) = color.as_hsla();
                return Ok(Value::Color(Box::new(Color::from_hsla(
                    scale(this_hue, Number::zero(), Number::from(360)),
                    scale(
                        this_saturation,
                        saturation.unwrap_or(Number::zero()),
                        Number::one(),
                    ),
                    scale(
                        this_luminance,
                        luminance.unwrap_or(Number::zero()),
                        Number::one(),
                    ),
                    scale(this_alpha, alpha.unwrap_or(Number::zero()), Number::one()),
                ))));
            }

            Ok(Value::Color(if let Some(a) = alpha {
                let temp_alpha = color.alpha();
                Box::new(color.with_alpha(scale(temp_alpha, a, Number::one())))
            } else {
                color
            }))
        }),
    );
    f.insert(
        "ie-hex-str".to_owned(),
        Builtin::new(|mut args, scope, super_selector| {
            max_args!(args, 1);
            let color = match arg!(args, scope, super_selector, 0, "color") {
                Value::Color(c) => c,
                v => {
                    return Err((
                        format!("$color: {} is not a color.", v.to_css_string(args.span())?),
                        args.span(),
                    )
                        .into())
                }
            };
            Ok(Value::Ident(color.to_ie_hex_str(), QuoteKind::None))
        }),
    );
}

fn scale(val: Number, by: Number, max: Number) -> Number {
    if by.is_zero() {
        return val;
    }
    val.clone() + (if by.is_positive() { max - val } else { val }) * by
}
