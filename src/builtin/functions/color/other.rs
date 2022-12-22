use crate::builtin::builtin_imports::*;

macro_rules! opt_rgba {
    ($args:ident, $name:ident, $arg:literal, $low:literal, $high:literal) => {
        let $name = match $args.default_named_arg($arg, Value::Null) {
            Value::Dimension { num: n, .. } if n.is_nan() => todo!(),
            Value::Dimension {
                num: n, unit: u, ..
            } => Some(bound!($args, $arg, n, u, $low, $high)),
            Value::Null => None,
            v => {
                return Err((
                    format!("${}: {} is not a number.", $arg, v.inspect($args.span())?),
                    $args.span(),
                )
                    .into())
            }
        };
    };
}

macro_rules! opt_hsl {
    ($args:ident, $name:ident, $arg:literal, $low:literal, $high:literal) => {
        let $name = match $args.default_named_arg($arg, Value::Null) {
            Value::Dimension { num: n, .. } if n.is_nan() => todo!(),
            Value::Dimension {
                num: n, unit: u, ..
            } => Some(bound!($args, $arg, n, u, $low, $high) / Number::from(100)),
            Value::Null => None,
            v => {
                return Err((
                    format!("${}: {} is not a number.", $arg, v.inspect($args.span())?),
                    $args.span(),
                )
                    .into())
            }
        };
    };
}

pub(crate) fn change_color(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    if args.get_positional(1).is_some() {
        return Err((
            "Only one positional argument is allowed. All other arguments must be passed by name.",
            args.span(),
        )
            .into());
    }

    let color = match args.get_err(0, "color")? {
        Value::Color(c) => c,
        v => {
            return Err((
                format!("$color: {} is not a color.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    opt_rgba!(args, alpha, "alpha", 0, 1);
    opt_rgba!(args, red, "red", 0, 255);
    opt_rgba!(args, green, "green", 0, 255);
    opt_rgba!(args, blue, "blue", 0, 255);

    if red.is_some() || green.is_some() || blue.is_some() {
        return Ok(Value::Color(Box::new(Color::from_rgba(
            red.unwrap_or_else(|| color.red()),
            green.unwrap_or_else(|| color.green()),
            blue.unwrap_or_else(|| color.blue()),
            alpha.unwrap_or_else(|| color.alpha()),
        ))));
    }

    let hue = match args.default_named_arg("hue", Value::Null) {
        Value::Dimension { num: n, .. } if n.is_nan() => todo!(),
        Value::Dimension { num: n, .. } => Some(n),
        Value::Null => None,
        v => {
            return Err((
                format!("$hue: {} is not a number.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    opt_hsl!(args, saturation, "saturation", 0, 100);
    opt_hsl!(args, luminance, "lightness", 0, 100);

    if hue.is_some() || saturation.is_some() || luminance.is_some() {
        // Color::as_hsla() returns more exact values than Color::hue(), etc.
        let (this_hue, this_saturation, this_luminance, this_alpha) = color.as_hsla();
        return Ok(Value::Color(Box::new(Color::from_hsla(
            hue.unwrap_or(this_hue),
            saturation.unwrap_or(this_saturation),
            luminance.unwrap_or(this_luminance),
            alpha.unwrap_or(this_alpha),
        ))));
    }

    Ok(Value::Color(if let Some(a) = alpha {
        Box::new(color.with_alpha(a))
    } else {
        color
    }))
}

pub(crate) fn adjust_color(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    let color = match args.get_err(0, "color")? {
        Value::Color(c) => c,
        v => {
            return Err((
                format!("$color: {} is not a color.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    opt_rgba!(args, alpha, "alpha", -1, 1);
    opt_rgba!(args, red, "red", -255, 255);
    opt_rgba!(args, green, "green", -255, 255);
    opt_rgba!(args, blue, "blue", -255, 255);

    if red.is_some() || green.is_some() || blue.is_some() {
        return Ok(Value::Color(Box::new(Color::from_rgba(
            color.red() + red.unwrap_or_else(Number::zero),
            color.green() + green.unwrap_or_else(Number::zero),
            color.blue() + blue.unwrap_or_else(Number::zero),
            color.alpha() + alpha.unwrap_or_else(Number::zero),
        ))));
    }

    let hue = match args.default_named_arg("hue", Value::Null) {
        Value::Dimension { num: n, .. } if n.is_nan() => todo!(),
        Value::Dimension { num: n, .. } => Some(n),
        Value::Null => None,
        v => {
            return Err((
                format!("$hue: {} is not a number.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    opt_hsl!(args, saturation, "saturation", -100, 100);
    opt_hsl!(args, luminance, "lightness", -100, 100);

    if hue.is_some() || saturation.is_some() || luminance.is_some() {
        // Color::as_hsla() returns more exact values than Color::hue(), etc.
        let (this_hue, this_saturation, this_luminance, this_alpha) = color.as_hsla();
        return Ok(Value::Color(Box::new(Color::from_hsla(
            this_hue + hue.unwrap_or_else(Number::zero),
            this_saturation + saturation.unwrap_or_else(Number::zero),
            this_luminance + luminance.unwrap_or_else(Number::zero),
            this_alpha + alpha.unwrap_or_else(Number::zero),
        ))));
    }

    Ok(Value::Color(if let Some(a) = alpha {
        let temp_alpha = color.alpha();
        Box::new(color.with_alpha(temp_alpha + a))
    } else {
        color
    }))
}

#[allow(clippy::cognitive_complexity)]
// todo: refactor into rgb and hsl?
pub(crate) fn scale_color(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    pub(crate) fn scale(val: Number, by: Number, max: Number) -> Number {
        if by.is_zero() {
            return val;
        }
        val.clone() + (if by.is_positive() { max - val } else { val }) * by
    }

    let span = args.span();
    let color = match args.get_err(0, "color")? {
        Value::Color(c) => c,
        v => {
            return Err((
                format!("$color: {} is not a color.", v.inspect(span)?),
                span,
            )
                .into())
        }
    };

    macro_rules! opt_scale_arg {
        ($args:ident, $name:ident, $arg:literal, $low:literal, $high:literal) => {
            let $name = match $args.default_named_arg($arg, Value::Null) {
                Value::Dimension { num: n, .. } if n.is_nan() => todo!(),
                Value::Dimension {
                    num: n,
                    unit: Unit::Percent,
                    ..
                } => Some(bound!($args, $arg, n, Unit::Percent, $low, $high) / Number::from(100)),
                v @ Value::Dimension { .. } => {
                    return Err((
                        format!(
                            "${}: Expected {} to have unit \"%\".",
                            $arg,
                            v.inspect($args.span())?
                        ),
                        $args.span(),
                    )
                        .into())
                }
                Value::Null => None,
                v => {
                    return Err((
                        format!("${}: {} is not a number.", $arg, v.inspect($args.span())?),
                        $args.span(),
                    )
                        .into())
                }
            };
        };
    }

    opt_scale_arg!(args, alpha, "alpha", -100, 100);
    opt_scale_arg!(args, red, "red", -100, 100);
    opt_scale_arg!(args, green, "green", -100, 100);
    opt_scale_arg!(args, blue, "blue", -100, 100);

    if red.is_some() || green.is_some() || blue.is_some() {
        return Ok(Value::Color(Box::new(Color::from_rgba(
            scale(
                color.red(),
                red.unwrap_or_else(Number::zero),
                Number::from(255),
            ),
            scale(
                color.green(),
                green.unwrap_or_else(Number::zero),
                Number::from(255),
            ),
            scale(
                color.blue(),
                blue.unwrap_or_else(Number::zero),
                Number::from(255),
            ),
            scale(
                color.alpha(),
                alpha.unwrap_or_else(Number::zero),
                Number::one(),
            ),
        ))));
    }

    opt_scale_arg!(args, saturation, "saturation", -100, 100);
    opt_scale_arg!(args, luminance, "lightness", -100, 100);

    if saturation.is_some() || luminance.is_some() {
        // Color::as_hsla() returns more exact values than Color::hue(), etc.
        let (this_hue, this_saturation, this_luminance, this_alpha) = color.as_hsla();
        return Ok(Value::Color(Box::new(Color::from_hsla(
            scale(this_hue, Number::zero(), Number::from(360)),
            scale(
                this_saturation,
                saturation.unwrap_or_else(Number::zero),
                Number::one(),
            ),
            scale(
                this_luminance,
                luminance.unwrap_or_else(Number::zero),
                Number::one(),
            ),
            scale(
                this_alpha,
                alpha.unwrap_or_else(Number::zero),
                Number::one(),
            ),
        ))));
    }

    Ok(Value::Color(if let Some(a) = alpha {
        let temp_alpha = color.alpha();
        Box::new(color.with_alpha(scale(temp_alpha, a, Number::one())))
    } else {
        color
    }))
}

pub(crate) fn ie_hex_str(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    let color = match args.get_err(0, "color")? {
        Value::Color(c) => c,
        v => {
            return Err((
                format!("$color: {} is not a color.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::String(color.to_ie_hex_str(), QuoteKind::None))
}

pub(crate) fn declare(f: &mut GlobalFunctionMap) {
    f.insert("change-color", Builtin::new(change_color));
    f.insert("adjust-color", Builtin::new(adjust_color));
    f.insert("scale-color", Builtin::new(scale_color));
    f.insert("ie-hex-str", Builtin::new(ie_hex_str));
}
