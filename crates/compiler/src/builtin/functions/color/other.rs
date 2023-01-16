use crate::builtin::builtin_imports::*;

macro_rules! opt_rgba {
    ($args:ident, $name:ident, $arg:literal, $low:literal, $high:literal) => {
        let $name = match $args.default_named_arg($arg, Value::Null) {
            Value::Dimension(SassNumber { num: n, .. }) if n.is_nan() => todo!(),
            Value::Dimension(SassNumber {
                num: n, unit: u, ..
            }) => Some(bound!($args, $arg, n, u, $low, $high)),
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
            Value::Dimension(SassNumber { num: n, .. }) if n.is_nan() => todo!(),
            Value::Dimension(SassNumber {
                num: n, unit: u, ..
            }) => Some(bound!($args, $arg, n, u, $low, $high) / Number(100.0)),
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

pub(crate) fn change_color(mut args: ArgumentResult, visitor: &mut Visitor) -> SassResult<Value> {
    if args.get_positional(1).is_some() {
        return Err((
            "Only one positional argument is allowed. All other arguments must be passed by name.",
            args.span(),
        )
            .into());
    }

    let color = args
        .get_err(0, "color")?
        .assert_color_with_name("color", args.span())?;

    opt_rgba!(args, alpha, "alpha", 0, 1);
    opt_rgba!(args, red, "red", 0, 255);
    opt_rgba!(args, green, "green", 0, 255);
    opt_rgba!(args, blue, "blue", 0, 255);

    if red.is_some() || green.is_some() || blue.is_some() {
        return Ok(Value::Color(std::rc::Rc::new(Color::from_rgba(
            red.unwrap_or_else(|| color.red()),
            green.unwrap_or_else(|| color.green()),
            blue.unwrap_or_else(|| color.blue()),
            alpha.unwrap_or_else(|| color.alpha()),
        ))));
    }

    let hue = match args.default_named_arg("hue", Value::Null) {
        Value::Dimension(SassNumber { num: n, .. }) if n.is_nan() => todo!(),
        Value::Dimension(SassNumber { num: n, .. }) => Some(n),
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
    opt_hsl!(args, lightness, "lightness", 0, 100);

    if hue.is_some() || saturation.is_some() || lightness.is_some() {
        // Color::as_hsla() returns more exact values than Color::hue(), etc.
        let (this_hue, this_saturation, this_lightness, this_alpha) = color.as_hsla();
        return Ok(Value::Color(std::rc::Rc::new(Color::from_hsla(
            hue.unwrap_or(this_hue),
            saturation.unwrap_or(this_saturation),
            lightness.unwrap_or(this_lightness),
            alpha.unwrap_or(this_alpha),
        ))));
    }

    Ok(Value::Color(if let Some(a) = alpha {
        std::rc::Rc::new(color.with_alpha(a))
    } else {
        color
    }))
}

pub(crate) fn adjust_color(mut args: ArgumentResult, visitor: &mut Visitor) -> SassResult<Value> {
    let color = args
        .get_err(0, "color")?
        .assert_color_with_name("color", args.span())?;

    opt_rgba!(args, alpha, "alpha", -1, 1);
    opt_rgba!(args, red, "red", -255, 255);
    opt_rgba!(args, green, "green", -255, 255);
    opt_rgba!(args, blue, "blue", -255, 255);

    if red.is_some() || green.is_some() || blue.is_some() {
        return Ok(Value::Color(std::rc::Rc::new(Color::from_rgba(
            color.red() + red.unwrap_or_else(Number::zero),
            color.green() + green.unwrap_or_else(Number::zero),
            color.blue() + blue.unwrap_or_else(Number::zero),
            color.alpha() + alpha.unwrap_or_else(Number::zero),
        ))));
    }

    let hue = match args.default_named_arg("hue", Value::Null) {
        Value::Dimension(SassNumber { num: n, .. }) if n.is_nan() => todo!(),
        Value::Dimension(SassNumber { num: n, .. }) => Some(n),
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
    opt_hsl!(args, lightness, "lightness", -100, 100);

    if hue.is_some() || saturation.is_some() || lightness.is_some() {
        // Color::as_hsla() returns more exact values than Color::hue(), etc.
        let (this_hue, this_saturation, this_lightness, this_alpha) = color.as_hsla();
        return Ok(Value::Color(std::rc::Rc::new(Color::from_hsla(
            this_hue + hue.unwrap_or_else(Number::zero),
            this_saturation + saturation.unwrap_or_else(Number::zero),
            this_lightness + lightness.unwrap_or_else(Number::zero),
            this_alpha + alpha.unwrap_or_else(Number::zero),
        ))));
    }

    Ok(Value::Color(if let Some(a) = alpha {
        let temp_alpha = color.alpha();
        std::rc::Rc::new(color.with_alpha(temp_alpha + a))
    } else {
        color
    }))
}

#[allow(clippy::cognitive_complexity)]
// todo: refactor into rgb and hsl?
pub(crate) fn scale_color(mut args: ArgumentResult, visitor: &mut Visitor) -> SassResult<Value> {
    pub(crate) fn scale(val: Number, by: Number, max: Number) -> Number {
        if by.is_zero() {
            return val;
        }
        val + (if by.is_positive() { max - val } else { val }) * by
    }

    fn check_num(num: Spanned<Value>, name: &str, min: f64, max: f64) -> SassResult<Number> {
        let span = num.span;
        let mut num = num.node.assert_number_with_name(name, span)?;

        num.assert_unit(&Unit::Percent, name, span)?;
        num.assert_bounds(name, min, max, span)?;

        num.num /= Number(100.0);

        Ok(num.num)
    }

    fn get_arg(
        args: &mut ArgumentResult,
        name: &str,
        min: f64,
        max: f64,
    ) -> SassResult<Option<Number>> {
        Ok(match args.get(usize::MAX, name) {
            Some(v) => Some(check_num(v, name, min, max)?),
            None => None,
        })
    }

    let span = args.span();
    let color = args
        .get_err(0, "color")?
        .assert_color_with_name("color", args.span())?;

    let red = get_arg(&mut args, "red", -100.0, 100.0)?;
    let green = get_arg(&mut args, "green", -100.0, 100.0)?;
    let blue = get_arg(&mut args, "blue", -100.0, 100.0)?;
    let alpha = get_arg(&mut args, "alpha", -100.0, 100.0)?;

    if red.is_some() || green.is_some() || blue.is_some() {
        return Ok(Value::Color(std::rc::Rc::new(Color::from_rgba(
            scale(color.red(), red.unwrap_or_else(Number::zero), Number(255.0)),
            scale(
                color.green(),
                green.unwrap_or_else(Number::zero),
                Number(255.0),
            ),
            scale(
                color.blue(),
                blue.unwrap_or_else(Number::zero),
                Number(255.0),
            ),
            scale(
                color.alpha(),
                alpha.unwrap_or_else(Number::zero),
                Number::one(),
            ),
        ))));
    }

    let saturation = get_arg(&mut args, "saturation", -100.0, 100.0)?;
    let lightness = get_arg(&mut args, "lightness", -100.0, 100.0)?;

    if saturation.is_some() || lightness.is_some() {
        // Color::as_hsla() returns more exact values than Color::hue(), etc.
        let (this_hue, this_saturation, this_lightness, this_alpha) = color.as_hsla();
        return Ok(Value::Color(std::rc::Rc::new(Color::from_hsla(
            scale(this_hue, Number::zero(), Number(360.0)),
            scale(
                this_saturation,
                saturation.unwrap_or_else(Number::zero),
                Number::one(),
            ),
            scale(
                this_lightness,
                lightness.unwrap_or_else(Number::zero),
                Number::one(),
            ),
            scale(
                this_alpha,
                alpha.unwrap_or_else(Number::zero),
                Number::one(),
            ),
        ))));
    }

    let whiteness = get_arg(&mut args, "whiteness", -100.0, 100.0)?;
    let blackness = get_arg(&mut args, "blackness", -100.0, 100.0)?;

    if whiteness.is_some() || blackness.is_some() {
        let this_hue = color.hue();
        let this_whiteness = color.whiteness() * Number(100.0);
        let this_blackness = color.blackness() * Number(100.0);

        return Ok(Value::Color(std::rc::Rc::new(Color::from_hwb(
            scale(this_hue, Number::zero(), Number(360.0)),
            scale(
                this_whiteness,
                whiteness.unwrap_or_else(Number::zero),
                Number(100.0),
            ),
            scale(
                this_blackness,
                blackness.unwrap_or_else(Number::zero),
                Number(100.0),
            ),
            scale(
                color.alpha(),
                alpha.unwrap_or_else(Number::zero),
                Number::one(),
            ),
        ))));
    }

    Ok(Value::Color(if let Some(a) = alpha {
        let temp_alpha = color.alpha();
        std::rc::Rc::new(color.with_alpha(scale(temp_alpha, a, Number::one())))
    } else {
        color
    }))
}

pub(crate) fn ie_hex_str(mut args: ArgumentResult, visitor: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    let color = args
        .get_err(0, "color")?
        .assert_color_with_name("color", args.span())?;
    Ok(Value::String(color.to_ie_hex_str(), QuoteKind::None))
}

pub(crate) fn declare(f: &mut GlobalFunctionMap) {
    f.insert("change-color", Builtin::new(change_color));
    f.insert("adjust-color", Builtin::new(adjust_color));
    f.insert("scale-color", Builtin::new(scale_color));
    f.insert("ie-hex-str", Builtin::new(ie_hex_str));
}
