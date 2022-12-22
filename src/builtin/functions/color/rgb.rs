use crate::{builtin::builtin_imports::*, serializer::inspect_number, value::fuzzy_round};

fn function_string(
    name: &'static str,
    args: &[Value],
    visitor: &mut Visitor,
    span: Span,
) -> SassResult<String> {
    let args = args
        .into_iter()
        .map(|arg| arg.to_css_string(span, visitor.parser.options.is_compressed()))
        .collect::<SassResult<Vec<_>>>()?
        .join(", ");

    Ok((format!("{}({})", name, args)))
}

fn inner_rgb_3_arg(
    name: &'static str,
    mut args: ArgumentResult,
    visitor: &mut Visitor,
) -> SassResult<Value> {
    let alpha = if args.len() > 3 {
        args.get(3, "alpha")
    } else {
        None
    };

    let red = args.get_err(0, "red")?;
    let green = args.get_err(1, "green")?;
    let blue = args.get_err(2, "blue")?;

    if red.is_special_function()
        || green.is_special_function()
        || blue.is_special_function()
        || alpha
            .as_ref()
            .map(|alpha| alpha.node.is_special_function())
            .unwrap_or(false)
    {
        let fn_string = if alpha.is_some() {
            function_string(
                name,
                &[red, green, blue, alpha.unwrap().node],
                visitor,
                args.span(),
            )?
        } else {
            function_string(name, &[red, green, blue], visitor, args.span())?
        };

        return Ok(Value::String(fn_string, QuoteKind::None));
    }

    let span = args.span();

    let red = red.assert_number_with_name(span, "red")?;
    let green = green.assert_number_with_name(span, "green")?;
    let blue = blue.assert_number_with_name(span, "blue")?;

    Ok(Value::Color(Box::new(Color::from_rgba_fn(
        Number(fuzzy_round(percentage_or_unitless(
            red, 255.0, "red", span, visitor,
        )?)),
        Number(fuzzy_round(percentage_or_unitless(
            green, 255.0, "green", span, visitor,
        )?)),
        Number(fuzzy_round(percentage_or_unitless(
            blue, 255.0, "blue", span, visitor,
        )?)),
        Number(
            alpha
                .map(|alpha| {
                    percentage_or_unitless(
                        alpha.node.assert_number_with_name(span, "alpha")?,
                        1.0,
                        "alpha",
                        span,
                        visitor,
                    )
                })
                .transpose()?
                .unwrap_or(1.0),
        ),
    ))))
}

fn percentage_or_unitless(
    number: SassNumber,
    max: f64,
    name: &str,
    span: Span,
    visitor: &mut Visitor,
) -> SassResult<f64> {
    let value = if number.unit == Unit::None {
        number.num
    } else if number.unit == Unit::Percent {
        (number.num * max) / 100.0
    } else {
        return Err((
            format!(
                "${name}: Expected {} to have no units or \"%\".",
                inspect_number(&number, visitor.parser.options, span)?
            ),
            span,
        )
            .into());
    };

    Ok(value.clamp(0.0, max))
}

/// name: Either `rgb` or `rgba` depending on the caller
// todo: refactor into smaller functions
#[allow(clippy::cognitive_complexity)]
fn inner_rgb(
    name: &'static str,
    mut args: ArgumentResult,
    parser: &mut Visitor,
) -> SassResult<Value> {
    if args.is_empty() {
        return Err(("Missing argument $channels.", args.span()).into());
    }

    args.max_args(4)?;

    let len = args.len();

    if len == 3 || len == 4 {
        return inner_rgb_3_arg(name, args, parser);
    }

    if len == 1 {
        let mut channels = match args.get_err(0, "channels")? {
            Value::List(v, ..) => v,
            v if v.is_special_function() => vec![v],
            _ => return Err(("Missing argument $channels.", args.span()).into()),
        };

        if channels.len() > 3 {
            return Err((
                format!(
                    "Only 3 elements allowed, but {} were passed.",
                    channels.len()
                ),
                args.span(),
            )
                .into());
        }

        if channels.iter().any(Value::is_special_function) {
            let channel_sep = if channels.len() < 3 {
                ListSeparator::Space
            } else {
                ListSeparator::Comma
            };

            return Ok(Value::String(
                format!(
                    "{}({})",
                    name,
                    Value::List(channels, channel_sep, Brackets::None)
                        .to_css_string(args.span(), false)?
                ),
                QuoteKind::None,
            ));
        }

        let blue = match channels.pop() {
            Some(Value::Dimension { num: n, .. }) if n.is_nan() => todo!(),
            Some(Value::Dimension {
                num: (n),
                unit: Unit::None,
                as_slash: _,
            }) => n,
            Some(Value::Dimension {
                num: (n),
                unit: Unit::Percent,
                as_slash: _,
            }) => (n / Number::from(100)) * Number::from(255),
            Some(v) if v.is_special_function() => {
                let green = channels.pop().unwrap();
                let red = channels.pop().unwrap();
                return Ok(Value::String(
                    format!(
                        "{}({}, {}, {})",
                        name,
                        red.to_css_string(args.span(), parser.parser.options.is_compressed())?,
                        green.to_css_string(args.span(), parser.parser.options.is_compressed())?,
                        v.to_css_string(args.span(), parser.parser.options.is_compressed())?
                    ),
                    QuoteKind::None,
                ));
            }
            Some(v) => {
                return Err((
                    format!("$blue: {} is not a number.", v.inspect(args.span())?),
                    args.span(),
                )
                    .into())
            }
            None => return Err(("Missing element $blue.", args.span()).into()),
        };

        let green = match channels.pop() {
            Some(Value::Dimension { num: n, .. }) if n.is_nan() => todo!(),
            Some(Value::Dimension {
                num: (n),
                unit: Unit::None,
                as_slash: _,
            }) => n,
            Some(Value::Dimension {
                num: (n),
                unit: Unit::Percent,
                as_slash: _,
            }) => (n / Number::from(100)) * Number::from(255),
            Some(v) if v.is_special_function() => {
                let string = match channels.pop() {
                    Some(red) => format!(
                        "{}({}, {}, {})",
                        name,
                        red.to_css_string(args.span(), parser.parser.options.is_compressed())?,
                        v.to_css_string(args.span(), parser.parser.options.is_compressed())?,
                        blue.to_string(parser.parser.options.is_compressed())
                    ),
                    None => format!(
                        "{}({} {})",
                        name,
                        v.to_css_string(args.span(), parser.parser.options.is_compressed())?,
                        blue.to_string(parser.parser.options.is_compressed())
                    ),
                };
                return Ok(Value::String(string, QuoteKind::None));
            }
            Some(v) => {
                return Err((
                    format!("$green: {} is not a number.", v.inspect(args.span())?),
                    args.span(),
                )
                    .into())
            }
            None => return Err(("Missing element $green.", args.span()).into()),
        };

        let red = match channels.pop() {
            Some(Value::Dimension { num: n, .. }) if n.is_nan() => todo!(),
            Some(Value::Dimension {
                num: (n),
                unit: Unit::None,
                as_slash: _,
            }) => n,
            Some(Value::Dimension {
                num: (n),
                unit: Unit::Percent,
                as_slash: _,
            }) => (n / Number::from(100)) * Number::from(255),
            Some(v) if v.is_special_function() => {
                return Ok(Value::String(
                    format!(
                        "{}({}, {}, {})",
                        name,
                        v.to_css_string(args.span(), parser.parser.options.is_compressed())?,
                        green.to_string(parser.parser.options.is_compressed()),
                        blue.to_string(parser.parser.options.is_compressed())
                    ),
                    QuoteKind::None,
                ));
            }
            Some(v) => {
                return Err((
                    format!("$red: {} is not a number.", v.inspect(args.span())?),
                    args.span(),
                )
                    .into())
            }
            None => return Err(("Missing element $red.", args.span()).into()),
        };

        let color = Color::from_rgba(red, green, blue, Number::one());

        Ok(Value::Color(Box::new(color)))
    } else {
        let color = args.get_err(0, "color")?;
        let alpha = args.get_err(1, "alpha")?;

        if color.is_special_function() || (alpha.is_special_function() && !color.is_color()) {
            return Ok(Value::String(
                format!(
                    "{}({})",
                    name,
                    Value::List(vec![color, alpha], ListSeparator::Comma, Brackets::None)
                        .to_css_string(args.span(), false)?
                ),
                QuoteKind::None,
            ));
        }

        let color = match color {
            Value::Color(c) => c,
            v => {
                return Err((
                    format!("$color: {} is not a color.", v.inspect(args.span())?),
                    args.span(),
                )
                    .into())
            }
        };

        if alpha.is_special_function() {
            return Ok(Value::String(
                format!(
                    "{}({}, {}, {}, {})",
                    name,
                    color.red().to_string(false),
                    color.green().to_string(false),
                    color.blue().to_string(false),
                    alpha.to_css_string(args.span(), false)?,
                ),
                QuoteKind::None,
            ));
        }

        let alpha = match alpha {
            Value::Dimension { num: n, .. } if n.is_nan() => todo!(),
            Value::Dimension {
                num: (n),
                unit: Unit::None,
                as_slash: _,
            } => n,
            Value::Dimension {
                num: (n),
                unit: Unit::Percent,
                as_slash: _,
            } => n / Number::from(100),
            v @ Value::Dimension { .. } => {
                return Err((
                    format!(
                        "$alpha: Expected {} to have no units or \"%\".",
                        v.to_css_string(args.span(), parser.parser.options.is_compressed())?
                    ),
                    args.span(),
                )
                    .into())
            }
            v => {
                return Err((
                    format!("$alpha: {} is not a number.", v.inspect(args.span())?),
                    args.span(),
                )
                    .into())
            }
        };
        Ok(Value::Color(Box::new(color.with_alpha(alpha))))
    }
}

pub(crate) fn rgb(args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    inner_rgb("rgb", args, parser)
}

pub(crate) fn rgba(args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    inner_rgb("rgba", args, parser)
}

pub(crate) fn red(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    match args.get_err(0, "color")? {
        Value::Color(c) => Ok(Value::Dimension {
            num: (c.red()),
            unit: Unit::None,
            as_slash: None,
        }),
        v => Err((
            format!("$color: {} is not a color.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn green(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    match args.get_err(0, "color")? {
        Value::Color(c) => Ok(Value::Dimension {
            num: (c.green()),
            unit: Unit::None,
            as_slash: None,
        }),
        v => Err((
            format!("$color: {} is not a color.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn blue(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    match args.get_err(0, "color")? {
        Value::Color(c) => Ok(Value::Dimension {
            num: (c.blue()),
            unit: Unit::None,
            as_slash: None,
        }),
        v => Err((
            format!("$color: {} is not a color.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn mix(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(3)?;
    let color1 = match args.get_err(0, "color1")? {
        Value::Color(c) => c,
        v => {
            return Err((
                format!("$color1: {} is not a color.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    let color2 = match args.get_err(1, "color2")? {
        Value::Color(c) => c,
        v => {
            return Err((
                format!("$color2: {} is not a color.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    let weight = match args.default_arg(
        2,
        "weight",
        Value::Dimension {
            num: (Number::from(50)),
            unit: Unit::None,
            as_slash: None,
        },
    ) {
        Value::Dimension { num: n, .. } if n.is_nan() => todo!(),
        Value::Dimension {
            num: (n),
            unit: u,
            as_slash: _,
        } => bound!(args, "weight", n, u, 0, 100) / Number::from(100),
        v => {
            return Err((
                format!(
                    "$weight: {} is not a number.",
                    v.to_css_string(args.span(), parser.parser.options.is_compressed())?
                ),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::Color(Box::new(color1.mix(&color2, weight))))
}

pub(crate) fn declare(f: &mut GlobalFunctionMap) {
    f.insert("rgb", Builtin::new(rgb));
    f.insert("rgba", Builtin::new(rgba));
    f.insert("red", Builtin::new(red));
    f.insert("green", Builtin::new(green));
    f.insert("blue", Builtin::new(blue));
    f.insert("mix", Builtin::new(mix));
}
