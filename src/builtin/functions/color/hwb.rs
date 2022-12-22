use crate::builtin::builtin_imports::*;

pub(crate) fn blackness(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
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

    let blackness =
        Number::from(1) - (color.red().max(color.green()).max(color.blue()) / Number::from(255));

    Ok(Value::Dimension {
        num: (blackness * 100),
        unit: Unit::Percent,
        as_slash: None,
    })
}

pub(crate) fn whiteness(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
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

    let whiteness = color.red().min(color.green()).min(color.blue()) / Number::from(255);

    Ok(Value::Dimension {
        num: (whiteness * 100),
        unit: Unit::Percent,
        as_slash: None,
    })
}

pub(crate) fn hwb(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(4)?;

    if args.is_empty() {
        return Err(("Missing argument $channels.", args.span()).into());
    }

    let hue = match args.get(0, "hue") {
        Some(v) => match v.node {
            Value::Dimension { num: n, .. } if n.is_nan() => todo!(),
            Value::Dimension { num: n, .. } => n,
            v => {
                return Err((
                    format!("$hue: {} is not a number.", v.inspect(args.span())?),
                    args.span(),
                )
                    .into())
            }
        },
        None => return Err(("Missing element $hue.", args.span()).into()),
    };

    let whiteness = match args.get(1, "whiteness") {
        Some(v) => match v.node {
            Value::Dimension { num: n, .. } if n.is_nan() => todo!(),
            Value::Dimension {
                num: n,
                unit: Unit::Percent,
                ..
            } => n,
            v @ Value::Dimension { .. } => {
                return Err((
                    format!(
                        "$whiteness: Expected {} to have unit \"%\".",
                        v.inspect(args.span())?
                    ),
                    args.span(),
                )
                    .into())
            }
            v => {
                return Err((
                    format!("$whiteness: {} is not a number.", v.inspect(args.span())?),
                    args.span(),
                )
                    .into())
            }
        },
        None => return Err(("Missing element $whiteness.", args.span()).into()),
    };

    let blackness = match args.get(2, "blackness") {
        Some(v) => match v.node {
            Value::Dimension { num: n, .. } if n.is_nan() => todo!(),
            Value::Dimension { num: n, .. } => n,
            v => {
                return Err((
                    format!("$blackness: {} is not a number.", v.inspect(args.span())?),
                    args.span(),
                )
                    .into())
            }
        },
        None => return Err(("Missing element $blackness.", args.span()).into()),
    };

    let alpha = match args.get(3, "alpha") {
        Some(v) => match v.node {
            Value::Dimension { num: n, .. } if n.is_nan() => todo!(),
            Value::Dimension {
                num: n,
                unit: Unit::Percent,
                ..
            } => n / Number::from(100),
            Value::Dimension { num: n, .. } => n,
            v => {
                return Err((
                    format!("$alpha: {} is not a number.", v.inspect(args.span())?),
                    args.span(),
                )
                    .into())
            }
        },
        None => Number::one(),
    };

    Ok(Value::Color(Box::new(Color::from_hwb(
        hue, whiteness, blackness, alpha,
    ))))
}
