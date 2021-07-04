use num_traits::One;

use crate::{
    args::CallArgs,
    color::Color,
    error::SassResult,
    parse::Parser,
    unit::Unit,
    value::{Number, Value},
};

pub(crate) fn blackness(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
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

    Ok(Value::Dimension(Some(blackness * 100), Unit::Percent, true))
}

pub(crate) fn whiteness(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
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

    Ok(Value::Dimension(Some(whiteness * 100), Unit::Percent, true))
}

pub(crate) fn hwb(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(4)?;

    if args.is_empty() {
        return Err(("Missing argument $channels.", args.span()).into());
    }

    let hue = match args.get(0, "hue") {
        Some(Ok(v)) => match v.node {
            Value::Dimension(Some(n), ..) => n,
            Value::Dimension(None, ..) => todo!(),
            v => {
                return Err((
                    format!("$hue: {} is not a number.", v.inspect(args.span())?),
                    args.span(),
                )
                    .into())
            }
        },
        Some(Err(e)) => return Err(e),
        None => return Err(("Missing element $hue.", args.span()).into()),
    };

    let whiteness = match args.get(1, "whiteness") {
        Some(Ok(v)) => match v.node {
            Value::Dimension(Some(n), Unit::Percent, ..) => n,
            v @ Value::Dimension(Some(..), ..) => {
                return Err((
                    format!(
                        "$whiteness: Expected {} to have unit \"%\".",
                        v.inspect(args.span())?
                    ),
                    args.span(),
                )
                    .into())
            }
            Value::Dimension(None, ..) => todo!(),
            v => {
                return Err((
                    format!("$whiteness: {} is not a number.", v.inspect(args.span())?),
                    args.span(),
                )
                    .into())
            }
        },
        Some(Err(e)) => return Err(e),
        None => return Err(("Missing element $whiteness.", args.span()).into()),
    };

    let blackness = match args.get(2, "blackness") {
        Some(Ok(v)) => match v.node {
            Value::Dimension(Some(n), ..) => n,
            Value::Dimension(None, ..) => todo!(),
            v => {
                return Err((
                    format!("$blackness: {} is not a number.", v.inspect(args.span())?),
                    args.span(),
                )
                    .into())
            }
        },
        Some(Err(e)) => return Err(e),
        None => return Err(("Missing element $blackness.", args.span()).into()),
    };

    let alpha = match args.get(3, "alpha") {
        Some(Ok(v)) => match v.node {
            Value::Dimension(Some(n), Unit::Percent, ..) => n / Number::from(100),
            Value::Dimension(Some(n), ..) => n,
            Value::Dimension(None, ..) => todo!(),
            v => {
                return Err((
                    format!("$alpha: {} is not a number.", v.inspect(args.span())?),
                    args.span(),
                )
                    .into())
            }
        },
        Some(Err(e)) => return Err(e),
        None => Number::one(),
    };

    Ok(Value::Color(Box::new(Color::from_hwb(
        hue, whiteness, blackness, alpha,
    ))))
}
