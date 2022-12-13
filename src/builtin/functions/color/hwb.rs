use num_traits::One;

use crate::{
    color::Color,
    error::SassResult,
    parse::{visitor::Visitor, ArgumentResult, Parser},
    unit::Unit,
    value::{Number, Value},
};

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

    Ok(Value::Dimension((blackness * 100), Unit::Percent, None))
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

    Ok(Value::Dimension((whiteness * 100), Unit::Percent, None))
}

pub(crate) fn hwb(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(4)?;

    if args.is_empty() {
        return Err(("Missing argument $channels.", args.span()).into());
    }

    let hue = match args.get(0, "hue") {
        Some(v) => match v.node {
            Value::Dimension(n, ..) if n.is_nan() => todo!(),
            Value::Dimension((n), ..) => n,
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
            Value::Dimension(n, ..) if n.is_nan() => todo!(),
            Value::Dimension((n), Unit::Percent, ..) => n,
            v @ Value::Dimension(..) => {
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
            Value::Dimension(n, ..) if n.is_nan() => todo!(),
            Value::Dimension((n), ..) => n,
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
            Value::Dimension(n, ..) if n.is_nan() => todo!(),
            Value::Dimension((n), Unit::Percent, ..) => n / Number::from(100),
            Value::Dimension((n), ..) => n,
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
