use crate::builtin::builtin_imports::*;

use super::rgb::{parse_channels, ParsedChannels};

pub(crate) fn blackness(mut args: ArgumentResult, visitor: &mut Visitor) -> SassResult<Value> {
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
        Number(1.0) - (color.red().max(color.green()).max(color.blue()) / Number(255.0));

    Ok(Value::Dimension(SassNumber {
        num: (blackness * 100),
        unit: Unit::Percent,
        as_slash: None,
    }))
}

pub(crate) fn whiteness(mut args: ArgumentResult, visitor: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;

    let color = args
        .get_err(0, "color")?
        .assert_color_with_name("color", args.span())?;

    let whiteness = color.red().min(color.green()).min(color.blue()) / Number(255.0);

    Ok(Value::Dimension(SassNumber {
        num: (whiteness * 100),
        unit: Unit::Percent,
        as_slash: None,
    }))
}

fn hwb_inner(mut args: ArgumentResult, visitor: &mut Visitor) -> SassResult<Value> {
    let span = args.span();

    let hue = match args.get(0, "hue") {
        Some(v) => match v.node {
            Value::Dimension(SassNumber { num: n, .. }) if n.is_nan() => todo!(),
            Value::Dimension(SassNumber { num: n, .. }) => n,
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

    let whiteness = args
        .get_err(1, "whiteness")?
        .assert_number_with_name("whiteness", span)?;
    whiteness.assert_unit(&Unit::Percent, "whiteness", span)?;

    let blackness = args
        .get_err(2, "blackness")?
        .assert_number_with_name("blackness", span)?;
    blackness.assert_unit(&Unit::Percent, "blackness", span)?;

    let alpha = match args.get(3, "alpha") {
        Some(v) => match v.node {
            Value::Dimension(SassNumber { num: n, .. }) if n.is_nan() => todo!(),
            Value::Dimension(SassNumber {
                num: n,
                unit: Unit::Percent,
                ..
            }) => n / Number(100.0),
            Value::Dimension(SassNumber { num: n, .. }) => n,
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
        hue,
        whiteness.num,
        blackness.num,
        alpha,
    ))))
}

pub(crate) fn hwb(mut args: ArgumentResult, visitor: &mut Visitor) -> SassResult<Value> {
    args.max_args(4)?;

    if args.len() == 0 || args.len() == 1 {
        match parse_channels(
            "hwb",
            &["hue", "whiteness", "blackness"],
            args.get_err(0, "channels")?,
            visitor,
            args.span(),
        )? {
            ParsedChannels::String(s) => {
                Err((format!("Expected numeric channels, got {}", s), args.span()).into())
            }
            ParsedChannels::List(list) => {
                let args = ArgumentResult {
                    positional: list,
                    named: BTreeMap::new(),
                    separator: ListSeparator::Comma,
                    span: args.span(),
                    touched: BTreeSet::new(),
                };

                hwb_inner(args, visitor)
            }
        }
    } else {
        hwb_inner(args, visitor)
    }
}
