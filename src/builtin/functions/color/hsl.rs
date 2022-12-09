use super::{Builtin, GlobalFunctionMap};

use codemap::Spanned;
use num_traits::One;

use crate::{
    color::Color,
    common::{Brackets, ListSeparator, QuoteKind},
    error::SassResult,
    parse::{visitor::Visitor, ArgumentResult, Parser},
    unit::Unit,
    value::{Number, Value},
};

fn inner_hsl(
    name: &'static str,
    mut args: ArgumentResult,
    parser: &mut Visitor,
) -> SassResult<Value> {
    if args.is_empty() {
        return Err(("Missing argument $channels.", args.span()).into());
    }

    let len = args.len();

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

        let lightness = match channels.pop() {
            Some(Value::Dimension(Some(n), ..)) => n / Number::from(100),
            Some(Value::Dimension(None, ..)) => todo!(),
            Some(v) => {
                return Err((
                    format!("$lightness: {} is not a number.", v.inspect(args.span())?),
                    args.span(),
                )
                    .into())
            }
            None => return Err(("Missing element $lightness.", args.span()).into()),
        };

        let saturation = match channels.pop() {
            Some(Value::Dimension(Some(n), ..)) => n / Number::from(100),
            Some(Value::Dimension(None, ..)) => todo!(),
            Some(v) => {
                return Err((
                    format!("$saturation: {} is not a number.", v.inspect(args.span())?),
                    args.span(),
                )
                    .into())
            }
            None => return Err(("Missing element $saturation.", args.span()).into()),
        };

        let hue = match channels.pop() {
            Some(Value::Dimension(Some(n), ..)) => n,
            Some(Value::Dimension(None, ..)) => todo!(),
            Some(v) => {
                return Err((
                    format!("$hue: {} is not a number.", v.inspect(args.span())?),
                    args.span(),
                )
                    .into())
            }
            None => return Err(("Missing element $hue.", args.span()).into()),
        };

        Ok(Value::Color(Box::new(Color::from_hsla(
            hue,
            saturation,
            lightness,
            Number::one(),
        ))))
    } else {
        let hue = args.get_err(0, "hue")?;
        let saturation = args.get_err(1, "saturation")?;
        let lightness = args.get_err(2, "lightness")?;
        let alpha = args.default_arg(
            3,
            "alpha",
            Value::Dimension(Some(Number::one()), Unit::None, true),
        );

        if [&hue, &saturation, &lightness, &alpha]
            .iter()
            .copied()
            .any(Value::is_special_function)
        {
            return Ok(Value::String(
                format!(
                    "{}({})",
                    name,
                    Value::List(
                        if len == 4 {
                            vec![hue, saturation, lightness, alpha]
                        } else {
                            vec![hue, saturation, lightness]
                        },
                        ListSeparator::Comma,
                        Brackets::None
                    )
                    .to_css_string(args.span(), false)?
                ),
                QuoteKind::None,
            ));
        }

        let hue = match hue {
            Value::Dimension(Some(n), ..) => n,
            Value::Dimension(None, ..) => todo!(),
            v => {
                return Err((
                    format!("$hue: {} is not a number.", v.inspect(args.span())?),
                    args.span(),
                )
                    .into())
            }
        };
        let saturation = match saturation {
            Value::Dimension(Some(n), ..) => n / Number::from(100),
            Value::Dimension(None, ..) => todo!(),
            v => {
                return Err((
                    format!(
                        "$saturation: {} is not a number.",
                        v.to_css_string(args.span(), parser.parser.options.is_compressed())?
                    ),
                    args.span(),
                )
                    .into())
            }
        };
        let lightness = match lightness {
            Value::Dimension(Some(n), ..) => n / Number::from(100),
            Value::Dimension(None, ..) => todo!(),
            v => {
                return Err((
                    format!(
                        "$lightness: {} is not a number.",
                        v.to_css_string(args.span(), parser.parser.options.is_compressed())?
                    ),
                    args.span(),
                )
                    .into())
            }
        };
        let alpha = match alpha {
            Value::Dimension(Some(n), Unit::None, _) => n,
            Value::Dimension(Some(n), Unit::Percent, _) => n / Number::from(100),
            Value::Dimension(None, ..) => todo!(),
            v @ Value::Dimension(..) => {
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
        Ok(Value::Color(Box::new(Color::from_hsla(
            hue, saturation, lightness, alpha,
        ))))
    }
}

pub(crate) fn hsl(args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    inner_hsl("hsl", args, parser)
}

pub(crate) fn hsla(args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    inner_hsl("hsla", args, parser)
}

pub(crate) fn hue(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    match args.get_err(0, "color")? {
        Value::Color(c) => Ok(Value::Dimension(Some(c.hue()), Unit::Deg, true)),
        v => Err((
            format!("$color: {} is not a color.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn saturation(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    match args.get_err(0, "color")? {
        Value::Color(c) => Ok(Value::Dimension(Some(c.saturation()), Unit::Percent, true)),
        v => Err((
            format!("$color: {} is not a color.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn lightness(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    match args.get_err(0, "color")? {
        Value::Color(c) => Ok(Value::Dimension(Some(c.lightness()), Unit::Percent, true)),
        v => Err((
            format!("$color: {} is not a color.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn adjust_hue(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(2)?;
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
    let degrees = match args.get_err(1, "degrees")? {
        Value::Dimension(Some(n), ..) => n,
        Value::Dimension(None, ..) => todo!(),
        v => {
            return Err((
                format!(
                    "$degrees: {} is not a number.",
                    v.to_css_string(args.span(), parser.parser.options.is_compressed())?
                ),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::Color(Box::new(color.adjust_hue(degrees))))
}

fn lighten(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(2)?;
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
    let amount = match args.get_err(1, "amount")? {
        Value::Dimension(Some(n), u, _) => bound!(args, "amount", n, u, 0, 100) / Number::from(100),
        Value::Dimension(None, ..) => todo!(),
        v => {
            return Err((
                format!(
                    "$amount: {} is not a number.",
                    v.to_css_string(args.span(), false)?
                ),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::Color(Box::new(color.lighten(amount))))
}

fn darken(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(2)?;
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
    let amount = match args.get_err(1, "amount")? {
        Value::Dimension(Some(n), u, _) => bound!(args, "amount", n, u, 0, 100) / Number::from(100),
        Value::Dimension(None, ..) => todo!(),
        v => {
            return Err((
                format!(
                    "$amount: {} is not a number.",
                    v.to_css_string(args.span(), false)?
                ),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::Color(Box::new(color.darken(amount))))
}

fn saturate(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(2)?;
    if args.len() == 1 {
        return Ok(Value::String(
            format!(
                "saturate({})",
                args.get_err(0, "amount")?
                    .to_css_string(args.span(), false)?
            ),
            QuoteKind::None,
        ));
    }

    let amount = match args.get_err(1, "amount")? {
        Value::Dimension(Some(n), u, _) => bound!(args, "amount", n, u, 0, 100) / Number::from(100),
        Value::Dimension(None, ..) => todo!(),
        v => {
            return Err((
                format!(
                    "$amount: {} is not a number.",
                    v.to_css_string(args.span(), false)?
                ),
                args.span(),
            )
                .into())
        }
    };
    let color = match args.get_err(0, "color")? {
        Value::Color(c) => c,
        Value::Dimension(Some(n), u, _) => {
            return Ok(Value::String(
                format!("saturate({}{})", n.inspect(), u),
                QuoteKind::None,
            ))
        }
        v => {
            return Err((
                format!("$color: {} is not a color.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::Color(Box::new(color.saturate(amount))))
}

fn desaturate(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(2)?;
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
    let amount = match args.get_err(1, "amount")? {
        Value::Dimension(Some(n), u, _) => bound!(args, "amount", n, u, 0, 100) / Number::from(100),
        Value::Dimension(None, ..) => todo!(),
        v => {
            return Err((
                format!(
                    "$amount: {} is not a number.",
                    v.to_css_string(args.span(), parser.parser.options.is_compressed())?
                ),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::Color(Box::new(color.desaturate(amount))))
}

pub(crate) fn grayscale(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    let color = match args.get_err(0, "color")? {
        Value::Color(c) => c,
        Value::Dimension(Some(n), u, _) => {
            return Ok(Value::String(
                format!("grayscale({}{})", n.inspect(), u),
                QuoteKind::None,
            ))
        }
        v => {
            return Err((
                format!("$color: {} is not a color.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::Color(Box::new(color.desaturate(Number::one()))))
}

pub(crate) fn complement(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
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
    Ok(Value::Color(Box::new(color.complement())))
}

pub(crate) fn invert(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(2)?;
    let weight = match args.get(1, "weight") {
        Some(Spanned {
            node: Value::Dimension(Some(n), u, _),
            ..
        }) => Some(bound!(args, "weight", n, u, 0, 100) / Number::from(100)),
        Some(Spanned {
            node: Value::Dimension(None, ..),
            ..
        }) => todo!(),
        None => None,
        Some(v) => {
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
    match args.get_err(0, "color")? {
        Value::Color(c) => Ok(Value::Color(Box::new(
            c.invert(weight.unwrap_or_else(Number::one)),
        ))),
        Value::Dimension(Some(n), u, _) => {
            if weight.is_some() {
                return Err((
                    "Only one argument may be passed to the plain-CSS invert() function.",
                    args.span(),
                )
                    .into());
            }
            Ok(Value::String(
                format!("invert({}{})", n.inspect(), u),
                QuoteKind::None,
            ))
        }
        Value::Dimension(None, u, _) => {
            Ok(Value::String(format!("invert(NaN{})", u), QuoteKind::None))
        }
        v => Err((
            format!("$color: {} is not a color.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn declare(f: &mut GlobalFunctionMap) {
    f.insert("hsl", Builtin::new(hsl));
    f.insert("hsla", Builtin::new(hsla));
    f.insert("hue", Builtin::new(hue));
    f.insert("saturation", Builtin::new(saturation));
    f.insert("adjust-hue", Builtin::new(adjust_hue));
    f.insert("lightness", Builtin::new(lightness));
    f.insert("lighten", Builtin::new(lighten));
    f.insert("darken", Builtin::new(darken));
    f.insert("saturate", Builtin::new(saturate));
    f.insert("desaturate", Builtin::new(desaturate));
    f.insert("grayscale", Builtin::new(grayscale));
    f.insert("complement", Builtin::new(complement));
    f.insert("invert", Builtin::new(invert));
}
