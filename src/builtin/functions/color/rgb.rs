use super::{Builtin, GlobalFunctionMap};

use num_traits::One;

use crate::{
    args::CallArgs,
    color::Color,
    common::{Brackets, ListSeparator, QuoteKind},
    error::SassResult,
    parse::Parser,
    unit::Unit,
    value::{Number, Value},
};

/// name: Either `rgb` or `rgba` depending on the caller
// todo: refactor into smaller functions
#[allow(clippy::cognitive_complexity)]
fn inner_rgb(name: &'static str, mut args: CallArgs, parser: &mut Parser) -> SassResult<Value> {
    if args.is_empty() {
        return Err(("Missing argument $channels.", args.span()).into());
    }

    if args.len() == 1 {
        let mut channels = match args.get_err(0, "channels")? {
            Value::List(v, ..) => v,
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
            Some(Value::Dimension(Some(n), Unit::None, _)) => n,
            Some(Value::Dimension(Some(n), Unit::Percent, _)) => {
                (n / Number::from(100)) * Number::from(255)
            }
            Some(Value::Dimension(None, ..)) => todo!(),
            Some(v) if v.is_special_function() => {
                let green = channels.pop().unwrap();
                let red = channels.pop().unwrap();
                return Ok(Value::String(
                    format!(
                        "{}({}, {}, {})",
                        name,
                        red.to_css_string(args.span(), parser.options.is_compressed())?,
                        green.to_css_string(args.span(), parser.options.is_compressed())?,
                        v.to_css_string(args.span(), parser.options.is_compressed())?
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
            Some(Value::Dimension(Some(n), Unit::None, _)) => n,
            Some(Value::Dimension(Some(n), Unit::Percent, _)) => {
                (n / Number::from(100)) * Number::from(255)
            }
            Some(Value::Dimension(None, ..)) => todo!(),
            Some(v) if v.is_special_function() => {
                let string = match channels.pop() {
                    Some(red) => format!(
                        "{}({}, {}, {})",
                        name,
                        red.to_css_string(args.span(), parser.options.is_compressed())?,
                        v.to_css_string(args.span(), parser.options.is_compressed())?,
                        blue.to_string(parser.options.is_compressed())
                    ),
                    None => format!(
                        "{}({} {})",
                        name,
                        v.to_css_string(args.span(), parser.options.is_compressed())?,
                        blue.to_string(parser.options.is_compressed())
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
            Some(Value::Dimension(Some(n), Unit::None, _)) => n,
            Some(Value::Dimension(Some(n), Unit::Percent, _)) => {
                (n / Number::from(100)) * Number::from(255)
            }
            Some(Value::Dimension(None, ..)) => todo!(),
            Some(v) if v.is_special_function() => {
                return Ok(Value::String(
                    format!(
                        "{}({}, {}, {})",
                        name,
                        v.to_css_string(args.span(), parser.options.is_compressed())?,
                        green.to_string(parser.options.is_compressed()),
                        blue.to_string(parser.options.is_compressed())
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
    } else if args.len() == 2 {
        let color = match args.get_err(0, "color")? {
            Value::Color(c) => c,
            v if v.is_special_function() => {
                let alpha = args.get_err(1, "alpha")?;
                return Ok(Value::String(
                    format!(
                        "{}({}, {})",
                        name,
                        v.to_css_string(args.span(), parser.options.is_compressed())?,
                        alpha.to_css_string(args.span(), parser.options.is_compressed())?
                    ),
                    QuoteKind::None,
                ));
            }
            v => {
                return Err((
                    format!("$color: {} is not a color.", v.inspect(args.span())?),
                    args.span(),
                )
                    .into())
            }
        };
        let alpha = match args.get_err(1, "alpha")? {
            Value::Dimension(Some(n), Unit::None, _) => n,
            Value::Dimension(Some(n), Unit::Percent, _) => n / Number::from(100),
            Value::Dimension(None, ..) => todo!(),
            v @ Value::Dimension(..) => {
                return Err((
                    format!(
                        "$alpha: Expected {} to have no units or \"%\".",
                        v.to_css_string(args.span(), parser.options.is_compressed())?
                    ),
                    args.span(),
                )
                    .into())
            }
            v if v.is_special_function() => {
                return Ok(Value::String(
                    format!(
                        "{}({}, {}, {}, {})",
                        name,
                        color.red().to_string(parser.options.is_compressed()),
                        color.green().to_string(parser.options.is_compressed()),
                        color.blue().to_string(parser.options.is_compressed()),
                        v.to_css_string(args.span(), parser.options.is_compressed())?
                    ),
                    QuoteKind::None,
                ));
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
    } else {
        let red = match args.get_err(0, "red")? {
            Value::Dimension(Some(n), Unit::None, _) => n,
            Value::Dimension(Some(n), Unit::Percent, _) => {
                (n / Number::from(100)) * Number::from(255)
            }
            Value::Dimension(None, ..) => todo!(),
            v @ Value::Dimension(..) => {
                return Err((
                    format!(
                        "$red: Expected {} to have no units or \"%\".",
                        v.to_css_string(args.span(), parser.options.is_compressed())?
                    ),
                    args.span(),
                )
                    .into())
            }
            v if v.is_special_function() => {
                let green = args.get_err(1, "green")?;
                let blue = args.get_err(2, "blue")?;
                let mut string = format!(
                    "{}({}, {}, {}",
                    name,
                    v.to_css_string(args.span(), parser.options.is_compressed())?,
                    green.to_css_string(args.span(), parser.options.is_compressed())?,
                    blue.to_css_string(args.span(), parser.options.is_compressed())?
                );
                if !args.is_empty() {
                    string.push_str(", ");
                    string.push_str(
                        &args
                            .get_err(3, "alpha")?
                            .to_css_string(args.span(), parser.options.is_compressed())?,
                    );
                }
                string.push(')');
                return Ok(Value::String(string, QuoteKind::None));
            }
            v => {
                return Err((
                    format!("$red: {} is not a number.", v.inspect(args.span())?),
                    args.span(),
                )
                    .into())
            }
        };
        let green = match args.get_err(1, "green")? {
            Value::Dimension(Some(n), Unit::None, _) => n,
            Value::Dimension(Some(n), Unit::Percent, _) => {
                (n / Number::from(100)) * Number::from(255)
            }
            Value::Dimension(None, ..) => todo!(),
            v @ Value::Dimension(..) => {
                return Err((
                    format!(
                        "$green: Expected {} to have no units or \"%\".",
                        v.to_css_string(args.span(), parser.options.is_compressed())?
                    ),
                    args.span(),
                )
                    .into())
            }
            v if v.is_special_function() => {
                let blue = args.get_err(2, "blue")?;
                let mut string = format!(
                    "{}({}, {}, {}",
                    name,
                    red.to_string(parser.options.is_compressed()),
                    v.to_css_string(args.span(), parser.options.is_compressed())?,
                    blue.to_css_string(args.span(), parser.options.is_compressed())?
                );
                if !args.is_empty() {
                    string.push_str(", ");
                    string.push_str(
                        &args
                            .get_err(3, "alpha")?
                            .to_css_string(args.span(), parser.options.is_compressed())?,
                    );
                }
                string.push(')');
                return Ok(Value::String(string, QuoteKind::None));
            }
            v => {
                return Err((
                    format!("$green: {} is not a number.", v.inspect(args.span())?),
                    args.span(),
                )
                    .into())
            }
        };
        let blue = match args.get_err(2, "blue")? {
            Value::Dimension(Some(n), Unit::None, _) => n,
            Value::Dimension(Some(n), Unit::Percent, _) => {
                (n / Number::from(100)) * Number::from(255)
            }
            Value::Dimension(None, ..) => todo!(),
            v @ Value::Dimension(..) => {
                return Err((
                    format!(
                        "$blue: Expected {} to have no units or \"%\".",
                        v.to_css_string(args.span(), parser.options.is_compressed())?
                    ),
                    args.span(),
                )
                    .into())
            }
            v if v.is_special_function() => {
                let mut string = format!(
                    "{}({}, {}, {}",
                    name,
                    red.to_string(parser.options.is_compressed()),
                    green.to_string(parser.options.is_compressed()),
                    v.to_css_string(args.span(), parser.options.is_compressed())?
                );
                if !args.is_empty() {
                    string.push_str(", ");
                    string.push_str(
                        &args
                            .get_err(3, "alpha")?
                            .to_css_string(args.span(), parser.options.is_compressed())?,
                    );
                }
                string.push(')');
                return Ok(Value::String(string, QuoteKind::None));
            }
            v => {
                return Err((
                    format!("$blue: {} is not a number.", v.inspect(args.span())?),
                    args.span(),
                )
                    .into())
            }
        };
        let alpha = match args.default_arg(
            3,
            "alpha",
            Value::Dimension(Some(Number::one()), Unit::None, true),
        )? {
            Value::Dimension(Some(n), Unit::None, _) => n,
            Value::Dimension(Some(n), Unit::Percent, _) => n / Number::from(100),
            Value::Dimension(None, ..) => todo!(),
            v @ Value::Dimension(..) => {
                return Err((
                    format!(
                        "$alpha: Expected {} to have no units or \"%\".",
                        v.to_css_string(args.span(), parser.options.is_compressed())?
                    ),
                    args.span(),
                )
                    .into())
            }
            v if v.is_special_function() => {
                let string = format!(
                    "{}({}, {}, {}, {})",
                    name,
                    red.to_string(parser.options.is_compressed()),
                    green.to_string(parser.options.is_compressed()),
                    blue.to_string(parser.options.is_compressed()),
                    v.to_css_string(args.span(), parser.options.is_compressed())?
                );
                return Ok(Value::String(string, QuoteKind::None));
            }
            v => {
                return Err((
                    format!("$alpha: {} is not a number.", v.inspect(args.span())?),
                    args.span(),
                )
                    .into())
            }
        };
        Ok(Value::Color(Box::new(Color::from_rgba(
            red, green, blue, alpha,
        ))))
    }
}

pub(crate) fn rgb(args: CallArgs, parser: &mut Parser) -> SassResult<Value> {
    inner_rgb("rgb", args, parser)
}

pub(crate) fn rgba(args: CallArgs, parser: &mut Parser) -> SassResult<Value> {
    inner_rgb("rgba", args, parser)
}

pub(crate) fn red(mut args: CallArgs, parser: &mut Parser) -> SassResult<Value> {
    args.max_args(1)?;
    match args.get_err(0, "color")? {
        Value::Color(c) => Ok(Value::Dimension(Some(c.red()), Unit::None, true)),
        v => Err((
            format!("$color: {} is not a color.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn green(mut args: CallArgs, parser: &mut Parser) -> SassResult<Value> {
    args.max_args(1)?;
    match args.get_err(0, "color")? {
        Value::Color(c) => Ok(Value::Dimension(Some(c.green()), Unit::None, true)),
        v => Err((
            format!("$color: {} is not a color.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn blue(mut args: CallArgs, parser: &mut Parser) -> SassResult<Value> {
    args.max_args(1)?;
    match args.get_err(0, "color")? {
        Value::Color(c) => Ok(Value::Dimension(Some(c.blue()), Unit::None, true)),
        v => Err((
            format!("$color: {} is not a color.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn mix(mut args: CallArgs, parser: &mut Parser) -> SassResult<Value> {
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
        Value::Dimension(Some(Number::from(50)), Unit::None, true),
    )? {
        Value::Dimension(Some(n), u, _) => bound!(args, "weight", n, u, 0, 100) / Number::from(100),
        Value::Dimension(None, ..) => todo!(),
        v => {
            return Err((
                format!(
                    "$weight: {} is not a number.",
                    v.to_css_string(args.span(), parser.options.is_compressed())?
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
