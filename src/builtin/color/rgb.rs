use super::{Builtin, GlobalFunctionMap};

use num_traits::One;

use crate::{
    args::CallArgs,
    color::Color,
    common::QuoteKind,
    error::SassResult,
    parse::Parser,
    unit::Unit,
    value::{Number, Value},
};

/// name: Either `rgb` or `rgba` depending on the caller
// todo: refactor into smaller functions
#[allow(clippy::cognitive_complexity)]
fn inner_rgb(name: &'static str, mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    if args.is_empty() {
        return Err(("Missing argument $channels.", args.span()).into());
    }

    if args.len() == 1 {
        let mut channels = match parser.arg(&mut args, 0, "channels")? {
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

        let blue = match channels.pop() {
            Some(Value::Dimension(n, Unit::None)) => n,
            Some(Value::Dimension(n, Unit::Percent)) => (n / Number::from(100)) * Number::from(255),
            Some(v) if v.is_special_function() => {
                let green = channels.pop().unwrap();
                let red = channels.pop().unwrap();
                return Ok(Value::String(
                    format!(
                        "{}({}, {}, {})",
                        name,
                        red.to_css_string(args.span())?,
                        green.to_css_string(args.span())?,
                        v.to_css_string(args.span())?
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
            Some(Value::Dimension(n, Unit::None)) => n,
            Some(Value::Dimension(n, Unit::Percent)) => (n / Number::from(100)) * Number::from(255),
            Some(v) if v.is_special_function() => {
                let string = match channels.pop() {
                    Some(red) => format!(
                        "{}({}, {}, {})",
                        name,
                        red.to_css_string(args.span())?,
                        v.to_css_string(args.span())?,
                        blue
                    ),
                    None => format!("{}({} {})", name, v.to_css_string(args.span())?, blue),
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
            Some(Value::Dimension(n, Unit::None)) => n,
            Some(Value::Dimension(n, Unit::Percent)) => (n / Number::from(100)) * Number::from(255),
            Some(v) if v.is_special_function() => {
                return Ok(Value::String(
                    format!(
                        "{}({}, {}, {})",
                        name,
                        v.to_css_string(args.span())?,
                        green,
                        blue
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
        let color = match parser.arg(&mut args, 0, "color")? {
            Value::Color(c) => c,
            v if v.is_special_function() => {
                let alpha = parser.arg(&mut args, 1, "alpha")?;
                return Ok(Value::String(
                    format!(
                        "{}({}, {})",
                        name,
                        v.to_css_string(args.span())?,
                        alpha.to_css_string(args.span())?
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
        let alpha = match parser.arg(&mut args, 1, "alpha")? {
            Value::Dimension(n, Unit::None) => n,
            Value::Dimension(n, Unit::Percent) => n / Number::from(100),
            v @ Value::Dimension(..) => {
                return Err((
                    format!(
                        "$alpha: Expected {} to have no units or \"%\".",
                        v.to_css_string(args.span())?
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
                        color.red(),
                        color.green(),
                        color.blue(),
                        v.to_css_string(args.span())?
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
        let red = match parser.arg(&mut args, 0, "red")? {
            Value::Dimension(n, Unit::None) => n,
            Value::Dimension(n, Unit::Percent) => (n / Number::from(100)) * Number::from(255),
            v @ Value::Dimension(..) => {
                return Err((
                    format!(
                        "$red: Expected {} to have no units or \"%\".",
                        v.to_css_string(args.span())?
                    ),
                    args.span(),
                )
                    .into())
            }
            v if v.is_special_function() => {
                let green = parser.arg(&mut args, 1, "green")?;
                let blue = parser.arg(&mut args, 2, "blue")?;
                let mut string = format!(
                    "{}({}, {}, {}",
                    name,
                    v.to_css_string(args.span())?,
                    green.to_css_string(args.span())?,
                    blue.to_css_string(args.span())?
                );
                if !args.is_empty() {
                    string.push_str(", ");
                    string.push_str(
                        &parser
                            .arg(&mut args, 3, "alpha")?
                            .to_css_string(args.span())?,
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
        let green = match parser.arg(&mut args, 1, "green")? {
            Value::Dimension(n, Unit::None) => n,
            Value::Dimension(n, Unit::Percent) => (n / Number::from(100)) * Number::from(255),
            v @ Value::Dimension(..) => {
                return Err((
                    format!(
                        "$green: Expected {} to have no units or \"%\".",
                        v.to_css_string(args.span())?
                    ),
                    args.span(),
                )
                    .into())
            }
            v if v.is_special_function() => {
                let blue = parser.arg(&mut args, 2, "blue")?;
                let mut string = format!(
                    "{}({}, {}, {}",
                    name,
                    red,
                    v.to_css_string(args.span())?,
                    blue.to_css_string(args.span())?
                );
                if !args.is_empty() {
                    string.push_str(", ");
                    string.push_str(
                        &parser
                            .arg(&mut args, 3, "alpha")?
                            .to_css_string(args.span())?,
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
        let blue = match parser.arg(&mut args, 2, "blue")? {
            Value::Dimension(n, Unit::None) => n,
            Value::Dimension(n, Unit::Percent) => (n / Number::from(100)) * Number::from(255),
            v @ Value::Dimension(..) => {
                return Err((
                    format!(
                        "$blue: Expected {} to have no units or \"%\".",
                        v.to_css_string(args.span())?
                    ),
                    args.span(),
                )
                    .into())
            }
            v if v.is_special_function() => {
                let mut string = format!(
                    "{}({}, {}, {}",
                    name,
                    red,
                    green,
                    v.to_css_string(args.span())?
                );
                if !args.is_empty() {
                    string.push_str(", ");
                    string.push_str(
                        &parser
                            .arg(&mut args, 3, "alpha")?
                            .to_css_string(args.span())?,
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
        let alpha = match parser.default_arg(
            &mut args,
            3,
            "alpha",
            Value::Dimension(Number::one(), Unit::None),
        )? {
            Value::Dimension(n, Unit::None) => n,
            Value::Dimension(n, Unit::Percent) => n / Number::from(100),
            v @ Value::Dimension(..) => {
                return Err((
                    format!(
                        "$alpha: Expected {} to have no units or \"%\".",
                        v.to_css_string(args.span())?
                    ),
                    args.span(),
                )
                    .into())
            }
            v if v.is_special_function() => {
                let string = format!(
                    "{}({}, {}, {}, {})",
                    name,
                    red,
                    green,
                    blue,
                    v.to_css_string(args.span())?
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

fn rgb(args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    inner_rgb("rgb", args, parser)
}

fn rgba(args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    inner_rgb("rgba", args, parser)
}

fn red(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    match parser.arg(&mut args, 0, "color")? {
        Value::Color(c) => Ok(Value::Dimension(c.red(), Unit::None)),
        v => Err((
            format!("$color: {} is not a color.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

fn green(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    match parser.arg(&mut args, 0, "color")? {
        Value::Color(c) => Ok(Value::Dimension(c.green(), Unit::None)),
        v => Err((
            format!("$color: {} is not a color.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

fn blue(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    match parser.arg(&mut args, 0, "color")? {
        Value::Color(c) => Ok(Value::Dimension(c.blue(), Unit::None)),
        v => Err((
            format!("$color: {} is not a color.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

fn mix(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(3)?;
    let color1 = match parser.arg(&mut args, 0, "color1")? {
        Value::Color(c) => c,
        v => {
            return Err((
                format!("$color1: {} is not a color.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    let color2 = match parser.arg(&mut args, 1, "color2")? {
        Value::Color(c) => c,
        v => {
            return Err((
                format!("$color2: {} is not a color.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    let weight = match parser.default_arg(
        &mut args,
        2,
        "weight",
        Value::Dimension(Number::from(50), Unit::None),
    )? {
        Value::Dimension(n, u) => bound!(args, "weight", n, u, 0, 100) / Number::from(100),
        v => {
            return Err((
                format!(
                    "$weight: {} is not a number.",
                    v.to_css_string(args.span())?
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
