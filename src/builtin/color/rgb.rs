use super::GlobalFunctionMap;

use num_traits::One;

use super::Builtin;
use crate::args::CallArgs;
use crate::color::Color;
use crate::common::QuoteKind;
use crate::error::SassResult;
use crate::scope::Scope;
use crate::selector::Selector;
use crate::unit::Unit;
use crate::value::{Number, Value};

/// name: Either `rgb` or `rgba` depending on the caller
fn inner_rgb(
    name: &'static str,
    mut args: CallArgs,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<Value> {
    if args.is_empty() {
        return Err(("Missing argument $channels.", args.span()).into());
    }

    if args.len() == 1 {
        let mut channels = match arg!(args, scope, super_selector, 0, "channels") {
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
                return Ok(Value::Ident(
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
                    format!("$blue: {} is not a number.", v.to_css_string(args.span())?),
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
                return Ok(Value::Ident(string, QuoteKind::None));
            }
            Some(v) => {
                return Err((
                    format!("$green: {} is not a number.", v.to_css_string(args.span())?),
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
                return Ok(Value::Ident(
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
                    format!("$red: {} is not a number.", v.to_css_string(args.span())?),
                    args.span(),
                )
                    .into())
            }
            None => return Err(("Missing element $red.", args.span()).into()),
        };

        let color = Color::from_rgba(red, green, blue, Number::one());

        Ok(Value::Color(Box::new(color)))
    } else if args.len() == 2 {
        let color = match arg!(args, scope, super_selector, 0, "color") {
            Value::Color(c) => c,
            v if v.is_special_function() => {
                let alpha = arg!(args, scope, super_selector, 1, "alpha");
                return Ok(Value::Ident(
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
                    format!("$color: {} is not a color.", v.to_css_string(args.span())?),
                    args.span(),
                )
                    .into())
            }
        };
        let alpha = match arg!(args, scope, super_selector, 1, "alpha") {
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
                return Ok(Value::Ident(
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
                    format!("$alpha: {} is not a number.", v.to_css_string(args.span())?),
                    args.span(),
                )
                    .into())
            }
        };
        Ok(Value::Color(Box::new(color.with_alpha(alpha))))
    } else {
        let red = match arg!(args, scope, super_selector, 0, "red") {
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
                let green = arg!(args, scope, super_selector, 1, "green");
                let blue = arg!(args, scope, super_selector, 2, "blue");
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
                        &arg!(args, scope, super_selector, 3, "alpha")
                            .to_css_string(args.span())?,
                    );
                }
                string.push(')');
                return Ok(Value::Ident(string, QuoteKind::None));
            }
            v => {
                return Err((
                    format!("$red: {} is not a number.", v.to_css_string(args.span())?),
                    args.span(),
                )
                    .into())
            }
        };
        let green = match arg!(args, scope, super_selector, 1, "green") {
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
                let blue = arg!(args, scope, super_selector, 2, "blue");
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
                        &arg!(args, scope, super_selector, 3, "alpha")
                            .to_css_string(args.span())?,
                    );
                }
                string.push(')');
                return Ok(Value::Ident(string, QuoteKind::None));
            }
            v => {
                return Err((
                    format!("$green: {} is not a number.", v.to_css_string(args.span())?),
                    args.span(),
                )
                    .into())
            }
        };
        let blue = match arg!(args, scope, super_selector, 2, "blue") {
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
                        &arg!(args, scope, super_selector, 3, "alpha")
                            .to_css_string(args.span())?,
                    );
                }
                string.push(')');
                return Ok(Value::Ident(string, QuoteKind::None));
            }
            v => {
                return Err((
                    format!("$blue: {} is not a number.", v.to_css_string(args.span())?),
                    args.span(),
                )
                    .into())
            }
        };
        let alpha = match arg!(
            args,
            scope,
            super_selector,
            3,
            "alpha" = Value::Dimension(Number::one(), Unit::None)
        ) {
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
                return Ok(Value::Ident(string, QuoteKind::None));
            }
            v => {
                return Err((
                    format!("$alpha: {} is not a number.", v.to_css_string(args.span())?),
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

fn rgb(args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    inner_rgb("rgb", args, scope, super_selector)
}

fn rgba(args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    inner_rgb("rgba", args, scope, super_selector)
}

fn red(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    args.max_args(1)?;
    match arg!(args, scope, super_selector, 0, "color") {
        Value::Color(c) => Ok(Value::Dimension(c.red(), Unit::None)),
        v => Err((
            format!("$color: {} is not a color.", v.to_css_string(args.span())?),
            args.span(),
        )
            .into()),
    }
}

fn green(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    args.max_args(1)?;
    match arg!(args, scope, super_selector, 0, "color") {
        Value::Color(c) => Ok(Value::Dimension(c.green(), Unit::None)),
        v => Err((
            format!("$color: {} is not a color.", v.to_css_string(args.span())?),
            args.span(),
        )
            .into()),
    }
}

fn blue(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    args.max_args(1)?;
    match arg!(args, scope, super_selector, 0, "color") {
        Value::Color(c) => Ok(Value::Dimension(c.blue(), Unit::None)),
        v => Err((
            format!("$color: {} is not a color.", v.to_css_string(args.span())?),
            args.span(),
        )
            .into()),
    }
}

fn mix(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    args.max_args(3)?;
    let color1 = match arg!(args, scope, super_selector, 0, "color1") {
        Value::Color(c) => c,
        v => {
            return Err((
                format!("$color1: {} is not a color.", v.to_css_string(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    let color2 = match arg!(args, scope, super_selector, 1, "color2") {
        Value::Color(c) => c,
        v => {
            return Err((
                format!("$color2: {} is not a color.", v.to_css_string(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    let weight = match arg!(
        args,
        scope,
        super_selector,
        2,
        "weight" = Value::Dimension(Number::from(50), Unit::None)
    ) {
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

pub(crate) fn register(f: &mut GlobalFunctionMap) {
    f.insert("rgb", Builtin::new(rgb));
    f.insert("rgba", Builtin::new(rgba));
    f.insert("red", Builtin::new(red));
    f.insert("green", Builtin::new(green));
    f.insert("blue", Builtin::new(blue));
    f.insert("mix", Builtin::new(mix));
}
