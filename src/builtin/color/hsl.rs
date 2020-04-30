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

fn hsl(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
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

        let lightness = match channels.pop() {
            Some(Value::Dimension(n, _)) => n / Number::from(100),
            Some(v) => {
                return Err((
                    format!(
                        "$lightness: {} is not a number.",
                        v.to_css_string(args.span())?
                    ),
                    args.span(),
                )
                    .into())
            }
            None => return Err(("Missing element $lightness.", args.span()).into()),
        };

        let saturation = match channels.pop() {
            Some(Value::Dimension(n, _)) => n / Number::from(100),
            Some(v) => {
                return Err((
                    format!(
                        "$saturation: {} is not a number.",
                        v.to_css_string(args.span())?
                    ),
                    args.span(),
                )
                    .into())
            }
            None => return Err(("Missing element $saturation.", args.span()).into()),
        };

        let hue = match channels.pop() {
            Some(Value::Dimension(n, _)) => n,
            Some(v) => {
                return Err((
                    format!("$hue: {} is not a number.", v.to_css_string(args.span())?),
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
        let hue = match arg!(args, scope, super_selector, 0, "hue") {
            Value::Dimension(n, _) => n,
            v if v.is_special_function() => {
                let saturation = arg!(args, scope, super_selector, 1, "saturation");
                let lightness = arg!(args, scope, super_selector, 2, "lightness");
                let mut string = format!(
                    "hsl({}, {}, {}",
                    v.to_css_string(args.span())?,
                    saturation.to_css_string(args.span())?,
                    lightness.to_css_string(args.span())?
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
                    format!("$hue: {} is not a number.", v.to_css_string(args.span())?),
                    args.span(),
                )
                    .into())
            }
        };
        let saturation = match arg!(args, scope, super_selector, 1, "saturation") {
            Value::Dimension(n, _) => n / Number::from(100),
            v if v.is_special_function() => {
                let lightness = arg!(args, scope, super_selector, 2, "lightness");
                let mut string = format!(
                    "hsl({}, {}, {}",
                    hue,
                    v.to_css_string(args.span())?,
                    lightness.to_css_string(args.span())?
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
                    format!(
                        "$saturation: {} is not a number.",
                        v.to_css_string(args.span())?
                    ),
                    args.span(),
                )
                    .into())
            }
        };
        let lightness = match arg!(args, scope, super_selector, 2, "lightness") {
            Value::Dimension(n, _) => n / Number::from(100),
            v if v.is_special_function() => {
                let mut string = format!(
                    "hsl({}, {}, {}",
                    hue,
                    saturation,
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
                    format!(
                        "$lightness: {} is not a number.",
                        v.to_css_string(args.span())?
                    ),
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
                return Ok(Value::Ident(
                    format!(
                        "hsl({}, {}, {}, {})",
                        hue,
                        saturation,
                        lightness,
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
        Ok(Value::Color(Box::new(Color::from_hsla(
            hue, saturation, lightness, alpha,
        ))))
    }
}

fn hsla(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
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

        let lightness = match channels.pop() {
            Some(Value::Dimension(n, _)) => n / Number::from(100),
            Some(v) => {
                return Err((
                    format!(
                        "$lightness: {} is not a number.",
                        v.to_css_string(args.span())?
                    ),
                    args.span(),
                )
                    .into())
            }
            None => return Err(("Missing element $lightness.", args.span()).into()),
        };

        let saturation = match channels.pop() {
            Some(Value::Dimension(n, _)) => n / Number::from(100),
            Some(v) => {
                return Err((
                    format!(
                        "$saturation: {} is not a number.",
                        v.to_css_string(args.span())?
                    ),
                    args.span(),
                )
                    .into())
            }
            None => return Err(("Missing element $saturation.", args.span()).into()),
        };

        let hue = match channels.pop() {
            Some(Value::Dimension(n, _)) => n,
            Some(v) => {
                return Err((
                    format!("$hue: {} is not a number.", v.to_css_string(args.span())?),
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
        let hue = match arg!(args, scope, super_selector, 0, "hue") {
            Value::Dimension(n, _) => n,
            v if v.is_special_function() => {
                let saturation = arg!(args, scope, super_selector, 1, "saturation");
                let lightness = arg!(args, scope, super_selector, 2, "lightness");
                let mut string = format!(
                    "hsla({}, {}, {}",
                    v.to_css_string(args.span())?,
                    saturation.to_css_string(args.span())?,
                    lightness.to_css_string(args.span())?
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
                    format!("$hue: {} is not a number.", v.to_css_string(args.span())?),
                    args.span(),
                )
                    .into())
            }
        };
        let saturation = match arg!(args, scope, super_selector, 1, "saturation") {
            Value::Dimension(n, _) => n / Number::from(100),
            v if v.is_special_function() => {
                let lightness = arg!(args, scope, super_selector, 2, "lightness");
                let mut string = format!(
                    "hsla({}, {}, {}",
                    hue,
                    v.to_css_string(args.span())?,
                    lightness.to_css_string(args.span())?
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
                    format!(
                        "$saturation: {} is not a number.",
                        v.to_css_string(args.span())?
                    ),
                    args.span(),
                )
                    .into())
            }
        };
        let lightness = match arg!(args, scope, super_selector, 2, "lightness") {
            Value::Dimension(n, _) => n / Number::from(100),
            v if v.is_special_function() => {
                let mut string = format!(
                    "hsla({}, {}, {}",
                    hue,
                    saturation,
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
                    format!(
                        "$lightness: {} is not a number.",
                        v.to_css_string(args.span())?
                    ),
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
                return Ok(Value::Ident(
                    format!(
                        "hsl({}, {}, {}, {})",
                        hue,
                        saturation,
                        lightness,
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
        Ok(Value::Color(Box::new(Color::from_hsla(
            hue, saturation, lightness, alpha,
        ))))
    }
}

fn hue(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    args.max_args(1)?;
    match arg!(args, scope, super_selector, 0, "color") {
        Value::Color(c) => Ok(Value::Dimension(c.hue(), Unit::Deg)),
        v => Err((
            format!("$color: {} is not a color.", v.to_css_string(args.span())?),
            args.span(),
        )
            .into()),
    }
}

fn saturation(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    args.max_args(1)?;
    match arg!(args, scope, super_selector, 0, "color") {
        Value::Color(c) => Ok(Value::Dimension(c.saturation(), Unit::Percent)),
        v => Err((
            format!("$color: {} is not a color.", v.to_css_string(args.span())?),
            args.span(),
        )
            .into()),
    }
}

fn lightness(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    args.max_args(1)?;
    match arg!(args, scope, super_selector, 0, "color") {
        Value::Color(c) => Ok(Value::Dimension(c.lightness(), Unit::Percent)),
        v => Err((
            format!("$color: {} is not a color.", v.to_css_string(args.span())?),
            args.span(),
        )
            .into()),
    }
}

fn adjust_hue(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    args.max_args(2)?;
    let color = match arg!(args, scope, super_selector, 0, "color") {
        Value::Color(c) => c,
        v => {
            return Err((
                format!("$color: {} is not a color.", v.to_css_string(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    let degrees = match arg!(args, scope, super_selector, 1, "degrees") {
        Value::Dimension(n, _) => n,
        v => {
            return Err((
                format!(
                    "$degrees: {} is not a number.",
                    v.to_css_string(args.span())?
                ),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::Color(Box::new(color.adjust_hue(degrees))))
}

fn lighten(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    args.max_args(2)?;
    let color = match arg!(args, scope, super_selector, 0, "color") {
        Value::Color(c) => c,
        v => {
            return Err((
                format!("$color: {} is not a color.", v.to_css_string(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    let amount = match arg!(args, scope, super_selector, 1, "amount") {
        Value::Dimension(n, u) => bound!(args, "amount", n, u, 0, 100) / Number::from(100),
        v => {
            return Err((
                format!(
                    "$amount: {} is not a number.",
                    v.to_css_string(args.span())?
                ),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::Color(Box::new(color.lighten(amount))))
}

fn darken(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    args.max_args(2)?;
    let color = match arg!(args, scope, super_selector, 0, "color") {
        Value::Color(c) => c,
        v => {
            return Err((
                format!("$color: {} is not a color.", v.to_css_string(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    let amount = match arg!(args, scope, super_selector, 1, "amount") {
        Value::Dimension(n, u) => bound!(args, "amount", n, u, 0, 100) / Number::from(100),
        v => {
            return Err((
                format!(
                    "$amount: {} is not a number.",
                    v.to_css_string(args.span())?
                ),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::Color(Box::new(color.darken(amount))))
}

fn saturate(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    args.max_args(2)?;
    if args.len() == 1 {
        return Ok(Value::Ident(
            format!(
                "saturate({})",
                arg!(args, scope, super_selector, 0, "amount").to_css_string(args.span())?
            ),
            QuoteKind::None,
        ));
    }

    let amount = match arg!(args, scope, super_selector, 1, "amount") {
        Value::Dimension(n, u) => bound!(args, "amount", n, u, 0, 100) / Number::from(100),
        v => {
            return Err((
                format!(
                    "$amount: {} is not a number.",
                    v.to_css_string(args.span())?
                ),
                args.span(),
            )
                .into())
        }
    };
    let color = match arg!(args, scope, super_selector, 0, "color") {
        Value::Color(c) => c,
        Value::Dimension(n, u) => {
            return Ok(Value::Ident(
                format!("saturate({}{})", n, u),
                QuoteKind::None,
            ))
        }
        v => {
            return Err((
                format!("$color: {} is not a color.", v.to_css_string(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::Color(Box::new(color.saturate(amount))))
}

fn desaturate(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    args.max_args(2)?;
    let color = match arg!(args, scope, super_selector, 0, "color") {
        Value::Color(c) => c,
        v => {
            return Err((
                format!("$color: {} is not a color.", v.to_css_string(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    let amount = match arg!(args, scope, super_selector, 1, "amount") {
        Value::Dimension(n, u) => bound!(args, "amount", n, u, 0, 100) / Number::from(100),
        v => {
            return Err((
                format!(
                    "$amount: {} is not a number.",
                    v.to_css_string(args.span())?
                ),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::Color(Box::new(color.desaturate(amount))))
}

fn grayscale(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    args.max_args(1)?;
    let color = match arg!(args, scope, super_selector, 0, "color") {
        Value::Color(c) => c,
        Value::Dimension(n, u) => {
            return Ok(Value::Ident(
                format!("grayscale({}{})", n, u),
                QuoteKind::None,
            ))
        }
        v => {
            return Err((
                format!("$color: {} is not a color.", v.to_css_string(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::Color(Box::new(color.desaturate(Number::one()))))
}

fn complement(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    args.max_args(1)?;
    let color = match arg!(args, scope, super_selector, 0, "color") {
        Value::Color(c) => c,
        v => {
            return Err((
                format!("$color: {} is not a color.", v.to_css_string(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::Color(Box::new(color.complement())))
}

fn invert(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    args.max_args(2)?;
    let weight = match arg!(
        args,
        scope,
        super_selector,
        1,
        "weight" = Value::Dimension(Number::from(100), Unit::Percent)
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
    match arg!(args, scope, super_selector, 0, "color") {
        Value::Color(c) => Ok(Value::Color(Box::new(c.invert(weight)))),
        Value::Dimension(n, Unit::Percent) => {
            Ok(Value::Ident(format!("invert({}%)", n), QuoteKind::None))
        }
        Value::Dimension(..) => Err((
            "Only one argument may be passed to the plain-CSS invert() function.",
            args.span(),
        )
            .into()),
        v => Err((
            format!("$color: {} is not a color.", v.to_css_string(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn register(f: &mut GlobalFunctionMap) {
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
