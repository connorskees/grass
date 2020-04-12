use std::collections::HashMap;

use super::Builtin;
use crate::common::QuoteKind;
use crate::unit::Unit;
use crate::value::Number;
use crate::value::Value;

pub(crate) fn register(f: &mut HashMap<String, Builtin>) {
    f.insert(
        "alpha".to_owned(),
        Builtin::new(|mut args, scope, super_selector| {
            max_args!(args, 1);
            match arg!(args, scope, super_selector, 0, "color") {
                Value::Color(c) => Ok(Value::Dimension(c.alpha(), Unit::None)),
                v => Err((
                    format!("$color: {} is not a color.", v.to_css_string(args.span())?),
                    args.span(),
                )
                    .into()),
            }
        }),
    );
    f.insert(
        "opacity".to_owned(),
        Builtin::new(|mut args, scope, super_selector| {
            max_args!(args, 1);
            match arg!(args, scope, super_selector, 0, "color") {
                Value::Color(c) => Ok(Value::Dimension(c.alpha(), Unit::None)),
                Value::Dimension(num, unit) => Ok(Value::Ident(
                    format!("opacity({}{})", num, unit),
                    QuoteKind::None,
                )),
                v => Err((
                    format!("$color: {} is not a color.", v.to_css_string(args.span())?),
                    args.span(),
                )
                    .into()),
            }
        }),
    );
    f.insert(
        "opacify".to_owned(),
        Builtin::new(|mut args, scope, super_selector| {
            max_args!(args, 2);
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
                Value::Dimension(n, u) => bound!(args, "amount", n, u, 0, 1),
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
            Ok(Value::Color(color.fade_in(amount)))
        }),
    );
    f.insert(
        "fade-in".to_owned(),
        Builtin::new(|mut args, scope, super_selector| {
            max_args!(args, 2);
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
                Value::Dimension(n, u) => bound!(args, "amount", n, u, 0, 1),
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
            Ok(Value::Color(color.fade_in(amount)))
        }),
    );
    f.insert(
        "transparentize".to_owned(),
        Builtin::new(|mut args, scope, super_selector| {
            max_args!(args, 2);
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
                Value::Dimension(n, u) => bound!(args, "amount", n, u, 0, 1),
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
            Ok(Value::Color(color.fade_out(amount)))
        }),
    );
    f.insert(
        "fade-out".to_owned(),
        Builtin::new(|mut args, scope, super_selector| {
            max_args!(args, 2);
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
                Value::Dimension(n, u) => bound!(args, "amount", n, u, 0, 1),
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
            Ok(Value::Color(color.fade_out(amount)))
        }),
    );
}
