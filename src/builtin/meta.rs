use super::GlobalFunctionMap;

use codemap::Spanned;

use super::{Builtin, GLOBAL_FUNCTIONS};
use crate::common::QuoteKind;
use crate::scope::global_var_exists;
use crate::unit::Unit;
use crate::value::{SassFunction, Value};

pub(crate) fn register(f: &mut GlobalFunctionMap) {
    f.insert(
        "if",
        Builtin::new(|mut args, scope, super_selector| {
            args.max_args(3)?;
            if arg!(args, scope, super_selector, 0, "condition").is_true(args.span())? {
                Ok(arg!(args, scope, super_selector, 1, "if-true"))
            } else {
                Ok(arg!(args, scope, super_selector, 2, "if-false"))
            }
        }),
    );
    f.insert(
        "feature-exists",
        Builtin::new(|mut args, scope, super_selector| {
            args.max_args(1)?;
            match arg!(args, scope, super_selector, 0, "feature") {
                Value::Ident(s, _) => Ok(match s.as_str() {
                    // A local variable will shadow a global variable unless
                    // `!global` is used.
                    "global-variable-shadowing" => Value::True,
                    // the @extend rule will affect selectors nested in pseudo-classes
                    // like :not()
                    "extend-selector-pseudoclass" => Value::False,
                    // Full support for unit arithmetic using units defined in the
                    // [Values and Units Level 3][] spec.
                    "units-level-3" => Value::True,
                    // The Sass `@error` directive is supported.
                    "at-error" => Value::True,
                    // The "Custom Properties Level 1" spec is supported. This means
                    // that custom properties are parsed statically, with only
                    // interpolation treated as SassScript.
                    "custom-property" => Value::False,
                    _ => Value::False,
                }),
                v => Err((
                    format!(
                        "$feature: {} is not a string.",
                        v.to_css_string(args.span())?
                    ),
                    args.span(),
                )
                    .into()),
            }
        }),
    );
    f.insert(
        "unit",
        Builtin::new(|mut args, scope, super_selector| {
            args.max_args(1)?;
            let unit = match arg!(args, scope, super_selector, 0, "number") {
                Value::Dimension(_, u) => u.to_string(),
                v => {
                    return Err((
                        format!(
                            "$number: {} is not a number.",
                            v.to_css_string(args.span())?
                        ),
                        args.span(),
                    )
                        .into())
                }
            };
            Ok(Value::Ident(unit, QuoteKind::Quoted))
        }),
    );
    f.insert(
        "type-of",
        Builtin::new(|mut args, scope, super_selector| {
            args.max_args(1)?;
            let value = arg!(args, scope, super_selector, 0, "value");
            Ok(Value::Ident(
                value.kind(args.span())?.to_owned(),
                QuoteKind::None,
            ))
        }),
    );
    f.insert(
        "unitless",
        Builtin::new(|mut args, scope, super_selector| {
            args.max_args(1)?;
            match arg!(args, scope, super_selector, 0, "number") {
                Value::Dimension(_, Unit::None) => Ok(Value::True),
                Value::Dimension(_, _) => Ok(Value::False),
                _ => Ok(Value::True),
            }
        }),
    );
    f.insert(
        "inspect",
        Builtin::new(|mut args, scope, super_selector| {
            args.max_args(1)?;
            Ok(Value::Ident(
                arg!(args, scope, super_selector, 0, "value").inspect(args.span())?,
                QuoteKind::None,
            ))
        }),
    );
    f.insert(
        "variable-exists",
        Builtin::new(|mut args, scope, super_selector| {
            args.max_args(1)?;
            match arg!(args, scope, super_selector, 0, "name") {
                Value::Ident(s, _) => Ok(Value::bool(scope.var_exists(&s))),
                v => Err((
                    format!("$name: {} is not a string.", v.to_css_string(args.span())?),
                    args.span(),
                )
                    .into()),
            }
        }),
    );
    f.insert(
        "global-variable-exists",
        Builtin::new(|mut args, scope, super_selector| {
            args.max_args(1)?;
            match arg!(args, scope, super_selector, 0, "name") {
                Value::Ident(s, _) => Ok(Value::bool(global_var_exists(&s))),
                v => Err((
                    format!("$name: {} is not a string.", v.to_css_string(args.span())?),
                    args.span(),
                )
                    .into()),
            }
        }),
    );
    f.insert(
        "mixin-exists",
        Builtin::new(|mut args, scope, super_selector| {
            args.max_args(2)?;
            match arg!(args, scope, super_selector, 0, "name") {
                Value::Ident(s, _) => Ok(Value::bool(scope.mixin_exists(&s))),
                v => Err((
                    format!("$name: {} is not a string.", v.to_css_string(args.span())?),
                    args.span(),
                )
                    .into()),
            }
        }),
    );
    f.insert(
        "function-exists",
        Builtin::new(|mut args, scope, super_selector| {
            args.max_args(2)?;
            match arg!(args, scope, super_selector, 0, "name") {
                Value::Ident(s, _) => Ok(Value::bool(
                    scope.fn_exists(&s) || GLOBAL_FUNCTIONS.contains_key(s.as_str()),
                )),
                v => Err((
                    format!("$name: {} is not a string.", v.to_css_string(args.span())?),
                    args.span(),
                )
                    .into()),
            }
        }),
    );
    f.insert(
        "get-function",
        Builtin::new(|mut args, scope, super_selector| {
            args.max_args(3)?;
            let name = match arg!(args, scope, super_selector, 0, "name") {
                Value::Ident(s, _) => s,
                v => {
                    return Err((
                        format!("$name: {} is not a string.", v.to_css_string(args.span())?),
                        args.span(),
                    )
                        .into())
                }
            };
            let css =
                arg!(args, scope, super_selector, 1, "css" = Value::False).is_true(args.span())?;
            let module = match arg!(args, scope, super_selector, 2, "module" = Value::Null) {
                Value::Ident(s, ..) => Some(s),
                Value::Null => None,
                v => {
                    return Err((
                        format!(
                            "$module: {} is not a string.",
                            v.to_css_string(args.span())?
                        ),
                        args.span(),
                    )
                        .into())
                }
            };

            if module.is_some() && css {
                return Err((
                    "$css and $module may not both be passed at once.",
                    args.span(),
                )
                    .into());
            }

            let func = match scope.get_fn(Spanned {
                node: name.clone(),
                span: args.span(),
            }) {
                Ok(f) => SassFunction::UserDefined(Box::new(f), name),
                Err(..) => match GLOBAL_FUNCTIONS.get(name.as_str()) {
                    Some(f) => SassFunction::Builtin(f.clone(), name),
                    None => {
                        return Err((format!("Function not found: {}", name), args.span()).into())
                    }
                },
            };

            Ok(Value::Function(func))
        }),
    );
    f.insert(
        "call",
        Builtin::new(|mut args, scope, super_selector| {
            let func = match arg!(args, scope, super_selector, 0, "function") {
                Value::Function(f) => f,
                v => {
                    return Err((
                        format!(
                            "$function: {} is not a function reference.",
                            v.to_css_string(args.span())?
                        ),
                        args.span(),
                    )
                        .into())
                }
            };
            func.call(args.decrement(), scope, super_selector)
        }),
    );
}
