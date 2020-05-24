use super::{Builtin, GlobalFunctionMap, GLOBAL_FUNCTIONS};

use codemap::Spanned;

use crate::args::CallArgs;
use crate::common::QuoteKind;
use crate::error::SassResult;
use crate::scope::global_var_exists;
use crate::scope::Scope;
use crate::selector::Selector;
use crate::unit::Unit;
use crate::value::{SassFunction, Value};

fn if_(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    args.max_args(3)?;
    if arg!(args, scope, super_selector, 0, "condition").is_true(args.span())? {
        Ok(arg!(args, scope, super_selector, 1, "if-true"))
    } else {
        Ok(arg!(args, scope, super_selector, 2, "if-false"))
    }
}

fn feature_exists(
    mut args: CallArgs,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<Value> {
    args.max_args(1)?;
    match arg!(args, scope, super_selector, 0, "feature") {
        #[allow(clippy::match_same_arms)]
        Value::String(s, _) => Ok(match s.as_str() {
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
}

fn unit(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
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
    Ok(Value::String(unit, QuoteKind::Quoted))
}

fn type_of(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    args.max_args(1)?;
    let value = arg!(args, scope, super_selector, 0, "value");
    Ok(Value::String(
        value.kind(args.span())?.to_owned(),
        QuoteKind::None,
    ))
}

fn unitless(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    args.max_args(1)?;
    Ok(match arg!(args, scope, super_selector, 0, "number") {
        Value::Dimension(_, Unit::None) => Value::True,
        Value::Dimension(_, _) => Value::False,
        _ => Value::True,
    })
}

fn inspect(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    args.max_args(1)?;
    Ok(Value::String(
        arg!(args, scope, super_selector, 0, "value")
            .inspect(args.span())?
            .into(),
        QuoteKind::None,
    ))
}

fn variable_exists(
    mut args: CallArgs,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<Value> {
    args.max_args(1)?;
    match arg!(args, scope, super_selector, 0, "name") {
        Value::String(s, _) => Ok(Value::bool(scope.var_exists(&s))),
        v => Err((
            format!("$name: {} is not a string.", v.to_css_string(args.span())?),
            args.span(),
        )
            .into()),
    }
}

fn global_variable_exists(
    mut args: CallArgs,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<Value> {
    args.max_args(1)?;
    match arg!(args, scope, super_selector, 0, "name") {
        Value::String(s, _) => Ok(Value::bool(global_var_exists(&s))),
        v => Err((
            format!("$name: {} is not a string.", v.to_css_string(args.span())?),
            args.span(),
        )
            .into()),
    }
}

fn mixin_exists(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    args.max_args(2)?;
    match arg!(args, scope, super_selector, 0, "name") {
        Value::String(s, _) => Ok(Value::bool(scope.mixin_exists(&s))),
        v => Err((
            format!("$name: {} is not a string.", v.to_css_string(args.span())?),
            args.span(),
        )
            .into()),
    }
}

fn function_exists(
    mut args: CallArgs,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<Value> {
    args.max_args(2)?;
    match arg!(args, scope, super_selector, 0, "name") {
        Value::String(s, _) => Ok(Value::bool(
            scope.fn_exists(&s) || GLOBAL_FUNCTIONS.contains_key(s.as_str()),
        )),
        v => Err((
            format!("$name: {} is not a string.", v.to_css_string(args.span())?),
            args.span(),
        )
            .into()),
    }
}

fn get_function(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    args.max_args(3)?;
    let name = match arg!(args, scope, super_selector, 0, "name") {
        Value::String(s, _) => s,
        v => {
            return Err((
                format!("$name: {} is not a string.", v.to_css_string(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    let css = arg!(args, scope, super_selector, 1, "css" = Value::False).is_true(args.span())?;
    let module = match arg!(args, scope, super_selector, 2, "module" = Value::Null) {
        Value::String(s, ..) => Some(s),
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
        node: &name,
        span: args.span(),
    }) {
        Ok(f) => SassFunction::UserDefined(Box::new(f), name.into()),
        Err(..) => match GLOBAL_FUNCTIONS.get(name.as_str()) {
            Some(f) => SassFunction::Builtin(f.clone(), name.into()),
            None => return Err((format!("Function not found: {}", name), args.span()).into()),
        },
    };

    Ok(Value::Function(func))
}

fn call(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
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
}

pub(crate) fn declare(f: &mut GlobalFunctionMap) {
    f.insert("if", Builtin::new(if_));
    f.insert("feature-exists", Builtin::new(feature_exists));
    f.insert("unit", Builtin::new(unit));
    f.insert("type-of", Builtin::new(type_of));
    f.insert("unitless", Builtin::new(unitless));
    f.insert("inspect", Builtin::new(inspect));
    f.insert("variable-exists", Builtin::new(variable_exists));
    f.insert(
        "global-variable-exists",
        Builtin::new(global_variable_exists),
    );
    f.insert("mixin-exists", Builtin::new(mixin_exists));
    f.insert("function-exists", Builtin::new(function_exists));
    f.insert("get-function", Builtin::new(get_function));
    f.insert("call", Builtin::new(call));
}
