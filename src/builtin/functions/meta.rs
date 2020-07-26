use super::{Builtin, GlobalFunctionMap, GLOBAL_FUNCTIONS};

use codemap::Spanned;

use crate::{
    args::CallArgs,
    common::{Identifier, QuoteKind},
    error::SassResult,
    parse::Parser,
    unit::Unit,
    value::{SassFunction, Value},
};

fn if_(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(3)?;
    if args.get_err(0, "condition")?.is_true() {
        Ok(args.get_err(1, "if-true")?)
    } else {
        Ok(args.get_err(2, "if-false")?)
    }
}

pub(crate) fn feature_exists(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    match args.get_err(0, "feature")? {
        #[allow(clippy::match_same_arms)]
        Value::String(s, _) => Ok(match s.as_str() {
            // A local variable will shadow a global variable unless
            // `!global` is used.
            "global-variable-shadowing" => Value::True,
            // the @extend rule will affect selectors nested in pseudo-classes
            // like :not()
            "extend-selector-pseudoclass" => Value::True,
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
            format!("$feature: {} is not a string.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn unit(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    let unit = match args.get_err(0, "number")? {
        Value::Dimension(_, u, _) => u.to_string(),
        v => {
            return Err((
                format!("$number: {} is not a number.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::String(unit, QuoteKind::Quoted))
}

pub(crate) fn type_of(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    let value = args.get_err(0, "value")?;
    Ok(Value::String(value.kind().to_owned(), QuoteKind::None))
}

pub(crate) fn unitless(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    #[allow(clippy::match_same_arms)]
    Ok(match args.get_err(0, "number")? {
        Value::Dimension(_, Unit::None, _) => Value::True,
        Value::Dimension(..) => Value::False,
        _ => Value::True,
    })
}

pub(crate) fn inspect(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    Ok(Value::String(
        args.get_err(0, "value")?.inspect(args.span())?.into_owned(),
        QuoteKind::None,
    ))
}

pub(crate) fn variable_exists(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    match args.get_err(0, "name")? {
        Value::String(s, _) => Ok(Value::bool(
            parser.scopes.var_exists(s.into(), parser.global_scope),
        )),
        v => Err((
            format!("$name: {} is not a string.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn global_variable_exists(
    mut args: CallArgs,
    parser: &mut Parser<'_>,
) -> SassResult<Value> {
    args.max_args(1)?;
    match args.get_err(0, "name")? {
        Value::String(s, _) => Ok(Value::bool(parser.global_scope.var_exists(s.into()))),
        v => Err((
            format!("$name: {} is not a string.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

// todo: should check for module arg (as well as several others)
pub(crate) fn mixin_exists(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(2)?;
    match args.get_err(0, "name")? {
        Value::String(s, _) => Ok(Value::bool(
            parser.scopes.mixin_exists(s.into(), parser.global_scope),
        )),
        v => Err((
            format!("$name: {} is not a string.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn function_exists(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(2)?;
    match args.get_err(0, "name")? {
        Value::String(s, _) => Ok(Value::bool(
            parser.scopes.fn_exists(s.into(), parser.global_scope),
        )),
        v => Err((
            format!("$name: {} is not a string.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn get_function(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(3)?;
    let name: Identifier = match args.get_err(0, "name")? {
        Value::String(s, _) => s.into(),
        v => {
            return Err((
                format!("$name: {} is not a string.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    let css = args.default_arg(1, "css", Value::False)?.is_true();
    let module = match args.default_arg(2, "module", Value::Null)? {
        Value::String(s, ..) => Some(s),
        Value::Null => None,
        v => {
            return Err((
                format!("$module: {} is not a string.", v.inspect(args.span())?),
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

    let func = match parser.scopes.get_fn(
        Spanned {
            node: name,
            span: args.span(),
        },
        parser.global_scope,
    ) {
        Some(f) => SassFunction::UserDefined(Box::new(f), name),
        None => match GLOBAL_FUNCTIONS.get(name.as_str()) {
            Some(f) => SassFunction::Builtin(f.clone(), name),
            None => return Err((format!("Function not found: {}", name), args.span()).into()),
        },
    };

    Ok(Value::FunctionRef(func))
}

pub(crate) fn call(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    let func = match args.get_err(0, "function")? {
        Value::FunctionRef(f) => f,
        v => {
            return Err((
                format!(
                    "$function: {} is not a function reference.",
                    v.inspect(args.span())?
                ),
                args.span(),
            )
                .into())
        }
    };
    func.call(args.decrement(), parser)
}

#[allow(clippy::needless_pass_by_value)]
pub(crate) fn content_exists(args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(0)?;
    if !parser.flags.in_mixin() {
        return Err((
            "content-exists() may only be called within a mixin.",
            parser.span_before,
        )
            .into());
    }
    Ok(Value::bool(
        parser.content.last().map_or(false, |c| c.content.is_some()),
    ))
}

#[allow(dead_code, unused_mut, unused_variables)]
pub(crate) fn keywords(args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    todo!("builtin function `keywords` blocked on better handling of call args")
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
    f.insert("content-exists", Builtin::new(content_exists));
    f.insert("keywords", Builtin::new(keywords));
}
