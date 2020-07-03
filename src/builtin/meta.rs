use super::{Builtin, GlobalFunctionMap, GLOBAL_FUNCTIONS};

use codemap::Spanned;

use crate::{
    args::CallArgs,
    common::QuoteKind,
    error::SassResult,
    parse::Parser,
    unit::Unit,
    value::{SassFunction, Value},
};

fn if_(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(3)?;
    if parser.arg(&mut args, 0, "condition")?.is_true() {
        Ok(parser.arg(&mut args, 1, "if-true")?)
    } else {
        Ok(parser.arg(&mut args, 2, "if-false")?)
    }
}

fn feature_exists(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    match parser.arg(&mut args, 0, "feature")? {
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
            format!(
                "$feature: {} is not a string.",
                v.to_css_string(args.span())?
            ),
            args.span(),
        )
            .into()),
    }
}

fn unit(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    let unit = match parser.arg(&mut args, 0, "number")? {
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

fn type_of(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    let value = parser.arg(&mut args, 0, "value")?;
    Ok(Value::String(value.kind().to_owned(), QuoteKind::None))
}

fn unitless(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    #[allow(clippy::match_same_arms)]
    Ok(match parser.arg(&mut args, 0, "number")? {
        Value::Dimension(_, Unit::None) => Value::True,
        Value::Dimension(_, _) => Value::False,
        _ => Value::True,
    })
}

fn inspect(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    Ok(Value::String(
        parser
            .arg(&mut args, 0, "value")?
            .inspect(args.span())?
            .into_owned(),
        QuoteKind::None,
    ))
}

fn variable_exists(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    match parser.arg(&mut args, 0, "name")? {
        Value::String(s, _) => Ok(Value::bool(
            parser
                .scopes
                .last()
                .var_exists(&s.into(), parser.global_scope),
        )),
        v => Err((
            format!("$name: {} is not a string.", v.to_css_string(args.span())?),
            args.span(),
        )
            .into()),
    }
}

fn global_variable_exists(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    match parser.arg(&mut args, 0, "name")? {
        Value::String(s, _) => Ok(Value::bool(
            parser.global_scope.var_exists_no_global(&s.into()),
        )),
        v => Err((
            format!("$name: {} is not a string.", v.to_css_string(args.span())?),
            args.span(),
        )
            .into()),
    }
}

fn mixin_exists(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(2)?;
    match parser.arg(&mut args, 0, "name")? {
        Value::String(s, _) => Ok(Value::bool(
            parser.scopes.last().mixin_exists(&s, parser.global_scope),
        )),
        v => Err((
            format!("$name: {} is not a string.", v.to_css_string(args.span())?),
            args.span(),
        )
            .into()),
    }
}

fn function_exists(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(2)?;
    match parser.arg(&mut args, 0, "name")? {
        Value::String(s, _) => Ok(Value::bool(
            parser.scopes.last().fn_exists(&s, parser.global_scope),
        )),
        v => Err((
            format!("$name: {} is not a string.", v.to_css_string(args.span())?),
            args.span(),
        )
            .into()),
    }
}

fn get_function(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(3)?;
    let name = match parser.arg(&mut args, 0, "name")? {
        Value::String(s, _) => s,
        v => {
            return Err((
                format!("$name: {} is not a string.", v.to_css_string(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    let css = parser
        .default_arg(&mut args, 1, "css", Value::False)
        .is_true();
    let module = match parser.default_arg(&mut args, 2, "module", Value::Null) {
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

    let func = match parser.scopes.last().get_fn(
        Spanned {
            node: &name,
            span: args.span(),
        },
        parser.global_scope,
    ) {
        Ok(f) => SassFunction::UserDefined(Box::new(f), name.into()),
        Err(..) => match GLOBAL_FUNCTIONS.get(name.as_str()) {
            Some(f) => SassFunction::Builtin(f.clone(), name.into()),
            None => return Err((format!("Function not found: {}", name), args.span()).into()),
        },
    };

    Ok(Value::FunctionRef(func))
}

fn call(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    let func = match parser.arg(&mut args, 0, "function")? {
        Value::FunctionRef(f) => f,
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
    func.call(args.decrement(), parser)
}

#[allow(clippy::needless_pass_by_value)]
fn content_exists(args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(0)?;
    if !parser.in_mixin {
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
}
