use crate::{
    args::CallArgs,
    builtin::{
        meta::{
            call, content_exists, feature_exists, function_exists, get_function,
            global_variable_exists, inspect, keywords, mixin_exists, type_of, variable_exists,
        },
        modules::Module,
    },
    error::SassResult,
    parse::{Parser, Stmt},
    value::Value,
};

fn load_css(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Vec<Stmt>> {
    args.max_args(2)?;

    // todo: https://github.com/sass/dart-sass/issues/1054
    let url = match args.get_err(0, "module")? {
        Value::String(s, ..) => s,
        v => {
            return Err((
                format!("$module: {} is not a string.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    let with = match args.default_arg(1, "with", Value::Null)? {
        Value::Map(map) => Some(map),
        Value::Null => None,
        v => {
            return Err((
                format!("$with: {} is not a map.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    if let Some(with) = with {
        todo!("`$with` to `load-css` not yet implemented")
    } else {
        parser.parse_single_import(&url, args.span())
    }
}

fn module_functions(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;

    let module = match args.get_err(0, "module")? {
        Value::String(s, ..) => s,
        v => {
            return Err((
                format!("$module: {} is not a string.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    Ok(Value::Map(
        parser.modules.get(module.into(), args.span())?.functions(),
    ))
}

fn module_variables(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;

    let module = match args.get_err(0, "module")? {
        Value::String(s, ..) => s,
        v => {
            return Err((
                format!("$module: {} is not a string.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    Ok(Value::Map(
        parser.modules.get(module.into(), args.span())?.variables(),
    ))
}

pub(crate) fn declare(f: &mut Module) {
    f.insert_builtin("feature-exists", feature_exists);
    f.insert_builtin("inspect", inspect);
    f.insert_builtin("type-of", type_of);
    f.insert_builtin("keywords", keywords);
    f.insert_builtin("global-variable-exists", global_variable_exists);
    f.insert_builtin("variable-exists", variable_exists);
    f.insert_builtin("function-exists", function_exists);
    f.insert_builtin("mixin-exists", mixin_exists);
    f.insert_builtin("content-exists", content_exists);
    f.insert_builtin("module-variables", module_variables);
    f.insert_builtin("module-functions", module_functions);
    f.insert_builtin("get-function", get_function);
    f.insert_builtin("call", call);

    f.insert_builtin_mixin("load-css", load_css);
}
