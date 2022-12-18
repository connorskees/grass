use crate::builtin::builtin_imports::*;

use crate::builtin::{
    meta::{
        call, content_exists, feature_exists, function_exists, get_function,
        global_variable_exists, inspect, keywords, mixin_exists, type_of, variable_exists,
    },
    modules::Module,
};

fn load_css(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Vec<Stmt>> {
    // args.max_args(2)?;

    // let span = args.span();

    // let url = match args.get_err(0, "module")? {
    //     Value::String(s, ..) => s,
    //     v => {
    //         return Err((
    //             format!("$module: {} is not a string.", v.inspect(span)?),
    //             span,
    //         )
    //             .into())
    //     }
    // };

    // let with = match args.default_arg(1, "with", Value::Null) {
    //     Value::Map(map) => Some(map),
    //     Value::Null => None,
    //     v => return Err((format!("$with: {} is not a map.", v.inspect(span)?), span).into()),
    // };

    // // todo: tests for `with`
    // if let Some(with) = with {
    //     let mut config = ModuleConfig::default();

    //     for (key, value) in with {
    //         let key = match key {
    //             Value::String(s, ..) => s,
    //             v => {
    //                 return Err((
    //                     format!("$with key: {} is not a string.", v.inspect(span)?),
    //                     span,
    //                 )
    //                     .into())
    //             }
    //         };

    //         config.insert(
    //             Spanned {
    //                 node: key.into(),
    //                 span,
    //             },
    //             value.span(span),
    //         )?;
    //     }

    //     let (_, stmts) = parser.load_module(&url, &mut config)?;

    //     Ok(stmts)
    // } else {
    //     parser.parser.parse_single_import(&url, span)
    // }
    todo!()
}

fn module_functions(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
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
        (*parser.env.modules)
            .borrow()
            .get(module.into(), args.span())?
            .functions(),
    ))
}

fn module_variables(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
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
        (*parser.env.modules)
            .borrow()
            .get(module.into(), args.span())?
            .variables(),
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
