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
    parse::Parser,
    value::Value,
};

fn load_css(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(2)?;
    todo!()
}

fn module_functions(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(2)?;
    todo!()
}

fn module_variables(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(2)?;
    todo!()
}

pub(crate) fn declare(f: &mut Module) {
    f.insert_builtin("call", call);
    f.insert_builtin("content-exists", content_exists);
    f.insert_builtin("feature-exists", feature_exists);
    f.insert_builtin("function-exists", function_exists);
    f.insert_builtin("get-function", get_function);
    f.insert_builtin("global-variable-exists", global_variable_exists);
    f.insert_builtin("inspect", inspect);
    f.insert_builtin("keywords", keywords);
    f.insert_builtin("mixin-exists", mixin_exists);
    f.insert_builtin("type-of", type_of);
    f.insert_builtin("variable-exists", variable_exists);
}
