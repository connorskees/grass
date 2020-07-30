#![cfg(test)]

use std::io::Write;

#[macro_use]
mod macros;

test!(
    module_functions_builtin,
    "@use 'sass:meta';\na {\n  color: inspect(meta.module-functions(meta));\n}\n",
    "a {\n  color: (\"feature-exists\": get-function(\"feature-exists\"), \"inspect\": get-function(\"inspect\"), \"type-of\": get-function(\"type-of\"), \"keywords\": get-function(\"keywords\"), \"global-variable-exists\": get-function(\"global-variable-exists\"), \"variable-exists\": get-function(\"variable-exists\"), \"function-exists\": get-function(\"function-exists\"), \"mixin-exists\": get-function(\"mixin-exists\"), \"content-exists\": get-function(\"content-exists\"), \"module-variables\": get-function(\"module-variables\"), \"module-functions\": get-function(\"module-functions\"), \"get-function\": get-function(\"get-function\"), \"call\": get-function(\"call\"));\n}\n"
);
test!(
    module_variables_builtin,
    "@use 'sass:meta';\n@use 'sass:math';\na {\n  color: inspect(meta.module-variables(math));\n}\n",
    "a {\n  color: (\"e\": 2.7182818285, \"pi\": 3.1415926536);\n}\n"
);
test!(
    global_var_exists_module,
    "@use 'sass:math';\na {\n  color: global-variable-exists(pi, $module: math);\n}\n",
    "a {\n  color: true;\n}\n"
);

#[test]
fn mixin_exists_module() {
    let input = "@use \"mixin_exists_module\" as module;\na {\n color: mixin-exists(foo, $module: module);\n}";
    tempfile!("mixin_exists_module.scss", "@mixin foo {}");
    assert_eq!(
        "a {\n  color: true;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default()).expect(input)
    );
}
