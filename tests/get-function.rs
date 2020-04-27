#![cfg(test)]

#[macro_use]
mod macros;

test!(
    different_function_same_body_not_equal,
    "@function user-defined() {@return null}
    $first-reference: get-function(user-defined);
    @function user-defined() {@return null}
    $second-reference: get-function(user-defined);
    a {b: $first-reference == $second-reference}",
    "a {\n  b: false;\n}\n"
);
test!(
    same_function_equal,
    "@function user-defined() {@return null}
    a {b: get-function(user-defined) == get-function(user-defined)}s",
    "a {\n  b: true;\n}\n"
);
test!(
    different_name_same_body_not_equal,
    "@function user-defined-1() {@return null}
     @function user-defined-2() {@return null}
     a {b: get-function(user-defined-1) == get-function(user-defined-2)}",
    "a {\n  b: false;\n}\n"
);
test!(
    type_of_user_defined_function,
    "@function user-defined() {@return null}
     a {b: type-of(get-function(user-defined));}",
    "a {\n  b: function;\n}\n"
);
test!(
    type_of_builtin_function,
    "a {b: type-of(get-function(lighten));}",
    "a {\n  b: function;\n}\n"
);
test!(
    same_builtin_function_is_equal,
    "a {b: get-function(lighten) == get-function(lighten);}",
    "a {\n  b: true;\n}\n"
);
test!(
    different_builtin_function_not_equal,
    "a {b: get-function(lighten) == get-function(darken);}",
    "a {\n  b: false;\n}\n"
);
test!(
    builtin_not_equal_to_user_defined,
    "@function user-defined() {\n  @return foo;\n}\n
    a {b: get-function(lighten) == get-function(user-defined);}",
    "a {\n  b: false;\n}\n"
);
test!(
    inspect_builtin_function,
    "a {b: inspect(get-function(lighten));}",
    "a {\n  b: get-function(\"lighten\");\n}\n"
);
test!(
    call_user_defined_no_args,
    "@function user-defined() {\n  @return foo;\n}\n
     a {b: call(get-function(user-defined));}",
    "a {\n  b: foo;\n}\n"
);
test!(
    call_user_defined_positional_args,
    "@function user-defined($a, $b) {\n  @return $a, $b;\n}\n
     a {b: call(get-function(user-defined), a, b);}",
    "a {\n  b: a, b;\n}\n"
);
test!(
    call_user_defined_keyword_args,
    "@function user-defined($a, $b) {\n  @return $a, $b;\n}\n
     a {b: call(get-function(user-defined), $a: a, $b: b);}",
    "a {\n  b: a, b;\n}\n"
);
test!(
    call_builtin_positional_args,
    "a {b: call(get-function(lighten), red, 5);}",
    "a {\n  b: #ff1a1a;\n}\n"
);
test!(
    call_builtin_keyword_args,
    "a {b: call(get-function(lighten), $color: red, $amount: 5);}",
    "a {\n  b: #ff1a1a;\n}\n"
);
test!(
    call_user_defined_super_selector,
    "@function user-defined() {\n  @return &;\n}\n
     a {b: call(get-function(user-defined));}",
    "a {\n  b: a;\n}\n"
);
error!(
    undefined_function,
    "a {color: get-function(foo);}", "Error: Function not found: foo"
);
error!(
    non_function_call,
    "a {color: call(4);}", "Error: $function: 4 is not a function reference."
);
