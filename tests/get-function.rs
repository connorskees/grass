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
    inspect_builtin_function,
    "a {b: inspect(get-function(lighten));}",
    "a {\n  b: get-function(\"lighten\");\n}\n"
);
