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
