#![cfg(test)]

#[macro_use]
mod macros;

test!(
    scoping_var_decl_inner_ruleset,
    "a {\n  $color: red;\n  b {\n    $color: blue;\n  }\n  color: $color;\n}\n",
    "a {\n  color: blue;\n}\n"
);
test!(
    basic_global,
    "a {\n  $color: red !global;\n}\n\nb {\n  color: $color;\n}\n",
    "b {\n  color: red;\n}\n"
);
