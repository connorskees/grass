#![cfg(test)]

#[macro_use]
mod macros;

test!(
    inner_increment_var,
    "$a: 4;\n$b: 1;\na {\n  @while $a > $b {\n    color: $b;\n    $b: $b + 1;\n  }\n}",
    "a {\n  color: 1;\n  color: 2;\n  color: 3;\n}\n"
);
test!(
    outer_increment_var,
    "$a: 4;\n$b: 1;\n@while $a > $b {\na {\n    color: $b;\n  }\n  $b: $b + 1;\n}",
    "a {\n  color: 1;\n}\n\na {\n  color: 2;\n}\n\na {\n  color: 3;\n}\n"
);
test!(
    inner_while_false,
    "a {\n  @while false {\n    color: foo;\n  }\n}",
    ""
);
test!(
    outer_while_false,
    "@while false {\na {\n    color: $b;\n  }\n  $b: $b + 1;\n}",
    ""
);
error!(
    while_empty_condition,
    "@while {}", "Error: Expected expression."
);
