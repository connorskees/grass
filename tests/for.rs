#![cfg(test)]

#[macro_use]
mod macros;

test!(
    for_1_through_3,
    "@for $i from 1 through 3 {\n  a {\n    color: $i;\n  }\n}\n",
    "a {\n  color: 1;\n}\n\na {\n  color: 2;\n}\n\na {\n  color: 3;\n}\n"
);
test!(
    for_1_to_3,
    "@for $i from 1 to 3 {\n  a {\n    color: $i;\n  }\n}\n",
    "a {\n  color: 1;\n}\n\na {\n  color: 2;\n}\n"
);
test!(
    for_3_through_1,
    "@for $i from 3 through 1 {\n  a {\n    color: $i;\n  }\n}\n",
    "a {\n  color: 3;\n}\n\na {\n  color: 2;\n}\n\na {\n  color: 1;\n}\n"
);
test!(
    for_3_to_1,
    "@for $i from 3 to 1 {\n  a {\n    color: $i;\n  }\n}\n",
    "a {\n  color: 3;\n}\n\na {\n  color: 2;\n}\n"
);
test!(
    for_var_through_var,
    "$a: 1;\n$b: 3;\n@for $x from $a through $b {\n  div {\n    color: $x;\n  }\n}\n",
    "div {\n  color: 1;\n}\n\ndiv {\n  color: 2;\n}\n\ndiv {\n  color: 3;\n}\n"
);
test!(
    for_var_decl,
    "@for $x from 1 to 3 {\n  $limit: $x;\n\n  a {\n    color: $limit;\n  }\n}\n",
    "a {\n  color: 1;\n}\n\na {\n  color: 2;\n}\n"
);
test!(
    for_styles,
    "a {\n  @for $i from 1 to 3 {\n    color: $i;\n  }\n}\n",
    "a {\n  color: 1;\n  color: 2;\n}\n"
);
test!(
    scope,
    "a {\n  $a: red;\n  @for $i from 1 to 3 {\n    $a: blue;\n  }\n  color: $a;\n}\n",
    "a {\n  color: blue;\n}\n"
);
test!(
    simple_return_in_function,
    "@function foo() {\n  @for $i from 1 to 2 {\n    @return $i;\n  }\n}\na {\n  color: foo();\n}\n",
    "a {\n  color: 1;\n}\n"
);
test!(
    return_gated_by_if_in_function,
    "@function foo() {\n  @for $i from 1 through 2 {\n    @if $i==2 {\n      @return $i;\n    }\n  }\n}\na {\n  color: foo();\n}\n",
    "a {\n  color: 2;\n}\n"
);
test!(
    inner_if,
    "a {\n  @for $i from 1 to 3 {\n    @if $i==2 {\n      color: red;\n    }\n  }\n}\n",
    "a {\n  color: red;\n}\n"
);
test!(
    from_negative_to_positive,
    "@for $i from -1 to 3 {\n    a {\n        color: red;\n    }\n}\n",
    "a {\n  color: red;\n}\n\na {\n  color: red;\n}\n\na {\n  color: red;\n}\n\na {\n  color: red;\n}\n"
);
test!(
    from_negative_to_negative,
    "@for $i from -1 to -3 {\n    a {\n        color: red;\n    }\n}\n",
    "a {\n  color: red;\n}\n\na {\n  color: red;\n}\n"
);
test!(
    variable_named_to_as_value,
    "$to: 0;

    @for $i from $to to 1 {
        a {
            color: red;
        }
    }",
    "a {\n  color: red;\n}\n"
);
