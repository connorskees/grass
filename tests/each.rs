#![cfg(test)]

#[macro_use]
mod macros;

test!(
    each_space_separated_inner,
    "a {\n  @each $i in 1 2 3 {\n    color: $i;\n  }\n}\n",
    "a {\n  color: 1;\n  color: 2;\n  color: 3;\n}\n"
);
test!(
    each_space_separated_outer,
    "@each $i in 1 2 3 {\n  a {\n    color: $i;\n  }\n}\n",
    "a {\n  color: 1;\n}\n\na {\n  color: 2;\n}\n\na {\n  color: 3;\n}\n"
);
test!(
    each_two_variables_one_null,
    "a {\n  @each $i, $c in 1 2 3 {\n    color: $i;\n  }\n}\n",
    "a {\n  color: 1;\n  color: 2;\n  color: 3;\n}\n"
);
test!(
    each_one_var_in_one_map,
    "a {\n  @each $i in (a: b) {\n    color: $i;\n  }\n}\n",
    "a {\n  color: a b;\n}\n"
);
test!(
    each_two_vars_in_one_map,
    "a {\n  @each $i, $c in (a: b) {\n    color: $i;\n  }\n}\n",
    "a {\n  color: a;\n}\n"
);
// blocked on better parsing of comma and space separated lists
// test!(
//     each_two_vars_in_3_2_list,
//     "a {\n  @each $i, $c in (1 2 3, 4 5) {\n    color: $i, $c;\n  }\n}\n",
//     "a {\n  color: 1, 2;\n  color: 4, 5;\n}\n"
// );