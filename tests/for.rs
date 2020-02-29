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
