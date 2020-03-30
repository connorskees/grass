#![cfg(test)]

#[macro_use]
mod macros;

test!(
    greater_than_or_equal_is_greater,
    "a {\n  color: 2 >= 1;\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    greater_than_or_equal_is_equal,
    "a {\n  color: 1 >= 1;\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    greater_than_or_equal_is_less,
    "a {\n  color: 0 >= 1;\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    greater_than_is_greater,
    "a {\n  color: 2 > 1;\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    greater_than_is_equal,
    "a {\n  color: 1 > 1;\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    greater_than_is_less,
    "a {\n  color: 0 > 1;\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    less_than_or_equal_is_greater,
    "a {\n  color: 2 <= 1;\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    less_than_or_equal_is_equal,
    "a {\n  color: 1 <= 1;\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    less_than_or_equal_is_less,
    "a {\n  color: 0 <= 1;\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    less_than_is_greater,
    "a {\n  color: 2 < 1;\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    less_than_is_equal,
    "a {\n  color: 1 < 1;\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    less_than_is_less,
    "a {\n  color: 0 < 1;\n}\n",
    "a {\n  color: true;\n}\n"
);
