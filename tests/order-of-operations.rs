#![cfg(test)]

#[macro_use]
mod macros;

test!(
    addition_then_division,
    "a {\n  color: 3+3/4;\n}\n",
    "a {\n  color: 3.75;\n}\n"
);
test!(
    division_then_addition,
    "a {\n  color: 3/4 + 3;\n}\n",
    "a {\n  color: 3.75;\n}\n"
);
test!(
    addition_then_multiplication,
    "a {\n  color: 4 + 2 * 3;\n}\n",
    "a {\n  color: 10;\n}\n"
);
test!(
    multiplication_then_addition,
    "a {\n  color: 4 * 2 + 3;\n}\n",
    "a {\n  color: 11;\n}\n"
);
test!(
    comparison,
    "a {\n  color: 1 < 1 and 1 < 1;;\n}\n",
    "a {\n  color: false;\n}\n"
);
