#![cfg(test)]

#[macro_use]
mod macros;

test!(
    percentage_decimal,
    "a {\n  color: percentage(0.2);\n}\n",
    "a {\n  color: 20%;\n}\n"
);
test!(
    percentage_division,
    "a {\n  color: percentage(100px / 50px);\n}\n",
    "a {\n  color: 200%;\n}\n"
);
test!(
    integer_division,
    "a {\n  color: percentage(2);\n}\n",
    "a {\n  color: 200%;\n}\n"
);
