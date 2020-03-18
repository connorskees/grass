#![cfg(test)]

#[macro_use]
mod macros;

// this is `1` for libsass
test!(
    precision_does_not_round_up,
    "a {\n  color: 0.99999999991;\n}\n",
    "a {\n  color: 0.9999999999;\n}\n"
);
// this is `1` for libsass
test!(
    precision_does_round_up,
    "a {\n  color: 1.00000000009;\n}\n",
    "a {\n  color: 1.0000000001;\n}\n"
);
test!(
    many_nines_becomes_one,
    "a {\n  color: 0.9999999999999999;\n}\n",
    "a {\n  color: 1;\n}\n"
);
test!(
    many_nines_becomes_one_neg,
    "a {\n  color: -0.9999999999999999;\n}\n",
    "a {\n  color: -1;\n}\n"
);
test!(
    negative_zero,
    "a {\n  color: -0;\n}\n",
    "a {\n  color: 0;\n}\n"
);
test!(
    decimal_is_zero,
    "a {\n  color: 1.0000;\n}\n",
    "a {\n  color: 1;\n}\n"
);
test!(many_nines_not_rounded, "a {\n  color: 0.999999;\n}\n");
test!(positive_integer, "a {\n  color: 1;\n}\n");
test!(negative_integer, "a {\n  color: -1;\n}\n");
test!(
    positive_float_no_leading_zero,
    "a {\n  color: .1;\n}\n",
    "a {\n  color: 0.1;\n}\n"
);
test!(
    negative_float_no_leading_zero,
    "a {\n  color: -.1;\n}\n",
    "a {\n  color: -0.1;\n}\n"
);
test!(positive_float_leading_zero, "a {\n  color: 0.1;\n}\n");
test!(negative_float_leading_zero, "a {\n  color: -0.1;\n}\n");
