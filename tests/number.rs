#![cfg(test)]

#[macro_use]
mod macros;

// this is `1` for node-sass, but .999999etc for web compiler
test!(
    precision_does_not_round_up,
    "a {\n  color: 0.99999999991;\n}\n",
    "a {\n  color: 0.9999999999;\n}\n"
);
// this is `1` for node-sass, but .999999etc for web compiler
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

test!(
    px_mod_px,
    "a {\n  color: 10px % 2px;\n}\n",
    "a {\n  color: 0px;\n}\n"
);
test!(
    px_mod_in,
    "a {\n  color: 10px % 2in;\n}\n",
    "a {\n  color: 10px;\n}\n"
);
test!(
    px_mod_none,
    "a {\n  color: 10px % 2;\n}\n",
    "a {\n  color: 0px;\n}\n"
);
test!(
    none_mod_px,
    "a {\n  color: 10 % 2px;\n}\n",
    "a {\n  color: 0px;\n}\n"
);
test!(
    none_mod_none,
    "a {\n  color: 10 % 2;\n}\n",
    "a {\n  color: 0;\n}\n"
);
test!(
    num_plus_div,
    "a {\n  color: 1 + 3/4;\n}\n",
    "a {\n  color: 1.75;\n}\n"
);
test!(
    negative_near_zero_no_sign,
    "a {\n  color: -0.000000000001;\n}\n",
    "a {\n  color: 0;\n}\n"
);
test!(
    equality_unit_conversions,
    "a {\n  color: 1in == 96px;\n}\n",
    "a {\n  color: true;\n}\n"
);
