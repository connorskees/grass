#[macro_use]
mod macros;

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
    zero_mod_zero,
    "a {\n  color: 0 % 0;\n}\n",
    "a {\n  color: NaN;\n}\n"
);
test!(
    positive_mod_zero,
    "a {\n  color: 1 % 0;\n}\n",
    "a {\n  color: NaN;\n}\n"
);
test!(
    positive_unit_mod_zero,
    "a {\n  color: 1px % 0;\n}\n",
    "a {\n  color: NaNpx;\n}\n"
);
test!(
    positive_mod_zero_unit,
    "a {\n  color: 1 % 0px;\n}\n",
    "a {\n  color: NaNpx;\n}\n"
);
test!(
    positive_unit_mod_zero_unit_same,
    "a {\n  color: 1px % 0px;\n}\n",
    "a {\n  color: NaNpx;\n}\n"
);
test!(
    positive_unit_mod_zero_unit_different_compatible_takes_first_1,
    "a {\n  color: 1px % 0in;\n}\n",
    "a {\n  color: NaNpx;\n}\n"
);
test!(
    positive_unit_mod_zero_unit_different_compatible_takes_first_2,
    "a {\n  color: 1in % 0px;\n}\n",
    "a {\n  color: NaNin;\n}\n"
);
error!(
    positive_unit_mod_zero_unit_incompatible_units,
    "a {\n  color: 1rem % 0px;\n}\n", "Error: Incompatible units rem and px."
);
test!(
    positive_mod_negative,
    "a {\n  color: 1 % -4;\n}\n",
    "a {\n  color: -3;\n}\n"
);
test!(
    negative_mod_positive,
    "a {\n  color: -4 % 3;\n}\n",
    "a {\n  color: 2;\n}\n"
);
test!(
    negative_mod_negative,
    "a {\n  color: -4 % -3;\n}\n",
    "a {\n  color: -1;\n}\n"
);
test!(
    big_negative_mod_positive,
    "a {\n  color: -99999990000099999999999999 % 2;\n}\n",
    "a {\n  color: 0;\n}\n"
);
test!(
    big_int_result_is_equal_to_small_int,
    "a {\n  color: (6 % 2) == 0;\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    comparable_units_denom_0,
    "a {\n  color: 1in % 0px;\n}\n",
    "a {\n  color: NaNin;\n}\n"
);
test!(
    comparable_units_negative_denom_0,
    "a {\n  color: -1in % 0px;\n}\n",
    "a {\n  color: NaNin;\n}\n"
);
test!(
    comparable_units_both_positive,
    "a {\n  color: 1in % 1px;\n}\n",
    "a {\n  color: 0in;\n}\n"
);
test!(
    comparable_units_denom_negative,
    "a {\n  color: 1in % -1px;\n}\n",
    "a {\n  color: -0.0104166667in;\n}\n"
);
test!(
    comparable_units_both_negative,
    "a {\n  color: -1in % -1px;\n}\n",
    "a {\n  color: 0in;\n}\n"
);
test!(
    comparable_units_numer_negative,
    "a {\n  color: -1in % 1px;\n}\n",
    "a {\n  color: 0.0104166667in;\n}\n"
);
test!(
    comparable_units_both_0,
    "a {\n  color: 0in % 0px;\n}\n",
    "a {\n  color: NaNin;\n}\n"
);
