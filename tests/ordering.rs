#[macro_use]
mod macros;

test!(
    two_greater_than_or_equal_one,
    "a {\n  color: 2 >= 1;\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    one_greater_than_or_equal_one,
    "a {\n  color: 1 >= 1;\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    zero_greater_than_or_equal_one,
    "a {\n  color: 0 >= 1;\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    two_greater_than_one,
    "a {\n  color: 2 > 1;\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    one_greater_than_one,
    "a {\n  color: 1 > 1;\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    zero_greater_than_one,
    "a {\n  color: 0 > 1;\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    two_less_than_or_equal_one,
    "a {\n  color: 2 <= 1;\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    one_less_than_or_equal_one,
    "a {\n  color: 1 <= 1;\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    zero_less_than_or_equal_one,
    "a {\n  color: 0 <= 1;\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    two_less_than_one,
    "a {\n  color: 2 < 1;\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    one_less_than_one,
    "a {\n  color: 1 < 1;\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    zero_less_than_one,
    "a {\n  color: 0 < 1;\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    ord_the_same_as_partial_ord,
    "a {\n  color: 2in > 1cm;\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    takes_into_account_different_units,
    "a {\n  color: 2in < 1cm;\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    infinity_gt_infinity,
    "a {\n  color: (1/0) > (1/0);\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    infinity_gt_neg_infinity,
    "a {\n  color: (1/0) > (-1/0);\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    nan_gt_nan,
    "a {\n  color: (0/0) > (0/0);\n}\n",
    "a {\n  color: false;\n}\n"
);
error!(
    strings_not_comparable,
    "a {\n  color: a > b;\n}\n", "Error: Undefined operation \"a > b\"."
);
error!(
    number_and_string_not_comparable,
    "a {\n  color: 1 > b;\n}\n", "Error: Undefined operation \"1 > b\"."
);
