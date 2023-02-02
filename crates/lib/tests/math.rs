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
    percentage_nan,
    "a {\n  color: percentage((0/0));\n}\n",
    "a {\n  color: NaN%;\n}\n"
);
test!(
    percentage_infinity,
    "a {\n  color: percentage((1/0));\n}\n",
    "a {\n  color: Infinity%;\n}\n"
);
test!(
    percentage_neg_infinity,
    "a {\n  color: percentage((-1/0));\n}\n",
    "a {\n  color: -Infinity%;\n}\n"
);
test!(
    integer_division,
    "a {\n  color: percentage(2);\n}\n",
    "a {\n  color: 200%;\n}\n"
);
test!(
    rounds_down,
    "a {\n  color: round(10.4px);\n}\n",
    "a {\n  color: 10px;\n}\n"
);
test!(
    rounds_up,
    "a {\n  color: round(10.6px);\n}\n",
    "a {\n  color: 11px;\n}\n"
);
test!(
    floor_below_pt_5,
    "a {\n  color: floor(10.4px);\n}\n",
    "a {\n  color: 10px;\n}\n"
);
test!(
    floor_above_pt_5,
    "a {\n  color: floor(10.6px);\n}\n",
    "a {\n  color: 10px;\n}\n"
);
test!(
    floor_big_int,
    "a {\n  color: floor(1.000000000000000001);\n}\n",
    "a {\n  color: 1;\n}\n"
);
test!(
    ceil_below_pt_5,
    "a {\n  color: ceil(10.4px);\n}\n",
    "a {\n  color: 11px;\n}\n"
);
test!(
    ceil_above_pt_5,
    "a {\n  color: ceil(10.6px);\n}\n",
    "a {\n  color: 11px;\n}\n"
);
test!(
    ceil_big_int,
    "a {\n  color: ceil(1.000000000000000001);\n}\n",
    "a {\n  color: 1;\n}\n"
);
test!(
    abs_positive,
    "a {\n  color: abs(10);\n}\n",
    "a {\n  color: 10;\n}\n"
);
test!(
    abs_negative,
    "a {\n  color: abs(-10);\n}\n",
    "a {\n  color: 10;\n}\n"
);
test!(
    abs_unit,
    "a {\n  color: abs(-10px);\n}\n",
    "a {\n  color: 10px;\n}\n"
);
test!(
    abs_nan,
    "a {\n  color: abs((0/0));\n}\n",
    "a {\n  color: NaN;\n}\n"
);
test!(
    abs_infinity,
    "a {\n  color: abs((1/0));\n}\n",
    "a {\n  color: Infinity;\n}\n"
);
test!(
    abs_neg_infinity,
    "a {\n  color: abs((-1/0));\n}\n",
    "a {\n  color: Infinity;\n}\n"
);
test!(
    comparable_unitless,
    "a {\n  color: comparable(1, 2);\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    comparable_none_px,
    "a {\n  color: comparable(1, 2px);\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    comparable_px_px,
    "a {\n  color: comparable(1px, 2px);\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    comparable_absolute,
    "a {\n  color: comparable(1px, 2in);\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    comparable_absolute_font_relative,
    "a {\n  color: comparable(1px, 2em);\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    comparable_named,
    "a {\n  color: comparable($number1: 1, $number2: 2);\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    #[cfg(feature = "random")]
    random_limit_one,
    "a {\n  color: random(1);\n}\n",
    "a {\n  color: 1;\n}\n"
);
error!(
    #[cfg(feature = "random")]
    random_limit_big_one,
    "a {\n  color: random(1000000000000000001 - 1000000000000000000);\n}\n",
    "Error: $limit: Must be greater than 0, was 0."
);
error!(
    percentage_non_number_arg,
    "a {\n  color: percentage(a);\n}\n", "Error: $number: a is not a number."
);
error!(
    round_non_number_arg,
    "a {\n  color: round(a);\n}\n", "Error: $number: a is not a number."
);
error!(
    ceil_non_number_arg,
    "a {\n  color: ceil(a);\n}\n", "Error: $number: a is not a number."
);
error!(
    floor_non_number_arg,
    "a {\n  color: floor(a);\n}\n", "Error: $number: a is not a number."
);
error!(
    abs_non_number_arg,
    "a {\n  color: abs(a);\n}\n", "Error: $number: a is not a number."
);
error!(
    comparable_non_number_arg_both,
    "a {\n  color: comparable(a, b);\n}\n", "Error: $number1: a is not a number."
);
error!(
    comparable_non_number_arg_first,
    "a {\n  color: comparable(a, 1);\n}\n", "Error: $number1: a is not a number."
);
error!(
    comparable_non_number_arg_last,
    "a {\n  color: comparable(1, b);\n}\n", "Error: $number2: b is not a number."
);
error!(
    percentage_no_args,
    "a {\n  color: percentage();\n}\n", "Error: Missing argument $number."
);
error!(
    round_no_args,
    "a {\n  color: round();\n}\n", "Error: Missing argument $number."
);
error!(
    ceil_no_args,
    "a {\n  color: ceil();\n}\n", "Error: Missing argument $number."
);
error!(
    floor_no_args,
    "a {\n  color: floor();\n}\n", "Error: Missing argument $number."
);
error!(
    abs_no_args,
    "a {\n  color: abs();\n}\n", "Error: Missing argument $number."
);
error!(
    comparable_no_args,
    "a {\n  color: comparable();\n}\n", "Error: Missing argument $number1."
);
error!(
    comparable_one_arg,
    "a {\n  color: comparable(1);\n}\n", "Error: Missing argument $number2."
);
