#![cfg(test)]

#[macro_use]
mod macros;

test!(
    clamp_in_the_middle,
    "@use 'sass:math';\na {\n  color: math.clamp(0, 1, 2);\n}\n",
    "a {\n  color: 1;\n}\n"
);
test!(
    clamp_first_is_bigger,
    "@use 'sass:math';\na {\n  color: math.clamp(2, 1, 0);\n}\n",
    "a {\n  color: 2;\n}\n"
);
test!(
    clamp_all_same_unit,
    "@use 'sass:math';\na {\n  color: math.clamp(0px, 1px, 2px);\n}\n",
    "a {\n  color: 1px;\n}\n"
);
test!(
    clamp_all_different_but_compatible_unit,
    "@use 'sass:math';\na {\n  color: math.clamp(0mm, 1cm, 2in);\n}\n",
    "a {\n  color: 1cm;\n}\n"
);
error!(
    clamp_only_min_has_no_unit,
    "@use 'sass:math';\na {\n  color: math.clamp(0, 1cm, 2in);\n}\n",
    "Error: $min is unitless but $number has unit cm. Arguments must all have units or all be unitless."
);
error!(
    clamp_only_number_has_no_unit,
    "@use 'sass:math';\na {\n  color: math.clamp(0mm, 1, 2in);\n}\n",
    "Error: $min has unit mm but $number is unitless. Arguments must all have units or all be unitless."
);
error!(
    clamp_only_max_has_no_unit,
    "@use 'sass:math';\na {\n  color: math.clamp(0mm, 1cm, 2);\n}\n",
    "Error: $min has unit mm but $max is unitless. Arguments must all have units or all be unitless."
);
test!(
    sqrt_zero,
    "@use 'sass:math';\na {\n  color: math.sqrt(0);\n}\n",
    "a {\n  color: 0;\n}\n"
);
test!(
    sqrt_small_positive,
    "@use 'sass:math';\na {\n  color: math.sqrt(99);\n}\n",
    "a {\n  color: 9.9498743711;\n}\n"
);
test!(
    sqrt_small_negative,
    "@use 'sass:math';\na {\n  color: math.sqrt(-99);\n}\n",
    "a {\n  color: NaN;\n}\n"
);
test!(
    sqrt_big_positive,
    "@use 'sass:math';\na {\n  color: math.sqrt(9999999999999999999999999999999999999999999999999);\n}\n",
    "a {\n  color: 3162277660168379038695424;\n}\n"
);
test!(
    sqrt_big_negative,
    "@use 'sass:math';\na {\n  color: math.sqrt(-9999999999999999999999999999999999999999999999999);\n}\n",
    "a {\n  color: NaN;\n}\n"
);
test!(
    sqrt_irrational,
    "@use 'sass:math';\na {\n  color: math.sqrt(2);\n}\n",
    "a {\n  color: 1.4142135624;\n}\n"
);
test!(
    sqrt_of_nan,
    "@use 'sass:math';\na {\n  color: math.sqrt((0 / 0));\n}\n",
    "a {\n  color: NaN;\n}\n"
);
error!(
    sqrt_with_units,
    "@use 'sass:math';\na {\n  color: math.sqrt(1px);\n}\n",
    "Error: $number: Expected 1px to have no units."
);
