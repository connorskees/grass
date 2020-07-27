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
error!(
    cos_non_angle,
    "@use 'sass:math';\na {\n  color: math.cos(1px);\n}\n",
    "Error: $number: Expected 1px to be an angle."
);
test!(
    cos_small_degree,
    "@use 'sass:math';\na {\n  color: math.cos(1deg);\n}\n",
    "a {\n  color: 0.9998476952;\n}\n"
);
test!(
    cos_small_radian,
    "@use 'sass:math';\na {\n  color: math.cos(1rad);\n}\n",
    "a {\n  color: 0.5403023059;\n}\n"
);
test!(
    cos_small_no_unit,
    "@use 'sass:math';\na {\n  color: math.cos(1);\n}\n",
    "a {\n  color: 0.5403023059;\n}\n"
);
test!(
    cos_small_negative_degree,
    "@use 'sass:math';\na {\n  color: math.cos(-1deg);\n}\n",
    "a {\n  color: 0.9998476952;\n}\n"
);
test!(
    cos_small_negative_radian,
    "@use 'sass:math';\na {\n  color: math.cos(-1rad);\n}\n",
    "a {\n  color: 0.5403023059;\n}\n"
);
test!(
    cos_small_negative_no_unit,
    "@use 'sass:math';\na {\n  color: math.cos(-1);\n}\n",
    "a {\n  color: 0.5403023059;\n}\n"
);
test!(
    cos_pi,
    "@use 'sass:math';\na {\n  color: math.cos(math.$pi);\n}\n",
    "a {\n  color: -1;\n}\n"
);
test!(
    cos_two_pi,
    "@use 'sass:math';\na {\n  color: math.cos(2 * math.$pi);\n}\n",
    "a {\n  color: 1;\n}\n"
);
error!(
    sin_non_angle,
    "@use 'sass:math';\na {\n  color: math.sin(1px);\n}\n",
    "Error: $number: Expected 1px to be an angle."
);
test!(
    sin_small_degree,
    "@use 'sass:math';\na {\n  color: math.sin(1deg);\n}\n",
    "a {\n  color: 0.0174524064;\n}\n"
);
test!(
    sin_small_radian,
    "@use 'sass:math';\na {\n  color: math.sin(1rad);\n}\n",
    "a {\n  color: 0.8414709848;\n}\n"
);
test!(
    sin_small_no_unit,
    "@use 'sass:math';\na {\n  color: math.sin(1);\n}\n",
    "a {\n  color: 0.8414709848;\n}\n"
);
test!(
    sin_small_negative_degree,
    "@use 'sass:math';\na {\n  color: math.sin(-1deg);\n}\n",
    "a {\n  color: -0.0174524064;\n}\n"
);
test!(
    sin_small_negative_radian,
    "@use 'sass:math';\na {\n  color: math.sin(-1rad);\n}\n",
    "a {\n  color: -0.8414709848;\n}\n"
);
test!(
    sin_small_negative_no_unit,
    "@use 'sass:math';\na {\n  color: math.sin(-1);\n}\n",
    "a {\n  color: -0.8414709848;\n}\n"
);
test!(
    sin_pi,
    "@use 'sass:math';\na {\n  color: math.sin(math.$pi);\n}\n",
    "a {\n  color: 0;\n}\n"
);
test!(
    sin_two_pi,
    "@use 'sass:math';\na {\n  color: math.sin(2 * math.$pi);\n}\n",
    "a {\n  color: 0;\n}\n"
);
error!(
    tan_non_angle,
    "@use 'sass:math';\na {\n  color: math.tan(1px);\n}\n",
    "Error: $number: Expected 1px to be an angle."
);
test!(
    tan_small_degree,
    "@use 'sass:math';\na {\n  color: math.tan(1deg);\n}\n",
    "a {\n  color: 0.0174550649;\n}\n"
);
test!(
    tan_small_radian,
    "@use 'sass:math';\na {\n  color: math.tan(1rad);\n}\n",
    "a {\n  color: 1.5574077247;\n}\n"
);
test!(
    tan_small_no_unit,
    "@use 'sass:math';\na {\n  color: math.tan(1);\n}\n",
    "a {\n  color: 1.5574077247;\n}\n"
);
test!(
    tan_small_negative_degree,
    "@use 'sass:math';\na {\n  color: math.tan(-1deg);\n}\n",
    "a {\n  color: -0.0174550649;\n}\n"
);
test!(
    tan_small_negative_radian,
    "@use 'sass:math';\na {\n  color: math.tan(-1rad);\n}\n",
    "a {\n  color: -1.5574077247;\n}\n"
);
test!(
    tan_small_negative_no_unit,
    "@use 'sass:math';\na {\n  color: math.tan(-1);\n}\n",
    "a {\n  color: -1.5574077247;\n}\n"
);
test!(
    tan_pi,
    "@use 'sass:math';\na {\n  color: math.tan(math.$pi);\n}\n",
    "a {\n  color: 0;\n}\n"
);
test!(
    tan_two_pi,
    "@use 'sass:math';\na {\n  color: math.tan(2 * math.$pi);\n}\n",
    "a {\n  color: 0;\n}\n"
);

test!(
    acos_above_one,
    "@use 'sass:math';\na {\n  color: math.acos(2);\n}\n",
    "a {\n  color: NaNdeg;\n}\n"
);
test!(
    acos_below_negative_one,
    "@use 'sass:math';\na {\n  color: math.acos(-2);\n}\n",
    "a {\n  color: NaNdeg;\n}\n"
);
test!(
    acos_one,
    "@use 'sass:math';\na {\n  color: math.acos(1);\n}\n",
    "a {\n  color: 0deg;\n}\n"
);
test!(
    acos_negative_one,
    "@use 'sass:math';\na {\n  color: math.acos(-1);\n}\n",
    "a {\n  color: 180deg;\n}\n"
);
test!(
    acos_zero,
    "@use 'sass:math';\na {\n  color: math.acos(0);\n}\n",
    "a {\n  color: 90deg;\n}\n"
);
test!(
    acos_point_five,
    "@use 'sass:math';\na {\n  color: math.acos(.5);\n}\n",
    "a {\n  color: 60deg;\n}\n"
);
test!(
    acos_nan,
    "@use 'sass:math';\na {\n  color: math.acos((0 / 0));\n}\n",
    "a {\n  color: NaNdeg;\n}\n"
);
