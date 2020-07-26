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
