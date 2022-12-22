#[macro_use]
mod macros;

test!(
    min_not_evaluated_units_percent,
    "a {\n  color: min(1%, 2%);\n}\n",
    "a {\n  color: 1%;\n}\n"
);
test!(
    min_not_evaluated_units_px,
    "a {\n  color: min(1px, 2px);\n}\n",
    "a {\n  color: 1px;\n}\n"
);
test!(
    min_not_evaluated_no_units,
    "a {\n  color: min(1, 2);\n}\n",
    "a {\n  color: 1;\n}\n"
);
test!(
    min_not_evaluated_incompatible_units,
    "a {\n  color: min(1%, 2vh);\n}\n",
    "a {\n  color: min(1%, 2vh);\n}\n"
);
test!(
    min_not_evaluated_interpolation,
    "$a: 1%;\n$b: 2%;\na {\n  color: min(#{$a}, #{$b});;\n}\n",
    "a {\n  color: min(1%, 2%);\n}\n"
);
test!(
    min_evaluated_variable_units_percent,
    "$a: 1%;\n$b: 2%;\na {\n  color: min($a, $b);\n}\n",
    "a {\n  color: 1%;\n}\n"
);
test!(
    min_evaluated_variable_units_px,
    "$a: 1px;\n$b: 2px;\na {\n  color: min($a, $b);\n}\n",
    "a {\n  color: 1px;\n}\n"
);
error!(
    min_arg_of_incorrect_type,
    "$a: 1px;\n$b: 2px;\na {\n  color: min($a, $b, foo);\n}\n", "Error: foo is not a number."
);
error!(
    min_too_few_args,
    "a {\n  color: min();\n}\n", "Error: At least one argument must be passed."
);
test!(
    min_incompatible_units,
    "$a: 1px;\n$b: 2%;\na {\n  color: min($a, $b);\n}\n",
    "a {\n  color: min(1px, 2%);\n}\n"
);
test!(
    max_same_units_percent,
    "a {\n  color: max(1%, 2%);\n}\n",
    "a {\n  color: 2%;\n}\n"
);
test!(
    max_same_units_px,
    "a {\n  color: max(1px, 2px);\n}\n",
    "a {\n  color: 2px;\n}\n"
);
test!(
    max_same_units_none,
    "a {\n  color: max(1, 2);\n}\n",
    "a {\n  color: 2;\n}\n"
);
test!(
    max_uncomparable_but_compatible_units,
    "a {\n  color: max(1%, 2vh);\n}\n",
    "a {\n  color: max(1%, 2vh);\n}\n"
);
test!(
    max_not_evaluated_interpolation,
    "$a: 1%;\n$b: 2%;\na {\n  color: max(#{$a}, #{$b});\n}\n",
    "a {\n  color: max(1%, 2%);\n}\n"
);
test!(
    max_evaluated_variable_units_percent,
    "$a: 1%;\n$b: 2%;\na {\n  color: max($a, $b);\n}\n",
    "a {\n  color: 2%;\n}\n"
);
test!(
    max_evaluated_variable_units_px,
    "$a: 1px;\n$b: 2px;\na {\n  color: max($a, $b);\n}\n",
    "a {\n  color: 2px;\n}\n"
);
test!(
    max_evaluated_binop,
    "a {\n  color: max(100% - lightness(red) - 2%);\n}\n",
    "a {\n  color: 48%;\n}\n"
);
error!(
    max_arg_of_incorrect_type,
    "$a: 1px;\n$b: 2px;\na {\n  color: max($a, $b, foo);\n}\n", "Error: foo is not a number."
);
error!(
    max_too_few_args,
    "a {\n  color: max();\n}\n", "Error: At least one argument must be passed."
);
test!(
    max_incompatible_units,
    "$a: 1px;\n$b: 2%;\na {\n  color: max($a, $b);\n}\n",
    "a {\n  color: max(1px, 2%);\n}\n"
);
// todo: special functions, min(calc(1), $b);
test!(
    min_containing_max,
    "a {\n  color: min(1, max(2));\n}\n",
    "a {\n  color: 1;\n}\n"
);
test!(
    max_containing_min,
    "a {\n  color: max(1, min(2));\n}\n",
    "a {\n  color: 2;\n}\n"
);
test!(
    min_containing_max_as_only_arg,
    "a {\n  color: min(max(1px, 2px));\n}\n",
    "a {\n  color: 2px;\n}\n"
);
test!(
    max_containing_min_as_only_arg,
    "a {\n  color: max(min(1px, 2px));\n}\n",
    "a {\n  color: 1px;\n}\n"
);
test!(
    extremely_nested_min_and_max,
    "a {\n  color: min(max(min(max(min(min(1), max(2))))), min(max(min(3))));\n}\n",
    "a {\n  color: 1;\n}\n"
);
test!(
    decimal_without_leading_integer_is_evaluated,
    "a {\n  color: min(.2, .4);\n}\n",
    "a {\n  color: 0.2;\n}\n"
);
test!(
    decimal_with_leading_integer_is_not_evaluated,
    "a {\n  color: min(0.2, 0.4);\n}\n",
    "a {\n  color: 0.2;\n}\n"
);
test!(
    min_contains_special_fn_env,
    "a {\n  color: min(env(\"foo\"));\n}\n",
    "a {\n  color: min(env(\"foo\"));\n}\n"
);
test!(
    min_contains_special_fn_calc_with_div_and_spaces,
    "a {\n  color: min(calc(1 / 2));\n}\n",
    "a {\n  color: 0.5;\n}\n"
);
test!(
    min_contains_special_fn_calc_with_div_without_spaces,
    "a {\n  color: min(calc(1/2));\n}\n",
    "a {\n  color: 0.5;\n}\n"
);
error!(
    min_contains_special_fn_calc_with_plus_only,
    "a {\n  color: min(calc(+));\n}\n", "Error: Expected digit."
);
error!(
    min_contains_special_fn_calc_space_separated_list,
    "a {\n  color: min(calc(1  2));\n}\n", r#"Error: expected "+", "-", "*", "/", or ")"."#
);
test!(
    min_contains_special_fn_var,
    "a {\n  color: min(1, var(--foo));\n}\n",
    "a {\n  color: min(1, var(--foo));\n}\n"
);
test!(
    max_contains_special_fn_var,
    "a {\n  color: max(1, var(--foo));\n}\n",
    "a {\n  color: max(1, var(--foo));\n}\n"
);
test!(
    min_contains_multiline_comment,
    "a {\n  color: min(1/**/);\n}\n",
    "a {\n  color: 1;\n}\n"
);
error!(
    min_contains_calc_contains_multiline_comment,
    "a {\n  color: min(calc(1 /**/ 2));\n}\n", r#"Error: expected "+", "-", "*", "/", or ")"."#
);
test!(
    min_contains_calc_contains_multiline_comment_with_interpolation,
    "a {\n  color: min(calc(1 + /* #{5} */ 2));\n}\n",
    "a {\n  color: 3;\n}\n"
);
test!(
    min_uppercase,
    "a {\n  color: MIN(1);\n}\n",
    "a {\n  color: 1;\n}\n"
);
test!(
    max_uppercase,
    "a {\n  color: MAX(1);\n}\n",
    "a {\n  color: 1;\n}\n"
);
test!(
    min_parenthesis_around_arg,
    "a {\n  color: min((1));\n}\n",
    "a {\n  color: 1;\n}\n"
);
test!(
    max_compatible_units_does_conversion,
    "a {\n  color: max(1px, 1in, 1cm);\n}\n",
    "a {\n  color: 1in;\n}\n"
);
test!(
    min_compatible_units_does_conversion,
    "a {\n  color: min(1px, 1in, 1cm);\n}\n",
    "a {\n  color: 1px;\n}\n"
);
error!(
    min_parenthesis_around_arg_with_comma,
    "a {\n  color: min((1, 1));\n}\n", "Error: 1, 1 is not a number."
);
error!(
    min_hash_without_interpolation,
    "a {\n  color: min(#a);\n}\n", "Error: #a is not a number."
);
error!(
    min_calc_no_parens,
    "a {\n  color: min(calc);\n}\n", "Error: calc is not a number."
);
error!(
    min_env_no_parens,
    "a {\n  color: min(env);\n}\n", "Error: env is not a number."
);
error!(
    min_var_no_parens,
    "a {\n  color: min(var);\n}\n", "Error: var is not a number."
);
error!(
    min_min_unfinished,
    "a {\n  color: min(mi);\n}\n", "Error: mi is not a number."
);
error!(
    min_max_unfinished,
    "a {\n  color: min(ma);\n}\n", "Error: ma is not a number."
);
error!(
    min_min_no_parens,
    "a {\n  color: min(min);\n}\n", "Error: min is not a number."
);
error!(
    min_max_no_parens,
    "a {\n  color: min(max);\n}\n", "Error: max is not a number."
);
error!(
    min_min_invalid,
    "a {\n  color: min(min(#));\n}\n", "Error: Expected identifier."
);
error!(
    min_calc_parens_no_args,
    "a {\n  color: min(calc());\n}\n",
    "Error: Expected number, variable, function, or calculation."
);
