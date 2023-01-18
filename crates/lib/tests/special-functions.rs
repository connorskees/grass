#[macro_use]
mod macros;

test!(
    calc_whitespace,
    "a {\n  color: calc(       1      );\n}\n",
    "a {\n  color: 1;\n}\n"
);
error!(
    calc_newline,
    "a {\n  color: calc(\n);\n}\n", "Error: Expected number, variable, function, or calculation."
);
error!(
    calc_multiple_args,
    "a {\n  color: calc(1, 2, a, b, c);\n}\n", r#"Error: expected "+", "-", "*", "/", or ")"."#
);
test!(
    calc_does_evaluate_arithmetic,
    "a {\n  color: calc(1 + 2);\n}\n",
    "a {\n  color: 3;\n}\n"
);
test!(
    calc_operation_rhs_is_interpolation,
    "a {\n  color: calc(100% + (#{4px}));\n}\n",
    "a {\n  color: calc(100% + (4px));\n}\n"
);
test!(
    calc_mul_negative_number,
    "a {\n  color: calc(var(--bs-border-width) * -1);\n}\n",
    "a {\n  color: calc(var(--bs-border-width) * -1);\n}\n"
);
test!(
    calc_evaluates_interpolated_arithmetic,
    "a {\n  color: calc(#{1 + 2});\n}\n",
    "a {\n  color: calc(3);\n}\n"
);
error!(
    calc_retains_silent_comment,
    "a {\n  color: calc(//);\n}\n", "Error: Expected number, variable, function, or calculation."
);
error!(
    calc_retains_multiline_comment,
    "a {\n  color: calc(/**/);\n}\n", "Error: Expected number, variable, function, or calculation."
);
error!(
    calc_complex_unit,
    "a {\n  color: calc(1% + 1px * 2px);\n}\n",
    "Error: Number 2px*px isn't compatible with CSS calculations."
);
error!(
    calc_nested_parens,
    "a {\n  color: calc((((()))));\n}\n",
    "Error: Expected number, variable, function, or calculation."
);
test!(
    calc_invalid_arithmetic,
    "a {\n  color: calc(2px + 2px + 5%);\n}\n",
    "a {\n  color: calc(4px + 5%);\n}\n"
);
test!(
    calc_add_same_unit_opposite_sides_of_non_comparable_unit,
    "a {\n  color: calc(2px + 5% + 2px);\n}\n",
    "a {\n  color: calc(2px + 5% + 2px);\n}\n"
);
test!(
    calc_uppercase,
    "a {\n  color: CALC(1 + 1);\n}\n",
    "a {\n  color: 2;\n}\n"
);
test!(
    calc_mixed_casing,
    "a {\n  color: cAlC(1 + 1);\n}\n",
    "a {\n  color: 2;\n}\n"
);
test!(
    calc_browser_prefixed,
    "a {\n  color: -webkit-calc(1 + 2);\n}\n",
    "a {\n  color: -webkit-calc(1 + 2);\n}\n"
);
error!(
    calc_quoted_string,
    r#"a { color: calc("\ "); }"#, "Error: Expected number, variable, function, or calculation."
);
error!(
    calc_quoted_string_single_quoted_paren,
    r#"a {color: calc(")");}"#, "Error: Expected number, variable, function, or calculation."
);
error!(
    calc_quoted_string_single_quotes,
    "a {\n  color: calc('a');\n}\n", "Error: Expected number, variable, function, or calculation."
);
error!(
    calc_hash_no_interpolation,
    "a {\n  color: calc(#);\n}\n", "Error: Expected number, variable, function, or calculation."
);
error!(
    calc_boolean,
    "$a: true; a {\n  color: calc($a);\n}\n", "Error: Value true can't be used in a calculation."
);
test!(
    element_whitespace,
    "a {\n  color: element(       1      );\n}\n",
    "a {\n  color: element( 1 );\n}\n"
);
test!(
    element_newline,
    "a {\n  color: element(\n);\n}\n",
    "a {\n  color: element( );\n}\n"
);
test!(
    element_multiple_args,
    "a {\n  color: element(1, 2, a, b, c);\n}\n",
    "a {\n  color: element(1, 2, a, b, c);\n}\n"
);
test!(
    element_does_not_evaluate_arithmetic,
    "a {\n  color: element(1 + 2);\n}\n",
    "a {\n  color: element(1 + 2);\n}\n"
);
test!(
    element_evaluates_interpolated_arithmetic,
    "a {\n  color: element(#{1 + 2});\n}\n",
    "a {\n  color: element(3);\n}\n"
);
test!(
    element_retains_silent_comment,
    "a {\n  color: element(//);\n}\n",
    "a {\n  color: element(//);\n}\n"
);
test!(
    element_retains_multiline_comment,
    "a {\n  color: element(/**/);\n}\n",
    "a {\n  color: element(/**/);\n}\n"
);
test!(
    element_nested_parens,
    "a {\n  color: element((((()))));\n}\n",
    "a {\n  color: element((((()))));\n}\n"
);
test!(
    element_browser_prefixed,
    "a {\n  color: -webkit-element(1 + 2);\n}\n",
    "a {\n  color: -webkit-element(1 + 2);\n}\n"
);
test!(
    expression_whitespace,
    "a {\n  color: expression(       1      );\n}\n",
    "a {\n  color: expression( 1 );\n}\n"
);
test!(
    expression_newline,
    "a {\n  color: expression(\n);\n}\n",
    "a {\n  color: expression( );\n}\n"
);
test!(
    expression_multiple_args,
    "a {\n  color: expression(1, 2, a, b, c);\n}\n",
    "a {\n  color: expression(1, 2, a, b, c);\n}\n"
);
test!(
    expression_does_not_evaluate_arithmetic,
    "a {\n  color: expression(1 + 2);\n}\n",
    "a {\n  color: expression(1 + 2);\n}\n"
);
test!(
    expression_evaluates_interpolated_arithmetic,
    "a {\n  color: expression(#{1 + 2});\n}\n",
    "a {\n  color: expression(3);\n}\n"
);
test!(
    expression_retains_silent_comment,
    "a {\n  color: expression(//);\n}\n",
    "a {\n  color: expression(//);\n}\n"
);
test!(
    expression_retains_multiline_comment,
    "a {\n  color: expression(/**/);\n}\n",
    "a {\n  color: expression(/**/);\n}\n"
);
test!(
    expression_nested_parens,
    "a {\n  color: expression((((()))));\n}\n",
    "a {\n  color: expression((((()))));\n}\n"
);
test!(
    expression_browser_prefixed,
    "a {\n  color: -webkit-expression(1 + 2);\n}\n",
    "a {\n  color: -webkit-expression(1 + 2);\n}\n"
);
test!(
    progid_whitespace,
    "a {\n  color: progid:(       1      );\n}\n",
    "a {\n  color: progid:( 1 );\n}\n"
);
test!(
    progid_newline,
    "a {\n  color: progid:(\n);\n}\n",
    "a {\n  color: progid:( );\n}\n"
);
test!(
    progid_multiple_args,
    "a {\n  color: progid:(1, 2, a, b, c);\n}\n",
    "a {\n  color: progid:(1, 2, a, b, c);\n}\n"
);
test!(
    progid_does_not_evaluate_arithmetic,
    "a {\n  color: progid:(1 + 2);\n}\n",
    "a {\n  color: progid:(1 + 2);\n}\n"
);
test!(
    progid_evaluates_interpolated_arithmetic,
    "a {\n  color: progid:(#{1 + 2});\n}\n",
    "a {\n  color: progid:(3);\n}\n"
);
test!(
    progid_retains_silent_comment,
    "a {\n  color: progid:(//);\n}\n",
    "a {\n  color: progid:(//);\n}\n"
);
test!(
    progid_retains_multiline_comment,
    "a {\n  color: progid:(/**/);\n}\n",
    "a {\n  color: progid:(/**/);\n}\n"
);
test!(
    progid_nested_parens,
    "a {\n  color: progid:((((()))));\n}\n",
    "a {\n  color: progid:((((()))));\n}\n"
);
test!(
    progid_values_after_colon,
    "a {\n  color: progid:apple.bottoM..jeans.boots();\n}\n",
    "a {\n  color: progid:apple.bottoM..jeans.boots();\n}\n"
);
error!(
    progid_number_after_colon,
    "a {\n  color: progid:ap1ple.bottoM..jeans.boots();\n}\n", "Error: expected \"(\"."
);
test!(
    progid_uppercase,
    "a {\n  color: PROGID:foo(fff);\n}\n",
    "a {\n  color: progid:foo(fff);\n}\n"
);
test!(
    progid_mixed_casing,
    "a {\n  color: PrOgId:foo(fff);\n}\n",
    "a {\n  color: progid:foo(fff);\n}\n"
);
test!(
    calc_plus_minus,
    "a {\n  color: calc(1% + 3px - 2px);\n}\n",
    "a {\n  color: calc(1% + 3px - 2px);\n}\n"
);
test!(
    inspect_calc,
    "a {\n  color: inspect(calc(1% + 3px - 2px));\n}\n",
    "a {\n  color: calc(1% + 3px - 2px);\n}\n"
);
test!(
    calc_ne_number,
    "a {\n  color: calc(1% + 3px - 2px) == 1px;\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    calc_num_plus_interpolation,
    "a {\n  color: calc(1 + #{c});\n}\n",
    "a {\n  color: calc(1 + c);\n}\n"
);
test!(
    progid_no_colon,
    "a {\n  color: progid;\n}\n",
    "a {\n  color: progid;\n}\n"
);
test!(
    progid_no_colon_with_args,
    "a {\n  color: progid();\n}\n",
    "a {\n  color: progid();\n}\n"
);
test!(
    can_add_s_and_ms,
    "a {\n  color: calc(1s + 1ms);\n}\n",
    "a {\n  color: 1.001s;\n}\n"
);
test!(
    can_add_hz_and_khz,
    "a {\n  color: calc(1Hz + 1kHz);\n}\n",
    "a {\n  color: 1001Hz;\n}\n"
);
test!(
    can_add_dpi_and_dppx,
    "a {\n  color: calc(1dpi + 1dppx);\n}\n",
    "a {\n  color: 97dpi;\n}\n"
);
test!(
    ternary_inside_calc,
    "a {\n  color: calc(if(true, 1, unit(foo)));\n}\n",
    "a {\n  color: 1;\n}\n"
);
test!(
    retains_parens_around_var_in_calc,
    "a {\n  color: calc((var(--a)) + 1rem);\n}\n",
    "a {\n  color: calc((var(--a)) + 1rem);\n}\n"
);
test!(
    removes_superfluous_parens_around_function_call_in_calc,
    "a {\n  color: calc((foo(--a)) + 1rem);\n}\n",
    "a {\n  color: calc(foo(--a) + 1rem);\n}\n"
);
test!(
    calculation_inside_calc,
    "a {\n  color: calc(calc(1px + 1rem) * calc(2px - 2in));\n}\n",
    "a {\n  color: calc((1px + 1rem) * -190px);\n}\n"
);
error!(
    escaped_close_paren_inside_calc,
    "a {\n  color: calc(\\));\n}\n", r#"Error: Expected "(" or "."."#
);
error!(
    nothing_after_last_arg,
    "a { color: calc(1 + 1", r#"Error: expected "+", "-", "*", "/", or ")"."#
);
error!(
    progid_nothing_after,
    "a { color: progid:", "Error: expected \"(\"."
);
error!(
    calc_no_whitespace_between_operator,
    "a {\n  color: calc(1+1);\n}\n",
    r#"Error: "+" and "-" must be surrounded by whitespace in calculations."#
);
