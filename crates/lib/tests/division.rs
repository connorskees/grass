#[macro_use]
mod macros;

test!(
    none_div_none,
    "a {\n  color: (35 / 7);\n}\n",
    "a {\n  color: 5;\n}\n"
);
test!(
    unit_div_none,
    "a {\n  color: (35% / 7);\n}\n",
    "a {\n  color: 5%;\n}\n"
);
test!(
    unit_div_unit,
    "a {\n  color: (35% / 7%);\n}\n",
    "a {\n  color: 5;\n}\n"
);
test!(
    unit_conversion,
    "a {\n  color: (35px / 7in);\n}\n",
    "a {\n  color: 0.0520833333;\n}\n"
);
test!(
    slash_after_comma,
    "a {\n  slash-after-comma: (1, / 2);\n}\n",
    "a {\n  slash-after-comma: 1, /2;\n}\n"
);
test!(
    num_div_space_list,
    "a {\n  color: 1 / (a b);\n}\n",
    "a {\n  color: 1/a b;\n}\n"
);
test!(
    num_div_comma_list,
    "a {\n  color: 1 / (a, b);\n}\n",
    "a {\n  color: 1/a, b;\n}\n"
);
test!(
    num_div_true,
    "a {\n  color: 1 / true;\n}\n",
    "a {\n  color: 1/true;\n}\n"
);
test!(
    num_div_false,
    "a {\n  color: 1 / false;\n}\n",
    "a {\n  color: 1/false;\n}\n"
);
test!(
    num_div_important,
    "a {\n  color: 1 / !important;\n}\n",
    "a {\n  color: 1/!important;\n}\n"
);
test!(
    num_div_null,
    "a {\n  color: 1 / null;\n}\n",
    "a {\n  color: 1/;\n}\n"
);
test!(
    num_div_named_color,
    "a {\n  color: 1 / red;\n}\n",
    "a {\n  color: 1/red;\n}\n"
);
test!(
    dblquoted_string_div_space_separated_list,
    "a {\n  color: \"foo\"/(a b);\n}\n",
    "a {\n  color: \"foo\"/a b;\n}\n"
);
test!(
    null_div_number,
    "a {\n  color: null / 1;\n}\n",
    "a {\n  color: /1;\n}\n"
);
test!(
    null_div_named_color,
    "a {\n  color: null / red;\n}\n",
    "a {\n  color: /red;\n}\n"
);
test!(
    null_div_hex_color,
    "a {\n  color: null / #f0f0f0;\n}\n",
    "a {\n  color: /#f0f0f0;\n}\n"
);
test!(
    named_color_div_null,
    "a {\n  color: red / null;\n}\n",
    "a {\n  color: red/;\n}\n"
);
test!(
    hex_color_div_null,
    "a {\n  color: #f0f0f0 / null;\n}\n",
    "a {\n  color: #f0f0f0/;\n}\n"
);
test!(
    null_div_dblquoted_string,
    "a {\n  color: null / \"foo\";\n}\n",
    "a {\n  color: /\"foo\";\n}\n"
);
test!(
    number_div_arglist,
    "@function foo($a...) {
        @return 1 / $a;
    }

    a {
        color: foo(a, b);
    }",
    "a {\n  color: 1/a, b;\n}\n"
);
test!(
    string_div_arglist,
    "@function foo($a...) {
        @return foo / $a;
    }

    a {
        color: foo(a, b);
    }",
    "a {\n  color: foo/a, b;\n}\n"
);
error!(
    string_div_map,
    "a {\n  color: foo / (a: b);\n}\n", "Error: (a: b) isn't a valid CSS value."
);
error!(
    string_div_function,
    "a {\n  color: foo / get-function(lighten);\n}\n",
    "Error: get-function(\"lighten\") isn't a valid CSS value."
);
error!(
    num_div_map,
    "a {\n  color: 1 / (a: b);\n}\n", "Error: (a: b) isn't a valid CSS value."
);
error!(
    num_div_function,
    "a {\n  color: 1 / get-function(lighten);\n}\n",
    "Error: get-function(\"lighten\") isn't a valid CSS value."
);
test!(
    does_not_eval_plain,
    "a {\n  color: 1 / 2;\n}\n",
    "a {\n  color: 1/2;\n}\n"
);
test!(
    does_eval_inside_parens,
    "a {\n  color: (1/2);\n}\n",
    "a {\n  color: 0.5;\n}\n"
);
test!(
    does_eval_when_one_is_calculated,
    "a {\n  color: (1*1) / 2;\n}\n",
    "a {\n  color: 0.5;\n}\n"
);
test!(
    does_not_eval_from_unary_minus,
    "a {\n  color: -1 / 2;\n}\n",
    "a {\n  color: -1/2;\n}\n"
);
test!(
    does_eval_from_variable,
    "$a: 1;\na {\n  color: $a / 2;\n}\n",
    "a {\n  color: 0.5;\n}\n"
);
test!(
    does_eval_single_number_in_parens,
    "a {\n  color: (1) / 2;\n}\n",
    "a {\n  color: 0.5;\n}\n"
);
test!(
    does_eval_function_call,
    "@function foo() {
        @return 1;
    }
    
    a {
        color: foo() / 2;
    }",
    "a {\n  color: 0.5;\n}\n"
);
test!(
    does_not_eval_chained_binop_division,
    "a {\n  color: 1 / 3 / 4;\n}\n",
    "a {\n  color: 1/3/4;\n}\n"
);
test!(
    long_as_slash_chain,
    "a {\n  color: 1/2/3/4/5/6/7/8/9;\n}\n",
    "a {\n  color: 1/2/3/4/5/6/7/8/9;\n}\n"
);
test!(
    does_not_eval_chained_binop_one_not_division,
    "a {\n  color: 1 + 3 / 4;\n}\n",
    "a {\n  color: 1.75;\n}\n"
);
test!(
    zero_div_zero_is_nan,
    "a {\n  color: (0 / 0);\n}\n",
    "a {\n  color: NaN;\n}\n"
);
test!(
    divide_two_calculations,
    "a {\n  color: (calc(1rem + 1px) / calc(1rem + 1px));\n}\n",
    "a {\n  color: calc(1rem + 1px)/calc(1rem + 1px);\n}\n"
);
test!(
    num_div_calculation,
    "a {\n  color: (1 / calc(1rem + 1px));\n}\n",
    "a {\n  color: 1/calc(1rem + 1px);\n}\n"
);
test!(
    calculation_div_null,
    "a {\n  color: (calc(1rem + 1px) / null);\n}\n",
    "a {\n  color: calc(1rem + 1px)/;\n}\n"
);
test!(
    calculation_div_dbl_quoted_string,
    "a {\n  color: (calc(1rem + 1px) / \"foo\");\n}\n",
    "a {\n  color: calc(1rem + 1px)/\"foo\";\n}\n"
);
test!(
    calculation_div_sgl_quoted_string,
    "a {\n  color: (calc(1rem + 1px) / 'foo');\n}\n",
    "a {\n  color: calc(1rem + 1px)/\"foo\";\n}\n"
);
test!(
    three_chain_ending_in_string_is_not_evaled,
    "a {\n  color: 1 / 2 / foo();\n}\n",
    "a {\n  color: 1/2/foo();\n}\n"
);
test!(
    evaluates_variable_in_each,
    "$x: a 3/4 b;

    a {
        @each $elem in $x {
            color: $elem;
        }
    }",
    "a {\n  color: a;\n  color: 0.75;\n  color: b;\n}\n"
);
test!(
    evaluates_multiple_variables_in_each,
    "$x: a 3/4;

    a {

        @each $a,
        $b in $x {
            color: $a;
        }
    }",
    "a {\n  color: a;\n  color: 0.75;\n}\n"
);
test!(
    not_evaluated_for_variable_as_map_value_in_list,
    "$a: 1 2/3 4;

    a {
        color: inspect((a: $a))
    }",
    "a {\n  color: (a: 1 2/3 4);\n}\n"
);
test!(
    is_evaluated_for_variable_as_map_value_alone,
    "$a: 2/3;

    a {
        color: inspect((a: $a))
    }",
    "a {\n  color: (a: 0.6666666667);\n}\n"
);
test!(
    quoted_string_div_calculation,
    "a {\n  color: \"\" / calc(1vh + 1px);\n}\n",
    "a {\n  color: \"\"/calc(1vh + 1px);\n}\n"
);
test!(
    unquoted_string_div_calculation,
    "a {\n  color: foo / calc(1vh + 1px);\n}\n",
    "a {\n  color: foo/calc(1vh + 1px);\n}\n"
);
error!(
    color_div_color,
    "a {\n  color: red / green;\n}\n", r#"Error: Undefined operation "red / green"."#
);
error!(
    color_div_number,
    "a {\n  color: red / 1;\n}\n", r#"Error: Undefined operation "red / 1"."#
);
