#![cfg(test)]

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
