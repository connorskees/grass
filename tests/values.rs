#![cfg(test)]

#[macro_use]
mod macros;

test!(
    single_quote,
    "a {\n  color: 'foo';\n}\n",
    "a {\n  color: \"foo\";\n}\n"
);
test!(double_quote, "a {\n  color: \"foo\";\n}\n");
test!(comma_list_ident, "a {\n  color: foo, bar, baz;\n}\n");
test!(space_list_ident, "a {\n  color: foo bar baz;\n}\n");
test!(comma_list_number, "a {\n  color: 1, 2, 3;\n}\n");
test!(space_list_number, "a {\n  color: 1 2 3;\n}\n");
test!(comma_space_list_number, "a {\n  color: 1 1, 2 2, 3 3;\n}\n");
test!(preserves_keyword_true, "a {\n  color: true;\n}\n");
test!(preserves_keyword_false, "a {\n  color: false;\n}\n");
test!(
    does_not_preserve_keyword_null,
    "a {\n  color: null;\n}\n",
    ""
);
test!(preserves_keyword_auto, "a {\n  color: auto;\n}\n");
test!(preserves_keyword_initial, "a {\n  color: initial;\n}\n");
test!(preserves_keyword_infinity, "a {\n  color: infinity;\n}\n");
test!(preserves_keyword_not, "a {\n  color: not;\n}\n");
test!(preserves_keyword_and, "a {\n  color: and;\n}\n");
test!(preserves_keyword_or, "a {\n  color: or;\n}\n");
test!(preserves_keyword_unset, "a {\n  color: unset;\n}\n");
test!(preserves_keyword_nan, "a {\n  color: NaN;\n}\n");
test!(preserves_keyword_from, "a {\n  color: FRoM;\n}\n");
test!(preserves_keyword_to, "a {\n  color: To;\n}\n");
test!(preserves_keyword_through, "a {\n  color: ThRouGh;\n}\n");
test!(
    preserves_quotes,
    "a {\n  color: \"'foo' \\\"bar\\\"\";\n}\n"
);
test!(
    whitespace_space_list_number,
    "a {\n  color:  1  2  3  ;\n}\n",
    "a {\n  color: 1 2 3;\n}\n"
);
test!(
    whitespace_comma_list_number,
    "a {\n  color:  1 ,  2 ,  3  ;\n}\n",
    "a {\n  color: 1, 2, 3;\n}\n"
);
test!(number, "a {\n  color: 1;\n}\n");
test!(
    removes_paren_around_single_value,
    "a {\n  color: (foo);\n}\n",
    "a {\n  color: foo;\n}\n"
);
test!(
    undefined_function_call_is_ident,
    "a {\n  color: foo();\n}\n"
);
test!(hash_identifier_is_not_color, "a {\n  color: #foo;\n}\n");
test!(
    hash_identifier_is_string,
    "a {\n  color: type-of(#foo);\n}\n",
    "a {\n  color: string;\n}\n"
);
test!(
    adjacent_strings_get_spaced,
    "a {\n  color: \"f\"foo;\n}\n",
    "a {\n  color: \"f\" foo;\n}\n"
);
test!(
    escaped_backslash,
    "a {\n  color: \"\\\\\";\n}\n",
    "a {\n  color: \"\\\";\n}\n"
);
test!(
    double_quotes_when_containing_single_quote,
    "a {\n  color: '\\\'';\n}\n",
    "a {\n  color: \"'\";\n}\n"
);
test!(
    color_equals_color,
    "a {\n  color: red == red;\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    color_does_not_equal_color,
    "a {\n  color: red != red;\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    unquoted_ident_eq_unquoted_ident,
    "a {\n  color: foo == foo;\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    dblquoted_ident_eq_unquoted_ident,
    "a {\n  color: \"foo\" == foo;\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    dblquoted_ident_eq_sglquoted_ident,
    "a {\n  color: \"foo\" == 'foo';\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    many_parens,
    "a {\n  color: (((((red)))));\n}\n",
    "a {\n  color: red;\n}\n"
);
test!(
    negative_number_times_number,
    "a {\n  color: -1 * 2;\n}\n",
    "a {\n  color: -2;\n}\n"
);
error!(
    value_missing_closing_paren,
    "a {\n  color: (red;\n}\n", "Error: expected \")\"."
);
