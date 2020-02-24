#![cfg(test)]

#[macro_use]
mod macros;

test!(single_quote, "a {\n  color: 'foo';\n}\n");
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
    removes_paren_around_space_list,
    "a {\n  color: (foo bar);\n}\n",
    "a {\n  color: foo bar;\n}\n"
);
test!(
    removes_paren_around_item_in_list,
    "a {\n  color: 1 (foo bar);\n}\n",
    "a {\n  color: 1 foo bar;\n}\n"
);
test!(
    adds_idents,
    "a {\n  color: foo + bar;\n}\n",
    "a {\n  color: foobar;\n}\n"
);
test!(
    adds_dbl_quoted_idents,
    "a {\n  color: \"foo\" + \"bar\";\n}\n",
    "a {\n  color: \"foobar\";\n}\n"
);
test!(
    adds_sgl_quoted_idents,
    "a {\n  color: 'foo' + 'bar';\n}\n",
    "a {\n  color: \"foobar\";\n}\n"
);
test!(
    adds_dbl_and_un_quoted_idents,
    "a {\n  color: \"foo\" + bar;\n}\n",
    "a {\n  color: \"foobar\";\n}\n"
);
test!(
    adds_sgl_and_un_quoted_idents,
    "a {\n  color: 'foo' + bar;\n}\n",
    "a {\n  color: \"foobar\";\n}\n"
);
test!(
    adds_un_and_dbl_quoted_idents,
    "a {\n  color: foo + \"bar\";\n}\n",
    "a {\n  color: \"foobar\";\n}\n"
);
test!(
    adds_un_and_sgl_quoted_idents,
    "a {\n  color: foo + 'bar';\n}\n",
    "a {\n  color: \"foobar\";\n}\n"
);
test!(
    adds_sgl_and_dbl_quoted_idents,
    "a {\n  color: 'foo' + \"bar\";\n}\n",
    "a {\n  color: \"foobar\";\n}\n"
);
test!(
    adds_dbl_and_sgl_quoted_idents,
    "a {\n  color: \"foo\" + 'bar';\n}\n",
    "a {\n  color: \"foobar\";\n}\n"
);
test!(
    adds_ident_true,
    "a {\n  color: foo + true;\n}\n",
    "a {\n  color: footrue;\n}\n"
);
test!(
    adds_dbl_quoted_ident_true,
    "a {\n  color: \"foo\" + true;\n}\n",
    "a {\n  color: \"footrue\";\n}\n"
);
test!(
    adds_ident_false,
    "a {\n  color: foo + false;\n}\n",
    "a {\n  color: foofalse;\n}\n"
);
test!(
    adds_dbl_quoted_ident_false,
    "a {\n  color: \"foo\" + false;\n}\n",
    "a {\n  color: \"foofalse\";\n}\n"
);
test!(
    adds_ident_important,
    "a {\n  color: foo + !important;\n}\n",
    "a {\n  color: foo!important;\n}\n"
);
test!(
    adds_ident_null,
    "a {\n  color: foo + null;\n}\n",
    "a {\n  color: foo;\n}\n"
);
test!(
    adds_dbl_quoted_ident_null,
    "a {\n  color: \"foo\" + null;\n}\n",
    "a {\n  color: \"foo\";\n}\n"
);
test!(
    adds_sgl_quoted_ident_null,
    "a {\n  color: 'foo' + null;\n}\n",
    "a {\n  color: \"foo\";\n}\n"
);
test!(
    adds_ident_number,
    "a {\n  color: foo + 1;\n}\n",
    "a {\n  color: foo1;\n}\n"
);
test!(
    adds_dbl_quoted_ident_number,
    "a {\n  color: \"foo\" + 1;\n}\n",
    "a {\n  color: \"foo1\";\n}\n"
);
test!(
    adds_sgl_quoted_ident_number,
    "a {\n  color: 'foo' + 1;\n}\n",
    "a {\n  color: \"foo1\";\n}\n"
);
test!(
    adds_ident_dimension,
    "a {\n  color: foo + 1px;\n}\n",
    "a {\n  color: foo1px;\n}\n"
);
test!(
    adds_dbl_quoted_ident_dimension,
    "a {\n  color: \"foo\" + 1px;\n}\n",
    "a {\n  color: \"foo1px\";\n}\n"
);
test!(
    adds_sgl_quoted_ident_dimension,
    "a {\n  color: 'foo' + 1px;\n}\n",
    "a {\n  color: \"foo1px\";\n}\n"
);
test!(
    adds_true_false,
    "a {\n  color: true + false;\n}\n",
    "a {\n  color: truefalse;\n}\n"
);
test!(
    adds_false_null_is_string,
    "a {\n  color: if(false+null, 1, 2);\n}\n",
    "a {\n  color: 1;\n}\n"
);
test!(
    adds_null_num_is_string,
    "a {\n  color: null + 1;\n}\n",
    "a {\n  color: 1;\n}\n"
);
test!(
    subs_idents,
    "a {\n  color: foo - bar;\n}\n",
    "a {\n  color: foo-bar;\n}\n"
);
test!(
    subs_dbl_quoted_idents,
    "a {\n  color: \"foo\" - \"bar\";\n}\n",
    "a {\n  color: \"foo\"-\"bar\";\n}\n"
);
test!(
    subs_sgl_quoted_idents,
    "a {\n  color: 'foo' - 'bar';\n}\n",
    "a {\n  color: \"foo\"-\"bar\";\n}\n"
);
test!(
    subs_dbl_and_un_quoted_idents,
    "a {\n  color: \"foo\" - bar;\n}\n",
    "a {\n  color: \"foo\"-bar;\n}\n"
);
test!(
    subs_sgl_and_un_quoted_idents,
    "a {\n  color: 'foo' - bar;\n}\n",
    "a {\n  color: \"foo\"-bar;\n}\n"
);
test!(
    subs_un_and_dbl_quoted_idents,
    "a {\n  color: foo - \"bar\";\n}\n",
    "a {\n  color: foo-\"bar\";\n}\n"
);
test!(
    subs_un_and_sgl_quoted_idents,
    "a {\n  color: foo - 'bar';\n}\n",
    "a {\n  color: foo-\"bar\";\n}\n"
);
test!(
    subs_sgl_and_dbl_quoted_idents,
    "a {\n  color: 'foo' - \"bar\";\n}\n",
    "a {\n  color: \"foo\"-\"bar\";\n}\n"
);
test!(
    subs_dbl_and_sgl_quoted_idents,
    "a {\n  color: \"foo\" - 'bar';\n}\n",
    "a {\n  color: \"foo\"-\"bar\";\n}\n"
);
test!(
    subs_ident_true,
    "a {\n  color: foo - true;\n}\n",
    "a {\n  color: foo-true;\n}\n"
);
test!(
    subs_dbl_quoted_ident_true,
    "a {\n  color: \"foo\" - true;\n}\n",
    "a {\n  color: \"foo\"-true;\n}\n"
);
test!(
    subs_ident_false,
    "a {\n  color: foo - false;\n}\n",
    "a {\n  color: foo-false;\n}\n"
);
test!(
    subs_dbl_quoted_ident_false,
    "a {\n  color: \"foo\" - false;\n}\n",
    "a {\n  color: \"foo\"-false;\n}\n"
);
test!(
    subs_ident_important,
    "a {\n  color: foo - !important;\n}\n",
    "a {\n  color: foo-!important;\n}\n"
);
test!(
    subs_ident_null,
    "a {\n  color: foo - null;\n}\n",
    "a {\n  color: foo-;\n}\n"
);
test!(
    subs_dbl_quoted_ident_null,
    "a {\n  color: \"foo\" - null;\n}\n",
    "a {\n  color: \"foo\"-;\n}\n"
);
test!(
    subs_sgl_quoted_ident_null,
    "a {\n  color: 'foo' - null;\n}\n",
    "a {\n  color: \"foo\"-;\n}\n"
);
test!(
    subs_ident_number,
    "a {\n  color: foo - 1;\n}\n",
    "a {\n  color: foo-1;\n}\n"
);
test!(
    subs_dbl_quoted_ident_number,
    "a {\n  color: \"foo\" - 1;\n}\n",
    "a {\n  color: \"foo\"-1;\n}\n"
);
test!(
    subs_sgl_quoted_ident_number,
    "a {\n  color: 'foo' - 1;\n}\n",
    "a {\n  color: \"foo\"-1;\n}\n"
);
test!(
    subs_ident_dimension,
    "a {\n  color: foo - 1px;\n}\n",
    "a {\n  color: foo-1px;\n}\n"
);
test!(
    subs_dbl_quoted_ident_dimension,
    "a {\n  color: \"foo\" - 1px;\n}\n",
    "a {\n  color: \"foo\"-1px;\n}\n"
);
test!(
    subs_sgl_quoted_ident_dimension,
    "a {\n  color: 'foo' - 1px;\n}\n",
    "a {\n  color: \"foo\"-1px;\n}\n"
);
test!(
    undefined_function_call_is_ident,
    "a {\n  color: foo();\n}\n"
);
test!(
    undefined_function_call_is_ident_adds,
    "a {\n  color: 1 + foo();\n}\n",
    "a {\n  color: 1foo();\n}\n"
);
test!(positive_integer, "a {\n  color: 1;\n}\n");
test!(negative_integer, "a {\n  color: -1;\n}\n");
test!(
    positive_float_no_leading_zero,
    "a {\n  color: .1;\n}\n",
    "a {\n  color: 0.1;\n}\n"
);
test!(
    negative_float_no_leading_zero,
    "a {\n  color: -.1;\n}\n",
    "a {\n  color: -0.1;\n}\n"
);
test!(positive_float_leading_zero, "a {\n  color: 0.1;\n}\n");
test!(negative_float_leading_zero, "a {\n  color: -0.1;\n}\n");
test!(
    unitless_plus_null,
    "a {\n  color: 1 + null;\n}\n",
    "a {\n  color: 1;\n}\n"
);
// blocked on proper parsing of binary ops
// test!(
//     unitless_plus_null_plus_unitless,
//     "a {\n  color: 1 + null + 1;\n}\n",
//     "a {\n  color: 11;\n}\n"
// );
test!(
    unit_plus_null,
    "a {\n  color: 1px + null;\n}\n",
    "a {\n  color: 1px;\n}\n"
);
test!(hash_identifier_is_not_color, "a {\n  color: #foo;\n}\n");
test!(
    hash_identifier_is_string,
    "a {\n  color: type-of(#foo);\n}\n",
    "a {\n  color: string;\n}\n"
);
