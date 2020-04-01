#![cfg(test)]

#[macro_use]
mod macros;

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
    num_minus_list,
    "a {\n  color: 1 - (2 3);\n}\n",
    "a {\n  color: 1-2 3;\n}\n"
);
test!(
    list_minus_num,
    "a {\n  color: (1 2) - 3;\n}\n",
    "a {\n  color: 1 2-3;\n}\n"
);
test!(
    dblquoted_minus_list,
    "a {\n  color: \"1\" - (2 3);\n}\n",
    "a {\n  color: \"1\"-2 3;\n}\n"
);
test!(
    list_minus_dblquoted,
    "a {\n  color: (1 2) - \"3\";\n}\n",
    "a {\n  color: 1 2-\"3\";\n}\n"
);
test!(
    sglquoted_minus_list,
    "a {\n  color: 'a' - (b c);\n}\n",
    "a {\n  color: \"a\"-b c;\n}\n"
);
test!(
    list_minus_sglquoted,
    "a {\n  color: (b c) - 'a';\n}\n",
    "a {\n  color: b c-\"a\";\n}\n"
);
test!(
    list_minus_list,
    "a {\n  color: (a b) - (1 2);\n}\n",
    "a {\n  color: a b-1 2;\n}\n"
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
    number_minus_unquoted_ident,
    "a {\n  color: 1 - foo;\n}\n",
    "a {\n  color: 1-foo;\n}\n"
);
test!(
    number_minus_sglquoted_ident,
    "a {\n  color: 1 - 'foo';\n}\n",
    "a {\n  color: 1-\"foo\";\n}\n"
);
test!(
    number_minus_dblquoted_ident,
    "a {\n  color: 1 - \"foo\";\n}\n",
    "a {\n  color: 1-\"foo\";\n}\n"
);
test!(
    number_minus_minus_number,
    "a {\n  color: 1 - - 2;;\n}\n",
    "a {\n  color: 3;\n}\n"
);
