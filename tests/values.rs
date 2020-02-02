#![cfg(test)]

#[macro_use]
mod macros;

test!(comma_list_ident, "a {\n  color: red, white, blue;\n}\n");
test!(space_list_ident, "a {\n  color: red white blue;\n}\n");
test!(comma_list_number, "a {\n  color: 1, 2, 3;\n}\n");
test!(space_list_number, "a {\n  color: 1 2 3;\n}\n");
test!(comma_space_list_number, "a {\n  color: 1 1, 2 2, 3 3;\n}\n");
test!(preserves_keyword_true, "a {\n  color: true;\n}\n");
test!(preserves_keyword_false, "a {\n  color: false;\n}\n");
test!(preserves_keyword_null, "a {\n  color: null;\n}\n");
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
    "a {\n  color: \"'red' \\\"blue\\\"\";\n}\n"
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
    "a {\n  color: (red);\n}\n",
    "a {\n  color: red;\n}\n"
);
test!(
    removes_paren_around_space_list,
    "a {\n  color: (red blue);\n}\n",
    "a {\n  color: red blue;\n}\n"
);
test!(
    removes_paren_around_item_in_list,
    "a {\n  color: 1 (red blue);\n}\n",
    "a {\n  color: 1 red blue;\n}\n"
);
test!(
    adds_idents,
    "a {\n  color: red + blue;\n}\n",
    "a {\n  color: redblue;\n}\n"
);
test!(
    adds_dbl_quoted_idents,
    "a {\n  color: \"red\" + \"blue\";\n}\n",
    "a {\n  color: \"redblue\";\n}\n"
);
test!(
    adds_sgl_quoted_idents,
    "a {\n  color: 'red' + 'blue';\n}\n",
    "a {\n  color: \"redblue\";\n}\n"
);
test!(
    adds_dbl_and_un_quoted_idents,
    "a {\n  color: \"red\" + blue;\n}\n",
    "a {\n  color: \"redblue\";\n}\n"
);
test!(
    adds_sgl_and_un_quoted_idents,
    "a {\n  color: 'red' + blue;\n}\n",
    "a {\n  color: \"redblue\";\n}\n"
);
test!(
    adds_un_and_dbl_quoted_idents,
    "a {\n  color: red + \"blue\";\n}\n",
    "a {\n  color: \"redblue\";\n}\n"
);
test!(
    adds_un_and_sgl_quoted_idents,
    "a {\n  color: red + 'blue';\n}\n",
    "a {\n  color: \"redblue\";\n}\n"
);
test!(
    adds_sgl_and_dbl_quoted_idents,
    "a {\n  color: 'red' + \"blue\";\n}\n",
    "a {\n  color: \"redblue\";\n}\n"
);
test!(
    adds_dbl_and_sgl_quoted_idents,
    "a {\n  color: \"red\" + 'blue';\n}\n",
    "a {\n  color: \"redblue\";\n}\n"
);
test!(
    adds_ident_true,
    "a {\n  color: red + true;\n}\n",
    "a {\n  color: redtrue;\n}\n"
);
test!(
    adds_dbl_quoted_ident_true,
    "a {\n  color: \"red\" + true;\n}\n",
    "a {\n  color: \"redtrue\";\n}\n"
);
test!(
    adds_ident_false,
    "a {\n  color: red + false;\n}\n",
    "a {\n  color: redfalse;\n}\n"
);
test!(
    adds_dbl_quoted_ident_false,
    "a {\n  color: \"red\" + false;\n}\n",
    "a {\n  color: \"redfalse\";\n}\n"
);
test!(
    adds_ident_important,
    "a {\n  color: red + !important;\n}\n",
    "a {\n  color: red!important;\n}\n"
);
test!(
    adds_ident_null,
    "a {\n  color: red + null;\n}\n",
    "a {\n  color: red;\n}\n"
);
test!(
    adds_dbl_quoted_ident_null,
    "a {\n  color: \"red\" + null;\n}\n",
    "a {\n  color: \"red\";\n}\n"
);
test!(
    adds_sgl_quoted_ident_null,
    "a {\n  color: 'red' + null;\n}\n",
    "a {\n  color: \"red\";\n}\n"
);
test!(
    adds_ident_number,
    "a {\n  color: red + 1;\n}\n",
    "a {\n  color: red1;\n}\n"
);
test!(
    adds_dbl_quoted_ident_number,
    "a {\n  color: \"red\" + 1;\n}\n",
    "a {\n  color: \"red1\";\n}\n"
);
test!(
    adds_sgl_quoted_ident_number,
    "a {\n  color: 'red' + 1;\n}\n",
    "a {\n  color: \"red1\";\n}\n"
);
test!(
    adds_ident_dimension,
    "a {\n  color: red + 1px;\n}\n",
    "a {\n  color: red1px;\n}\n"
);
test!(
    adds_dbl_quoted_ident_dimension,
    "a {\n  color: \"red\" + 1px;\n}\n",
    "a {\n  color: \"red1px\";\n}\n"
);
test!(
    adds_sgl_quoted_ident_dimension,
    "a {\n  color: 'red' + 1px;\n}\n",
    "a {\n  color: \"red1px\";\n}\n"
);
test!(
    subs_idents,
    "a {\n  color: red - blue;\n}\n",
    "a {\n  color: red-blue;\n}\n"
);
test!(
    subs_dbl_quoted_idents,
    "a {\n  color: \"red\" - \"blue\";\n}\n",
    "a {\n  color: \"red\"-\"blue\";\n}\n"
);
test!(
    subs_sgl_quoted_idents,
    "a {\n  color: 'red' - 'blue';\n}\n",
    "a {\n  color: \"red\"-\"blue\";\n}\n"
);
test!(
    subs_dbl_and_un_quoted_idents,
    "a {\n  color: \"red\" - blue;\n}\n",
    "a {\n  color: \"red\"-blue;\n}\n"
);
test!(
    subs_sgl_and_un_quoted_idents,
    "a {\n  color: 'red' - blue;\n}\n",
    "a {\n  color: \"red\"-blue;\n}\n"
);
test!(
    subs_un_and_dbl_quoted_idents,
    "a {\n  color: red - \"blue\";\n}\n",
    "a {\n  color: red-\"blue\";\n}\n"
);
test!(
    subs_un_and_sgl_quoted_idents,
    "a {\n  color: red - 'blue';\n}\n",
    "a {\n  color: red-\"blue\";\n}\n"
);
test!(
    subs_sgl_and_dbl_quoted_idents,
    "a {\n  color: 'red' - \"blue\";\n}\n",
    "a {\n  color: \"red\"-\"blue\";\n}\n"
);
test!(
    subs_dbl_and_sgl_quoted_idents,
    "a {\n  color: \"red\" - 'blue';\n}\n",
    "a {\n  color: \"red\"-\"blue\";\n}\n"
);
test!(
    subs_ident_true,
    "a {\n  color: red - true;\n}\n",
    "a {\n  color: red-true;\n}\n"
);
test!(
    subs_dbl_quoted_ident_true,
    "a {\n  color: \"red\" - true;\n}\n",
    "a {\n  color: \"red\"-true;\n}\n"
);
test!(
    subs_ident_false,
    "a {\n  color: red - false;\n}\n",
    "a {\n  color: red-false;\n}\n"
);
test!(
    subs_dbl_quoted_ident_false,
    "a {\n  color: \"red\" - false;\n}\n",
    "a {\n  color: \"red\"-false;\n}\n"
);
test!(
    subs_ident_important,
    "a {\n  color: red - !important;\n}\n",
    "a {\n  color: red-!important;\n}\n"
);
test!(
    subs_ident_null,
    "a {\n  color: red - null;\n}\n",
    "a {\n  color: red-;\n}\n"
);
test!(
    subs_dbl_quoted_ident_null,
    "a {\n  color: \"red\" - null;\n}\n",
    "a {\n  color: \"red\"-;\n}\n"
);
test!(
    subs_sgl_quoted_ident_null,
    "a {\n  color: 'red' - null;\n}\n",
    "a {\n  color: \"red\"-;\n}\n"
);
test!(
    subs_ident_number,
    "a {\n  color: red - 1;\n}\n",
    "a {\n  color: red-1;\n}\n"
);
test!(
    subs_dbl_quoted_ident_number,
    "a {\n  color: \"red\" - 1;\n}\n",
    "a {\n  color: \"red\"-1;\n}\n"
);
test!(
    subs_sgl_quoted_ident_number,
    "a {\n  color: 'red' - 1;\n}\n",
    "a {\n  color: \"red\"-1;\n}\n"
);
test!(
    subs_ident_dimension,
    "a {\n  color: red - 1px;\n}\n",
    "a {\n  color: red-1px;\n}\n"
);
test!(
    subs_dbl_quoted_ident_dimension,
    "a {\n  color: \"red\" - 1px;\n}\n",
    "a {\n  color: \"red\"-1px;\n}\n"
);
test!(
    subs_sgl_quoted_ident_dimension,
    "a {\n  color: 'red' - 1px;\n}\n",
    "a {\n  color: \"red\"-1px;\n}\n"
);
