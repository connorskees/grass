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
