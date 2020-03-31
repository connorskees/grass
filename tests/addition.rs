#![cfg(test)]

#[macro_use]
mod macros;

test!(
    undefined_function_call_is_ident_adds,
    "a {\n  color: 1 + foo();\n}\n",
    "a {\n  color: 1foo();\n}\n"
);
test!(
    unitless_plus_null,
    "a {\n  color: 1 + null;\n}\n",
    "a {\n  color: 1;\n}\n"
);
test!(
    unitless_plus_null_plus_unitless,
    "a {\n  color: 1 + null + 1;\n}\n",
    "a {\n  color: 11;\n}\n"
);
test!(
    unit_plus_null,
    "a {\n  color: 1px + null;\n}\n",
    "a {\n  color: 1px;\n}\n"
);
test!(
    chain_ident_addition,
    "a {\n  color: a + b + c + d + e + f;\n}\n",
    "a {\n  color: abcdef;\n}\n"
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
    num_plus_list,
    "a {\n  color: 1 + (2 3);\n}\n",
    "a {\n  color: 12 3;\n}\n"
);
test!(
    list_plus_num,
    "a {\n  color: (1 2) + 3;\n}\n",
    "a {\n  color: 1 23;\n}\n"
);
test!(
    dblquoted_plus_list,
    "a {\n  color: \"1\" + (2 3);\n}\n",
    "a {\n  color: \"12 3\";\n}\n"
);
test!(
    list_plus_dblquoted,
    "a {\n  color: (1 2) + \"3\";\n}\n",
    "a {\n  color: \"1 23\";\n}\n"
);
test!(
    sglquoted_plus_list,
    "a {\n  color: 'a' + (b c);\n}\n",
    "a {\n  color: \"ab c\";\n}\n"
);
test!(
    list_plus_sglquoted,
    "a {\n  color: (b c) + 'a';\n}\n",
    "a {\n  color: \"b ca\";\n}\n"
);
test!(
    list_plus_list,
    "a {\n  color: (a b) + (1 2);\n}\n",
    "a {\n  color: a b1 2;\n}\n"
);
test!(
    multiple_ident_sum,
    "a {\n  color: foo + 1 + bar + 2;\n}\n",
    "a {\n  color: foo1bar2;\n}\n"
);
test!(
    unquoted_plus_dbl_quoted,
    "a {\n  color: foo + \"foo\";\n}\n",
    "a {\n  color: foofoo;\n}\n"
);
test!(
    unquoted_plus_sgl_quoted,
    "a {\n  color: foo + 'foo';\n}\n",
    "a {\n  color: foofoo;\n}\n"
);
