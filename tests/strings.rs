#![cfg(test)]

#[macro_use]
mod macros;

test!(
    uppercase_ident,
    "a {\n  color: to-upper-case(aBc123);\n}\n",
    "a {\n  color: ABC123;\n}\n"
);
test!(
    lowercase_ident,
    "a {\n  color: to-lower-case(AbC123);\n}\n",
    "a {\n  color: abc123;\n}\n"
);
test!(
    uppercase_named_arg,
    "a {\n  color: to-upper-case($string: aBc123);\n}\n",
    "a {\n  color: ABC123;\n}\n"
);
test!(
    lowercase_named_arg,
    "a {\n  color: to-lower-case($string: AbC123);\n}\n",
    "a {\n  color: abc123;\n}\n"
);
test!(
    length_ident,
    "a {\n  color: str-length(AbC123);\n}\n",
    "a {\n  color: 6;\n}\n"
);
test!(
    length_named_arg,
    "a {\n  color: str-length($string: aBc123);\n}\n",
    "a {\n  color: 6;\n}\n"
);
test!(
    str_slice_dbl_quote,
    "a {\n  color: str-slice(\"abcd\", 2, 3);\n}\n",
    "a {\n  color: \"bc\";\n}\n"
);
test!(
    str_slice_sgl_quote,
    "a {\n  color: str-slice('abcd', 2, 3);\n}\n",
    "a {\n  color: \"bc\";\n}\n"
);
test!(
    str_slice_no_quote,
    "a {\n  color: str-slice(abcd, 2, 3);\n}\n",
    "a {\n  color: bc;\n}\n"
);
test!(
    str_slice_no_end,
    "a {\n  color: str-slice(abcd, 2);\n}\n",
    "a {\n  color: bcd;\n}\n"
);
test!(
    str_slice_negative_start_negative_end,
    "a {\n  color: str-slice(abcd, -3, -2);\n}\n",
    "a {\n  color: bc;\n}\n"
);
test!(
    str_slice_negative_end,
    "a {\n  color: str-slice(abcd, 2, -2);\n}\n",
    "a {\n  color: bc;\n}\n"
);
test!(
    str_slice_start_0,
    "a {\n  color: str-slice(cde, 0);\n}\n",
    "a {\n  color: cde;\n}\n"
);
test!(
    str_slice_start_below_negative_str_len,
    "a {\n  color: str-slice(cde, -100);\n}\n",
    "a {\n  color: cde;\n}\n"
);
test!(
    str_slice_end_below_negative_str_len,
    "a {\n  color: str-slice(\"cde\", 0, -100);\n}\n",
    "a {\n  color: \"\";\n}\n"
);
test!(
    str_slice_end_0,
    "a {\n  color: str-slice(\"cde\", 1, 0);\n}\n",
    "a {\n  color: \"\";\n}\n"
);
test!(
    str_len_dbl_quotes,
    "a {\n  color: str-length(\"cde\");\n}\n",
    "a {\n  color: 3;\n}\n"
);
test!(
    str_len_unquoted,
    "a {\n  color: str-length(cde);\n}\n",
    "a {\n  color: 3;\n}\n"
);
test!(
    unquote_empty_string_is_null,
    "a {\n  color: unquote('');\n}\n",
    ""
);
test!(
    str_len_space,
    "a {\n  color: str-length(\"foo bar\");\n}\n",
    "a {\n  color: 7;\n}\n"
);
test!(
    str_index_char,
    "a {\n  color: str-index(abcd, a);\n}\n",
    "a {\n  color: 1;\n}\n"
);
test!(
    str_index_str,
    "a {\n  color: str-index(abcd, ab);\n}\n",
    "a {\n  color: 1;\n}\n"
);
test!(str_index_null, "a {\n  color: str-index(abcd, X);\n}\n", "");
test!(
    str_insert_start,
    "a {\n  color: str-insert(\"abcd\", \"X\", 1);\n}\n",
    "a {\n  color: \"Xabcd\";\n}\n"
);
test!(
    str_insert_middle,
    "a {\n  color: str-insert(\"abcd\", \"X\", 4);\n}\n",
    "a {\n  color: \"abcXd\";\n}\n"
);
test!(
    str_insert_end,
    "a {\n  color: str-insert(\"abcd\", \"X\", 5);\n}\n",
    "a {\n  color: \"abcdX\";\n}\n"
);
test!(
    str_insert_sgl_quotes,
    "a {\n  color: str-insert('abcd', \"X\", 4);\n}\n",
    "a {\n  color: \"abcXd\";\n}\n"
);
test!(
    str_insert_no_quotes,
    "a {\n  color: str-insert(abcd, \"X\", 4);\n}\n",
    "a {\n  color: abcXd;\n}\n"
);
test!(
    str_insert_empty_string,
    "a {\n  color: str-insert(abcd, \"\", 4);\n}\n",
    "a {\n  color: abcd;\n}\n"
);
