#![cfg(test)]

#[macro_use]
mod macros;

test!(
    length_of_list_as_var,
    "$a: 1 2 3 4 5;a {\n  color: length($a);\n}\n",
    "a {\n  color: 5;\n}\n"
);
test!(
    length_of_list,
    "a {\n  color: length(1 2 3 4 5);\n}\n",
    "a {\n  color: 5;\n}\n"
);
test!(
    length_of_comma_list,
    "a {\n  color: length((1, 2, 3, 4, 5));\n}\n",
    "a {\n  color: 5;\n}\n"
);
test!(
    nth_space_separated,
    "a {\n  color: nth(a b c, 1);\n}\n",
    "a {\n  color: a;\n}\n"
);
test!(
    nth_negative_index,
    "a {\n  color: nth(a b c, -2);\n}\n",
    "a {\n  color: b;\n}\n"
);
test!(
    nth_comma_separated,
    "a {\n  color: nth((a, b, c), 3);\n}\n",
    "a {\n  color: c;\n}\n"
);
test!(
    nth_non_list,
    "a {\n  color: nth(foo, 1);\n}\n",
    "a {\n  color: foo;\n}\n"
);
test!(
    list_separator_space_separated,
    "a {\n  color: list-separator(a b c);\n}\n",
    "a {\n  color: space;\n}\n"
);
test!(
    list_separator_foo,
    "a {\n  color: list-separator(foo);\n}\n",
    "a {\n  color: space;\n}\n"
);
test!(
    list_separator_comma_separated,
    "a {\n  color: list-separator((a, b, c));\n}\n",
    "a {\n  color: comma;\n}\n"
);
// blocked on better parsing of comma separated lists with
// space separated lists inside
// test!(
//     list_separator_comma_separated_with_space,
//     "a {\n  color: list-separator(((a b, c d)));\n}\n",
//     "a {\n  color: comma;\n}\n"
// );
test!(
    set_nth_named_args,
    "a {\n  color: set-nth($list: 1 2 3, $n: 2, $value: foo);\n}\n",
    "a {\n  color: 1 foo 3;\n}\n"
);
test!(
    set_nth_non_list,
    "a {\n  color: set-nth(c, 1, e);\n}\n",
    "a {\n  color: e;\n}\n"
);
test!(
    set_nth_2_long,
    "a {\n  color: set-nth(c d, 1, e);\n}\n",
    "a {\n  color: e d;\n}\n"
);
test!(
    set_nth_comma_separated,
    "a {\n  color: set-nth((a, b, c), 1, e);\n}\n",
    "a {\n  color: e, b, c;\n}\n"
);
test!(
    append_space_separated,
    "a {\n  color: append(a b, c);\n}\n",
    "a {\n  color: a b c;\n}\n"
);
test!(
    append_comma_separated,
    "a {\n  color: append((a, b), c);\n}\n",
    "a {\n  color: a, b, c;\n}\n"
);
test!(
    append_list,
    "a {\n  color: append(a b, c d);\n}\n",
    "a {\n  color: a b c d;\n}\n"
);
test!(
    append_list_separator_comma,
    "a {\n  color: append(a, b, comma);\n}\n",
    "a {\n  color: a, b;\n}\n"
);
test!(
    append_list_separator_space,
    "a {\n  color: append((a, b), c, space);\n}\n",
    "a {\n  color: a b c;\n}\n"
);
test!(
    list_separator_empty,
    "a {\n  color: list-separator(());\n}\n",
    "a {\n  color: space;\n}\n"
);
test!(
    append_empty,
    "a {\n  color: append((), a);\n}\n",
    "a {\n  color: a;\n}\n"
);
test!(
    join_space_separated,
    "a {\n  color: join(a b, c d);\n}\n",
    "a {\n  color: a b c d;\n}\n"
);
test!(
    join_comma_separated,
    "a {\n  color: join((a, b), (c, d));\n}\n",
    "a {\n  color: a, b, c, d;\n}\n"
);
test!(
    join_non_list,
    "a {\n  color: join(a, b);\n}\n",
    "a {\n  color: a b;\n}\n"
);
test!(
    join_separator_comma,
    "a {\n  color: join(a, b, comma);\n}\n",
    "a {\n  color: a, b;\n}\n"
);
test!(
    join_separator_space,
    "a {\n  color: join((a, b), (c, d), space);\n}\n",
    "a {\n  color: a b c d;\n}\n"
);
test!(
    join_bracketed,
    "a {\n  color: join([a], b);\n}\n",
    "a {\n  color: [a b];\n}\n"
);
test!(bracketed_ident, "a {\n  color: [a];\n}\n");
test!(bracketed_space_list, "a {\n  color: [a b];\n}\n");
test!(bracketed_comma_list, "a {\n  color: [a, b];\n}\n");
test!(bracketed_as_space_list, "a {\n  color: [a b] c;\n}\n");
test!(
    trailing_comma_bare,
    "a {\n  color: 1,;\n}\n",
    "a {\n  color: 1;\n}\n"
);
test!(
    trailing_comma_paren,
    "a {\n  color: (1,);\n}\n",
    "a {\n  color: 1;\n}\n"
);
test!(
    trailing_comma_bracket,
    "a {\n  color: [1,];\n}\n",
    "a {\n  color: [1];\n}\n"
);
test!(
    null_values_in_list_ommitted,
    "a {\n  color: 1, null, null;;\n}\n",
    "a {\n  color: 1;\n}\n"
);
