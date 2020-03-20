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
