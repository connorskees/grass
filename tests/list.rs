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
    nth_map,
    "a {\n  color: nth((c: d, e: f, g: h), 2);\n}\n",
    "a {\n  color: e f;\n}\n"
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
test!(
    list_separator_comma_separated_with_space,
    "a {\n  color: list-separator(((a b, c d)));\n}\n",
    "a {\n  color: comma;\n}\n"
);
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
    set_nth_bracketed,
    "a {\n  color: set-nth([a, b, c], 1, e);\n}\n",
    "a {\n  color: [e, b, c];\n}\n"
);
test!(
    set_nth_map,
    "a {\n  color: set-nth((c: d, e: f, g: h), 2, i);\n}\n",
    "a {\n  color: c d, i, g h;\n}\n"
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
    append_bracketed,
    "a {\n  color: append([], 1);\n}\n",
    "a {\n  color: [1];\n}\n"
);
error!(
    append_non_string_separator,
    "a {b: append(c, d, $separator: 1);}", "Error: $separator: 1 is not a string."
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
    join_first_bracketed,
    "a {\n  color: join([a], b);\n}\n",
    "a {\n  color: [a b];\n}\n"
);
test!(
    join_second_bracketed,
    "a {\n  color: join(a, [b]);\n}\n",
    "a {\n  color: a b;\n}\n"
);
test!(
    join_space_comma,
    "a {\n  color: join(a b, (c,));\n}\n",
    "a {\n  color: a b c;\n}\n"
);
test!(
    join_comma_space,
    "a {\n  color: join((a, b), (c));\n}\n",
    "a {\n  color: a, b, c;\n}\n"
);
test!(
    join_bracketed_null,
    "a {\n  color: join([a b], c, $bracketed: null);\n}\n",
    "a {\n  color: a b c;\n}\n"
);
test!(
    join_bracketed_false,
    "a {\n  color: join([a b], c, $bracketed: false);\n}\n",
    "a {\n  color: a b c;\n}\n"
);
test!(
    join_bracketed_auto_brackets,
    "a {\n  color: join([a b], c, $bracketed: auto);\n}\n",
    "a {\n  color: [a b c];\n}\n"
);
test!(
    join_bracketed_auto_none,
    "a {\n  color: join(a b, c, $bracketed: auto);\n}\n",
    "a {\n  color: a b c;\n}\n"
);
test!(
    join_bracketed_random_string,
    "a {\n  color: join(a b, c, $bracketed: afhsihsdhsdkhsd);\n}\n",
    "a {\n  color: [a b c];\n}\n"
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
    long_space_separated_list,
    "a {\n  color: a b c d e f g h i j k l m n o p q r s t u v w x y z;\n}\n"
);
test!(
    long_comma_separated_list,
    "a {\n  color: a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z;\n}\n"
);
test!(
    deeply_nested_square_braces,
    "a {\n  color: [[[[[[a]]]]]];\n}\n"
);
test!(
    index_found_space_separated,
    "a {\n  color: index(1px solid red, solid);\n}\n",
    "a {\n  color: 2;\n}\n"
);
test!(
    index_not_found_space_separated,
    "a {\n  color: index(1px solid red, dashed);\n}\n",
    ""
);
// test!(
//     index_found_map,
//     "a {\n  color: index((width: 10px, height: 20px), (height 20px));\n}\n",
//     "a {\n  color: 2;\n}\n"
// );
