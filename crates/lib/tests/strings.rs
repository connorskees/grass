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
error!(
    uppercase_non_ident,
    "a {\n  color: to-upper-case(123);\n}\n", "Error: $string: 123 is not a string."
);
error!(
    lowercase_non_ident,
    "a {\n  color: to-lower-case(123);\n}\n", "Error: $string: 123 is not a string."
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
    str_slice_bigger_than_usize_max,
    "a {\n  color: str-slice($string: \"foo\", $start-at: -99999999999999999999, $end-at: 99999999999999999999);\n}\n",
    "a {\n  color: \"foo\";\n}\n"
);
test!(
    str_slice_positive_index_bigger_than_usize_max,
    "a {\n  color: str-slice($string: \"foo\", $start-at: 99999999999999999999, $end-at: -99999999999999999999);\n}\n",
    "a {\n  color: \"\";\n}\n"
);
test!(
    str_slice_start_end_equal,
    "a {\n  color: str-slice(\"cde\", 1, 1);\n}\n",
    "a {\n  color: \"c\";\n}\n"
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
    str_len_double_wide,
    "a {\n  color: str-length(\"👭\");\n}\n",
    "a {\n  color: 1;\n}\n"
);
test!(
    str_len_combining,
    "a {\n  color: str-length(\"c\\0308\");\n}\n",
    "a {\n  color: 2;\n}\n"
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
    "a {\n  color: str-insert(\"\", \"abcd\", 4);\n}\n",
    "a {\n  color: \"abcd\";\n}\n"
);
test!(
    str_insert_empty_substring,
    "a {\n  color: str-insert(abcd, \"\", 4);\n}\n",
    "a {\n  color: abcd;\n}\n"
);
test!(
    str_insert_idx_0,
    "a {\n  color: str-insert(abcd, \"X\", 0);\n}\n",
    "a {\n  color: Xabcd;\n}\n"
);
test!(
    str_insert_negative_1,
    "a {\n  color: str-insert(abc, \"X\", -1);\n}\n",
    "a {\n  color: abcX;\n}\n"
);
test!(
    str_insert_negative_2,
    "a {\n  color: str-insert(abc, \"X\", -2);\n}\n",
    "a {\n  color: abXc;\n}\n"
);
test!(
    str_insert_negative_3,
    "a {\n  color: str-insert(abc, \"X\", -3);\n}\n",
    "a {\n  color: aXbc;\n}\n"
);
error!(
    str_insert_float_idx,
    "a {\n  color: str-insert(abcd, \"X\", .5);\n}\n", "Error: $index: 0.5 is not an int."
);
error!(
    str_insert_idx_with_units,
    "a {\n  color: str-insert(abcd, \"X\", 5px);\n}\n",
    "Error: $index: Expected 5px to have no units."
);
test!(
    str_insert_idx_larger_than_string,
    "a {\n  color: str-insert(abcd, \"X\", 20);\n}\n",
    "a {\n  color: abcdX;\n}\n"
);
test!(
    str_insert_idx_larger_than_string_negative,
    "a {\n  color: str-insert(abcd, \"X\", -20);\n}\n",
    "a {\n  color: Xabcd;\n}\n"
);
test!(
    str_insert_double_width_char,
    "a {\n  color: str-insert(\"👭\", \"c\", 2);\n}\n",
    "@charset \"UTF-8\";\na {\n  color: \"👭c\";\n}\n"
);
test!(
    str_insert_positive_index_bigger_than_usize_max,
    "a {\n  color: str-insert($string: \"foo\", $insert: \"X\", $index: 99999999999999999999);\n}\n",
    "a {\n  color: \"fooX\";\n}\n"
);
test!(
    str_insert_negative_index_bigger_than_usize_max,
    "a {\n  color: str-insert($string: \"foo\", $insert: \"X\", $index: -99999999999999999999);\n}\n",
    "a {\n  color: \"Xfoo\";\n}\n"
);
test!(
    hash_in_string,
    "a {\n  color: \"#foo\";\n}\n",
    "a {\n  color: \"#foo\";\n}\n"
);
test!(
    escaped_newline_inside_string,
    "a {\n  color: \"f\\\n oo\";\n}\n",
    "a {\n  color: \"f oo\";\n}\n"
);
test!(
    str_index_double_width_character,
    "a {\n  color: str-index(\"👭a\", \"a\");\n}\n",
    "a {\n  color: 2;\n}\n"
);
test!(
    str_index_combining_character,
    "a {\n  color: str-index(\"c\\0308 a\", \"a\");\n}\n",
    "a {\n  color: 3;\n}\n"
);
error!(
    to_lower_case_no_args,
    "a {\n  color: to_lower_case();\n}\n", "Error: Missing argument $string."
);
error!(
    str_length_no_args,
    "a {\n  color: str_length();\n}\n", "Error: Missing argument $string."
);
error!(
    quote_no_args,
    "a {\n  color: quote();\n}\n", "Error: Missing argument $string."
);
error!(
    unquote_no_args,
    "a {\n  color: unquote();\n}\n", "Error: Missing argument $string."
);
error!(
    str_slice_no_args,
    "a {\n  color: str_slice();\n}\n", "Error: Missing argument $string."
);
error!(
    str_index_no_args,
    "a {\n  color: str_index();\n}\n", "Error: Missing argument $string."
);
error!(
    str_insert_no_args,
    "a {\n  color: str_insert();\n}\n", "Error: Missing argument $string."
);
test!(
    unique_id_is_unique,
    "$init: unique-id();
    @for $_ from 0 to 100 {
        @if $init == unique-id() {
            @error 'got duplicate unique id: #{$init}';
        }
    }",
    ""
);
test!(
    unique_id_is_valid_identifier,
    "@for $_ from 0 to 100 {
        #{unique-id()} {}
    }",
    ""
);
test!(
    string_module_exists,
    "@use 'sass:string';
    a {
        color: string.to-lower-case('AAA');
    }
    ",
    "a {\n  color: \"aaa\";\n}\n"
);
test!(
    str_split_abc_space,
    "@use 'sass:string';
    a {
        color: string.split('a b c', ' ');
    }
    ",
    "a {\n  color: [\"a\", \"b\", \"c\"];\n}\n"
);
test!(
    str_split_abc_space_1,
    "@use 'sass:string';
    a {
        color: string.split('a b c', ' ', 1);
    }
    ",
    "a {\n  color: [\"a\", \"b c\"];\n}\n"
);
test!(
    str_split_rgb_comma,
    "@use 'sass:string';
    a {
        color: string.split('red,green,blue', ',');
    }
    ",
    "a {\n  color: [\"red\", \"green\", \"blue\"];\n}\n"
);
test!(
    str_split_big_limit,
    "@use 'sass:string';
    a {
        color: string.split('red,green,blue', ',', 9223372036854775808);
    }
    ",
    "a {\n  color: [\"red\", \"green\", \"blue\"];\n}\n"
);
error!(
    str_split_negative_limit,
    "@use 'sass:string';
    a {
        color: string.split('red,green,blue', ',', -1);
    }
    ",
    "Error: $limit: Must be 1 or greater, was -1."
);
error!(
    str_split_zero_limit,
    "@use 'sass:string';
    a {
        color: string.split('red,green,blue', ',', 0);
    }
    ",
    "Error: $limit: Must be 1 or greater, was 0."
);
error!(
    str_split_string_limit,
    "@use 'sass:string';
    a {
        color: string.split('red,green,blue', ',', '0');
    }
    ",
    "Error: $limit: \"0\" is not a number."
);
error!(
    str_split_first_arg_not_string,
    "@use 'sass:string';
    a {
        color: string.split(1, ',');
    }
    ",
    "Error: $string: 1 is not a string."
);
error!(
    str_split_second_arg_not_string,
    "@use 'sass:string';
    a {
        color: string.split('1', 2);
    }
    ",
    "Error: $separator: 2 is not a string."
);
error!(
    #[ignore = "overflow issue"]
    str_split_limit_above_i64_max,
    "@use 'sass:string';
    a {
        color: string.split('1', '1', 36893488147419103232);
    }
    ",
    "Error: $limit: 36893488147419103000 is not an int."
);
