#[macro_use]
mod macros;

test!(
    single_codepoint,
    "a {\n  color: U+26;\n}\n",
    "a {\n  color: U+26;\n}\n"
);
test!(
    simple_range,
    "a {\n  color: U+0-7F;\n}\n",
    "a {\n  color: U+0-7F;\n}\n"
);
test!(
    simple_wildcard_range,
    "a {\n  color: U+45????;\n}\n",
    "a {\n  color: U+45????;\n}\n"
);
test!(
    lowercase_u,
    "a {\n  color: u+27a;\n}\n",
    "a {\n  color: u+27a;\n}\n"
);
test!(
    second_element_in_list,
    "a {\n  color: a u+55;\n}\n",
    "a {\n  color: a u+55;\n}\n"
);
test!(
    escaped_lowercase_u,
    "a {\n  color: \\75+55;\n}\n",
    "a {\n  color: u55;\n}\n"
);
test!(
    escaped_uppercase_u,
    "a {\n  color: \\55+55;\n}\n",
    "a {\n  color: U55;\n}\n"
);
test!(
    escaped_lowercase_u_with_space_after_escape,
    "a {\n  color: \\75 +55;\n}\n",
    "a {\n  color: u55;\n}\n"
);
test!(
    escaped_uppercase_u_with_space_after_escape,
    "a {\n  color: \\55 +55;\n}\n",
    "a {\n  color: U55;\n}\n"
);
error!(
    interpolated_range,
    "a {\n  color: U+2A#{70}C;\n}\n", "Error: Expected end of identifier."
);
error!(
    unicode_escape_within_range,
    "a {\n  color: U+B\\a;\n}\n", "Error: Expected end of identifier."
);
error!(
    longer_than_6_characters,
    "a {\n  color: U+1234567;\n}\n", "Error: Expected at most 6 digits."
);
error!(
    length_of_6_with_question_mark,
    "a {\n  color: U+123456?;\n}\n", "Error: Expected at most 6 digits."
);
error!(
    nothing_after_plus_lowercase,
    "a {\n  color: u+;\n}\n", r#"Error: Expected hex digit or "?"."#
);
error!(
    nothing_after_plus_uppercase,
    "a {\n  color: U+;\n}\n", r#"Error: Expected hex digit or "?"."#
);
error!(
    second_part_of_range_is_empty,
    "a {\n  color: u+55-;\n}\n", r#"Error: Expected hex digit."#
);
error!(
    second_part_of_range_is_more_than_6_chars,
    "a {\n  color: u+55-1234567;\n}\n", r#"Error: Expected at most 6 digits."#
);
