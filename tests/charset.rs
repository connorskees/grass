#[macro_use]
mod macros;

test!(
    charset_exists_when_output_not_ascii,
    "a {\n  color: 🦆;\n}\n",
    "@charset \"UTF-8\";\na {\n  color: 🦆;\n}\n"
);
test!(
    charset_utf8_is_removed_when_ascii,
    "@charset \"UTF-8\";\na {\n  color: red;\n}\n",
    "a {\n  color: red;\n}\n"
);
test!(
    unknown_charset_is_removed,
    "@charset \"foo\";\na {\n  color: red;\n}\n",
    "a {\n  color: red;\n}\n"
);
test!(
    comment_between_rule_and_string,
    "@charset/**/\"foo\";\na {\n  color: red;\n}\n",
    "a {\n  color: red;\n}\n"
);
test!(
    comment_after_string,
    "@charset \"foo\"/**/;\na {\n  color: red;\n}\n",
    "/**/\na {\n  color: red;\n}\n"
);
test!(
    no_space_after_at_rule,
    "@charset\"foo\";\na {\n  color: red;\n}\n",
    "a {\n  color: red;\n}\n"
);
test!(
    charset_inside_rule,
    "a {\n  color: red;@charset \"foo\";\n\n}\n",
    "a {\n  color: red;\n  @charset \"foo\";\n}\n"
);
test!(
    charset_after_rule,
    "a {\n  color: red;\n}\n@charset \"foo\";\n",
    "a {\n  color: red;\n}\n"
);
error!(
    invalid_charset_value,
    "@charset 1;", "Error: Expected string."
);
error!(
    invalid_charset_value_unquoted_string,
    "@charset a;", "Error: Expected string."
);
