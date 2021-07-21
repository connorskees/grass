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
error!(
    invalid_charset_value,
    "@charset 1;",
    "Error: Expected string."
);
error!(
    invalid_charset_value_unquoted_string,
    "@charset a;",
    "Error: Expected string."
);
