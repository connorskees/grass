use grass_compiler::OutputStyle;

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
error!(
    invalid_charset_value_silent_comment,
    "@charset //", "Error: Expected string."
);
error!(
    invalid_charset_value_unterminated_loud_comment,
    "@charset /*", "Error: expected more input."
);

#[test]
fn charset_not_allowed_expanded() {
    let input = r#"
        a {
            color: 🦆;
        }
    "#;

    assert_eq!(
        "a {\n  color: 🦆;\n}\n",
        &grass::from_string(
            input.to_string(),
            &grass::Options::default().allows_charset(false)
        )
        .expect(input)
    );
}

#[test]
fn charset_not_allowed_compressed() {
    let input = r#"
        a {
            color: 🦆;
        }
    "#;

    assert_eq!(
        "a{color:🦆}",
        &grass::from_string(
            input.to_string(),
            &grass::Options::default()
                .allows_charset(false)
                .style(OutputStyle::Compressed)
        )
        .expect(input)
    );
}
