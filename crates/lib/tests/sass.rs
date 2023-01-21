use grass::InputSyntax;

#[macro_use]
mod macros;

test!(
    two_properties,
    r#"
a
    color: red
    foo: bar
"#,
    "a {\n  color: red;\n  foo: bar;\n}\n",
    grass::Options::default().input_syntax(InputSyntax::Sass)
);
test!(
    no_properties,
    r#"a"#,
    "",
    grass::Options::default().input_syntax(InputSyntax::Sass)
);
test!(
    nested_styles,
    r#"
a
    color: red
    b
        foo: bar
"#,
    "a {\n  color: red;\n}\na b {\n  foo: bar;\n}\n",
    grass::Options::default().input_syntax(InputSyntax::Sass)
);
test!(
    nested_declarations,
    r#"
a
    color: red
        foo: bar
"#,
    "a {\n  color: red;\n  color-foo: bar;\n}\n",
    grass::Options::default().input_syntax(InputSyntax::Sass)
);
test!(
    variable_declaration,
    r#"
$a: red
a
    color: $a
"#,
    "a {\n  color: red;\n}\n",
    grass::Options::default().input_syntax(InputSyntax::Sass)
);
test!(
    silent_comment_before_variable_declaration,
    r#"
// silent
$a: red

a
    color: $a
"#,
    "a {\n  color: red;\n}\n",
    grass::Options::default().input_syntax(InputSyntax::Sass)
);
test!(
    two_silent_comments_before_variable_declaration,
    r#"
// silent
// silent
$a: red

a
    color: $a
"#,
    "a {\n  color: red;\n}\n",
    grass::Options::default().input_syntax(InputSyntax::Sass)
);
test!(
    unclosed_loud_comment,
    r#"/* loud"#,
    "/* loud */\n",
    grass::Options::default().input_syntax(InputSyntax::Sass)
);
test!(
    special_mixin_and_include_characters,
    r#"
=foo
    color: red

a
    +foo
"#,
    "a {\n  color: red;\n}\n",
    grass::Options::default().input_syntax(InputSyntax::Sass)
);
test!(
    style_rule_begins_with_plus,
    r#"
a
    + b
        color: red
"#,
    "a + b {\n  color: red;\n}\n",
    grass::Options::default().input_syntax(InputSyntax::Sass)
);
test!(
    if_else_if_else,
    r#"
a
    @if false
        color: red
    @else if false
        color: blue
    @else
        color: orange
"#,
    "a {\n  color: orange;\n}\n",
    grass::Options::default().input_syntax(InputSyntax::Sass)
);
error!(
    multiline_comment_in_value_position,
    r#"
$a: /*
loud */ red
"#,
    "Error: expected */.",
    grass::Options::default().input_syntax(InputSyntax::Sass)
);
error!(
    document_starts_with_spaces,
    r#"    "#,
    "Error: Indenting at the beginning of the document is illegal.",
    grass::Options::default().input_syntax(InputSyntax::Sass)
);
error!(
    document_starts_with_tab,
    "\t",
    "Error: Indenting at the beginning of the document is illegal.",
    grass::Options::default().input_syntax(InputSyntax::Sass)
);
