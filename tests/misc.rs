#![cfg(test)]

#[macro_use]
mod macros;

test!(
    ident_starts_with_hyphen,
    "a {\n  foo: -webkit-bar-baz;\n}\n"
);
test!(
    ident_starts_with_double_hyphen,
    "a {\n  foo: --webkit-bar-baz;\n}\n"
);
test!(ident_with_num, "el1 {\n  a: b;\n}\n");
test!(keyword_important, "a {\n  height: 1 !important;\n}\n");
test!(
    keyword_important_uppercase,
    "a {\n  height: 1 !IMPORTANT;\n}\n",
    "a {\n  height: 1 !important;\n}\n"
);
test!(
    keyword_important_not_at_end,
    "a {\n  height: !important 1;\n}\n"
);
test!(
    emits_double_newline_between_unrelated_styles,
    "a {\n  color: red;\n}\n\nb {\n  color: blue;\n}\n"
);
test!(
    variable_interchangable_hypen_dash,
    "$a-b: red; $a_b: green; a {\n  color: $a-b;\n}\n",
    "a {\n  color: green;\n}\n"
);
test!(
    two_semicolons,
    "a {\n  color: red;;\n}\n",
    "a {\n  color: red;\n}\n"
);
test!(
    five_semicolons,
    "a {\n  color: red;;;;;\n}\n",
    "a {\n  color: red;\n}\n"
);
test!(
    two_semicolons_whitespace,
    "a {\n  color: red; ;\n}\n",
    "a {\n  color: red;\n}\n"
);
test!(
    utf8_ident_before_len,
    "a {\n  color: length(😀red);\n}\n",
    "@charset \"UTF-8\";\na {\n  color: 1;\n}\n"
);
test!(
    utf8_ident_before,
    "a {\n  color: 😀red;\n}\n",
    "@charset \"UTF-8\";\na {\n  color: 😀red;\n}\n"
);
test!(
    utf8_ident_after_len,
    "a {\n  color: length(red😁)\n}\n",
    "@charset \"UTF-8\";\na {\n  color: 1;\n}\n"
);
test!(
    utf8_ident_after,
    "a {\n  color: red😁\n}\n",
    "@charset \"UTF-8\";\na {\n  color: red😁;\n}\n"
);
