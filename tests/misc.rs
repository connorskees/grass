#![cfg(test)]

#[macro_use]
mod macros;

// test!(
//     combines_hyphens,
//     "a {\n  foo: bar - baz;\n}\n",
//     "a {\n  foo: bar-baz;\n}\n"
// );
test!(does_not_combine_hyphens, "a {\n  foo: bar -baz;\n}\n");
test!(
    ident_starts_with_hyphen,
    "a {\n  foo: -webkit-bar-baz;\n}\n"
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
    double_newline_between_unrelated_styles,
    "a {\n  color: red;\n}\n\nb {\n  color: blue;\n}\n"
);
