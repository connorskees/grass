#![cfg(test)]

#[macro_use]
mod macros;

test!(
    basic_toplevel,
    "@media foo {\n  a {\n    color: red;\n  }\n}\n"
);
error!(
    no_params,
    "@media {\n  a {\n    color: red;\n  }\n}\n", "Error: Expected identifier."
);
test!(
    basic_nested,
    "a {\n  @media foo {\n  color: red;\n  }\n}\n",
    "@media foo {\n  a {\n    color: red;\n  }\n}\n"
);
test!(empty_body, "@media (min-width: 2px) {}", "");
test!(
    newlines_are_not_emitted_for_child_styles,
    "a {
        @media screen {
            b {
                color: red;
            }
            c {
                color: green;
            }
        }
    }",
    "@media screen {\n  a b {\n    color: red;\n  }\n  a c {\n    color: green;\n  }\n}\n"
);
test!(
    multiple_identifiers_in_query,
    "@media not screen {
        a {
            color: red;
        }
    }",
    "@media not screen {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    multiple_identifiers_in_query_second_is_and,
    "@media print and (foo: 1 2 3) {
        a {
            color: red;
        }
    }",
    "@media print and (foo: 1 2 3) {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    single_identifier_inside_parens,
    "@media (color) {a {color: red;}}",
    "@media (color) {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    quoted_colon_in_parens,
    "@media screen and (\":\") {
        a {
            color: red;
        }
    }",
    "@media screen and (:) {\n  a {\n    color: red;\n  }\n}\n"
);
