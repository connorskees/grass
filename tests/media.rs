#![cfg(test)]

#[macro_use]
mod macros;

test!(
    basic_toplevel,
    "@media foo {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    toplevel_no_params,
    "@media {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    basic_nested,
    "a {\n  @media foo {\n  color: red;\n  }\n}\n",
    "@media foo {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    basic_unknown_at_rule,
    "@foo {\n  a {\n    color: red;\n  }\n}\n"
);
