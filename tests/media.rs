#![cfg(test)]

#[macro_use]
mod macros;

test!(
    basic_toplevel,
    "@media foo {\n  a {\n    color: red;\n  }\n}\n"
);
