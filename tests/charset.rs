#![cfg(test)]

#[macro_use]
mod macros;

test!(
    #[ignore]
    utf8_input,
    "a {\n  color: 🦆;\n}\n",
    "@charset \"UTF-8\";\na {\n  color: 🦆;\n}\n"
);
test!(
    #[ignore]
    ascii_charset_utf8,
    "@charset \"UTF-8\";\na {\n  color: red;\n}\n",
    "a {\n  color: red;\n}\n"
);
test!(
    unknown_charset,
    "@charset \"foo\";\na {\n  color: red;\n}\n",
    "a {\n  color: red;\n}\n"
);
