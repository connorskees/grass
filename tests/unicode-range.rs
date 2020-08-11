#![cfg(test)]

#[macro_use]
mod macros;

test!(
    single_codepoint,
    "a {\n  color: U+26;\n}\n",
    "a {\n  color: U+26;\n}\n"
);
test!(
    simple_range,
    "a {\n  color: U+0-7F;\n}\n",
    "a {\n  color: U+0-7F;\n}\n"
);
test!(
    simple_wildcard_range,
    "a {\n  color: U+45????;\n}\n",
    "a {\n  color: U+45????;\n}\n"
);
