#![cfg(test)]

#[macro_use]
mod macros;

test!(unit_none, "a {\n  height: 1;\n}\n");
test!(unit_not_attached, "a {\n  height: 1 px;\n}\n");
test!(unit_px, "a {\n  height: 1px;\n}\n");
test!(unit_em, "a {\n  height: 1em;\n}\n");
test!(unit_rem, "a {\n  height: 1rem;\n}\n");
test!(unit_percent, "a {\n  height: 1%;\n}\n");
