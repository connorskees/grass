#[macro_use]
mod macros;

test!(simple_debug, "@debug 2", "");
test!(simple_debug_with_semicolon, "@debug 2;", "");
