#[macro_use]
mod macros;

test!(simple_debug, "@debug 2", "");
test!(simple_debug_with_semicolon, "@debug 2;", "");
test!(
    // todo: test stdout
    debug_while_quiet,
    "@debug 2;",
    "",
    grass::Options::default().quiet(true)
);
