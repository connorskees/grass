#[macro_use]
mod macros;

test!(simple_warn, "@warn 2", "");
test!(
    // todo: test stdout
    warn_while_quiet,
    "@warn 2;",
    "",
    grass::Options::default().quiet(true)
);
