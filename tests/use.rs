#![cfg(test)]

#[macro_use]
mod macros;

error!(
    after_style,
    "a {}
    @use \"foo\";
    ",
    "Error: @use rules must be written before any other rules."
);
