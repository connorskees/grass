#![cfg(test)]

#[macro_use]
mod macros;

error!(
    nothing_after_decimal,
    "a {color: 1.;}", "Error: Expected digit."
);
error!(
    ascii_control_character,
    "a {color: ;}", "Error: Expected expression."
);
error!(
    toplevel_invalid_atrule_ident,
    "@`or $i from 1 through 3 {}", "Error: Expected identifier."
);
error!(
    return_as_style,
    "a {@return foo;}", "Error: This at-rule is not allowed here."
);
error!(
    colon_inside_style,
    "a {foo: bar: baz;}", "Error: expected \";\"."
);