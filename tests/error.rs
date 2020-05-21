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
    colon_inside_value,
    "a {foo: bar: baz;}", "Error: expected \";\"."
);
error!(
    question_mark_inside_value,
    "a {foo: bar?}", "Error: expected \";\"."
);
// TODO: special parsing rules for variable names
// error!(
//     interpolation_in_variable_declaration,
//     "$base-#{lor}: #036;", "Error: expected \":\"."
// );
// error!(
//     backslash_as_last_character,
//     "a {colo\\: red;}", "Error: expected \"{\"."
// );
error!(
    close_paren_without_opening,
    "a {color: foo);}", "Error: expected \";\"."
);
error!(
    symbol_after_hash,
    "a {color: bar + #}ar;}", "Error: Expected identifier."
);
error!(
    control_character_starts_selector_toplevel,
    "l {color: foo;}", "Error: expected selector."
);
error!(
    control_character_starts_selector_inner,
    "a{l {color: foo;}}", "Error: expected selector."
);
error!(backtick_in_selector, "a`{}", "Error: expected selector.");
error!(
    no_value_after_forward_slash,
    "a {color: 303/;}", "Error: Expected expression."
);
error!(xor_in_value, "a {color: a^;}", "Error: expected \";\".");
error!(
    nothing_after_at_sign,
    "a {color: red; @", "Error: Expected identifier."
);
error!(
    missing_colon_in_style,
    "a {color, red;}", "Error: Expected \":\"."
);
error!(
    toplevel_forward_slash,
    "/a {color, red;}", "Error: expected selector."
);
