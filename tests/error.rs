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
    "/a {color: red;}", "Error: expected selector."
);
error!(
    close_bracket_in_value,
    "a {color: red]}", "Error: expected \";\"."
);
error!(
    no_ident_after_dollar_in_style,
    "a {$", "Error: Expected identifier."
);
error!(
    nothing_after_variable_in_style,
    "a {$a", "Error: expected \":\"."
);
error!(toplevel_comma, "a {},", "Error: expected \"{\".");
error!(toplevel_exclamation, "! {}", "Error: expected \"{\".");
error!(toplevel_backtick, "` {}", "Error: expected selector.");
error!(
    toplevel_open_curly_brace,
    "{ {color: red;}", "Error: expected \"{\"."
);
error!(
    backtick_in_value,
    "a {color:`red;}", "Error: Expected expression."
);
error!(
    comma_begins_value,
    "a {color:,red;}", "Error: Expected expression."
);
error!(nothing_after_hyphen, "a {-}", "Error: Expected \":\".");
error!(
    nothing_after_hyphen_variable,
    "a {$-", "Error: expected \":\"."
);
error!(
    closing_brace_after_hyphen_variable,
    "a {$-}", "Error: Expected identifier."
);
error!(
    dbl_quoted_selector,
    "\"a\" {color: red;}", "Error: expected selector."
);
error!(
    sgl_quoted_selector,
    "'a' {color: red;}", "Error: expected selector."
);
error!(
    toplevel_hash_no_closing_curly_brace_has_value,
    "#{f", "Error: expected \"}\"."
);
error!(
    toplevel_hash_no_closing_curly_brace_no_value,
    "#{", "Error: expected \"}\"."
);
error!(toplevel_hash, "#", "Error: expected \"{\".");
error!(toplevel_var_no_colon, "$r", "Error: expected \":\".");
error!(bar_in_value, "a {color: a|b;}", "Error: expected \";\".");
error!(
    tilde_in_value,
    "a {color: ~a;}", "Error: Expected expression."
);
