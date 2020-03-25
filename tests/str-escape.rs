#![cfg(test)]

#[macro_use]
mod macros;

test!(
    escape_leading_zeros,
    "a {\n  color: ax \\61x \\61 x \\061x \\0061x \\00061x;\n}\n",
    "a {\n  color: ax ax ax ax ax ax;\n}\n"
);
test!(
    escape_start_non_hex,
    "a {\n  color: \\xx;\n}\n",
    "a {\n  color: xx;\n}\n"
);
test!(
    escape_start_non_ascii,
    "a {\n  color: ☃x \\☃x \\2603x;\n}\n",
    "@charset \"UTF-8\";\na {\n  color: ☃x ☃x ☃x;\n}\n"
);
test!(
    escape_hyphen_in_middle,
    "a {\n  color: a\\2dx a\\-x;\n}\n",
    "a {\n  color: a-x a-x;\n}\n"
);
test!(
    escape_hyphen_at_start,
    "a {\n  color: \\2dx \\-x;\n}\n",
    "a {\n  color: \\-x \\-x;\n}\n"
);
test!(
    escape_digit_in_middle,
    "a {\n  color: a\\31x a\\31 x;\n}\n",
    "a {\n  color: a1x a1x;\n}\n"
);
test!(
    escape_digit_at_start,
    "a {\n  color: \\31x \\31 x;\n}\n",
    "a {\n  color: \\31 x \\31 x;\n}\n"
);
test!(
    escape_non_printable_characters,
    "a {\n  color: \\0x \\1x \\2x \\3x \\4x \\5x \\6x \\7x \\8x \\Bx \\Ex \\Fx \\10x \\11x \\12x \\13x \\14x \\15x \\16x \\17x \\18x \\19x \\1Ax \\1Bx \\1Cx \\1Dx \\1Ex \\1Fx \\7Fx;\n}\n",
    "a {\n  color: \\0 x \\1 x \\2 x \\3 x \\4 x \\5 x \\6 x \\7 x \\8 x \\b x \\e x \\f x \\10 x \\11 x \\12 x \\13 x \\14 x \\15 x \\16 x \\17 x \\18 x \\19 x \\1a x \\1b x \\1c x \\1d x \\1e x \\1f x \\7f x;\n}\n"
);
test!(
    escape_newlines,
    "a {\n  color: \\ax \\cx \\dx;\n}\n",
    "a {\n  color: \\a x \\c x \\d x;\n}\n"
);
test!(
    escape_tabs,
    "a {\n  color: \\	x \\9x;\n}\n",
    "a {\n  color: \\\tx \\\tx;\n}\n"
);
test!(
    escape_interpolation_start,
    "a {\n  color: \\-#{foo};\n}\n",
    "a {\n  color: \\-foo;\n}\n"
);
test!(
    escape_interpolation_middle,
    "a {\n  color: #{foo}\\-#{bar};\n}\n",
    "a {\n  color: foo-bar;\n}\n"
);
test!(
    escape_interpolation_end,
    "a {\n  color: #{foo}\\-;\n}\n",
    "a {\n  color: foo-;\n}\n"
);
test!(
    escape_recognized_as_at_rule,
    "@\\69 f true {\n  a {\n    b: c;\n  }\n}\n",
    "a {\n  b: c;\n}\n"
);
test!(
    escape_in_middle,
    "a {\n  color: b\\6cue;\n}\n",
    "a {\n  color: blue;\n}\n"
);
test!(
    escape_at_end,
    "a {\n  color: blu\\65;\n}\n",
    "a {\n  color: blue;\n}\n"
);
test!(double_escape_is_preserved, "a {\n  color: r\\\\65;\n}\n");
test!(semicolon_in_string, "a {\n  color: \";\";\n}\n");
test!(
    single_character_escape_sequence_has_space,
    "a {\n  color: \\fg1;\n}\n",
    "a {\n  color: \\f g1;\n}\n"
);
test!(
    single_character_escape_sequence_removes_slash_when_not_hex_digit,
    "a {\n  color: \\g1;\n}\n",
    "a {\n  color: g1;\n}\n"
);
test!(
    single_character_escape_sequence_has_space_after,
    "a {\n  color: \\0;\n}\n",
    "a {\n  color: \\0 ;\n}\n"
);
