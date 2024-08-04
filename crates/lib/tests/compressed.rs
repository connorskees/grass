#[macro_use]
mod macros;

test!(
    compresses_simple_rule,
    "a {\n  color: red;\n}\n",
    "a{color:red}",
    grass::Options::default().style(grass::OutputStyle::Compressed)
);
test!(
    compresses_rule_with_many_styles,
    "a {\n  color: red;\n  color: green;\n  color: blue;\n}\n",
    "a{color:red;color:green;color:blue}",
    grass::Options::default().style(grass::OutputStyle::Compressed)
);
test!(
    compresses_media_rule,
    "@media foo {\n  a {\n    color: red;\n  }\n}\n",
    "@media foo{a{color:red}}",
    grass::Options::default().style(grass::OutputStyle::Compressed)
);
test!(
    compresses_selector_with_space_after_comma,
    "a, b {\n  color: red;\n}\n",
    "a,b{color:red}",
    grass::Options::default().style(grass::OutputStyle::Compressed)
);
test!(
    compresses_selector_with_newline_after_comma,
    "a,\nb {\n  color: red;\n}\n",
    "a,b{color:red}",
    grass::Options::default().style(grass::OutputStyle::Compressed)
);
test!(
    emits_bom_when_compressed,
    "a {\n  color: 👭;\n}\n",
    "\u{FEFF}a{color:👭}",
    grass::Options::default().style(grass::OutputStyle::Compressed)
);
test!(
    removes_space_between_selector_combinator,
    "a > b {\n  color: red;\n}\n",
    "a>b{color:red}",
    grass::Options::default().style(grass::OutputStyle::Compressed)
);
test!(
    removes_multiline_comment_before_style,
    "a {\n  /* abc */\n  color: red;\n}\n",
    "a{color:red}",
    grass::Options::default().style(grass::OutputStyle::Compressed)
);
test!(
    #[ignore = "regress not emitting the trailing semicolon here"]
    removes_multiline_comment_after_style,
    "a {\n  color: red;\n  /* abc */\n}\n",
    "a{color:red}",
    grass::Options::default().style(grass::OutputStyle::Compressed)
);
test!(
    removes_multiline_comment_between_styles,
    "a {\n  color: red;\n  /* abc */\n  color: green;\n}\n",
    "a{color:red;color:green}",
    grass::Options::default().style(grass::OutputStyle::Compressed)
);
test!(
    removes_multiline_comment_before_ruleset,
    "/* abc */a {\n  color: red;\n}\n",
    "a{color:red}",
    grass::Options::default().style(grass::OutputStyle::Compressed)
);
test!(
    keeps_preserved_multiline_comment_before_ruleset,
    "/*! abc */a {\n  color: red;\n}\n",
    "/*! abc */a{color:red}",
    grass::Options::default().style(grass::OutputStyle::Compressed)
);
test!(
    removes_multiline_comment_after_ruleset,
    "a {\n  color: red;\n}\n/* abc */",
    "a{color:red}",
    grass::Options::default().style(grass::OutputStyle::Compressed)
);
test!(
    removes_multiline_comment_between_rulesets,
    "a {\n  color: red;\n}\n/* abc */b {\n  color: green;\n}\n",
    "a{color:red}b{color:green}",
    grass::Options::default().style(grass::OutputStyle::Compressed)
);
test!(
    removes_spaces_in_comma_separated_list,
    "a {\n  color: a, b, c;\n}\n",
    "a{color:a,b,c}",
    grass::Options::default().style(grass::OutputStyle::Compressed)
);
test!(
    removes_leading_zero_in_number_under_1,
    "a {\n  color: 0.5;\n}\n",
    "a{color:.5}",
    grass::Options::default().style(grass::OutputStyle::Compressed)
);
test!(
    removes_leading_zero_in_number_under_1_in_rgba_alpha_channel,
    "a {\n  color: rgba(1, 1, 1, 0.5);\n}\n",
    "a{color:rgba(1,1,1,.5)}",
    grass::Options::default().style(grass::OutputStyle::Compressed)
);
test!(
    retains_leading_zero_in_opacity,
    "a {\n  color: opacity(0.5);\n}\n",
    "a{color:opacity(0.5)}",
    grass::Options::default().style(grass::OutputStyle::Compressed)
);
test!(
    retains_leading_zero_in_saturate,
    "a {\n  color: saturate(0.5);\n}\n",
    "a{color:saturate(0.5)}",
    grass::Options::default().style(grass::OutputStyle::Compressed)
);
test!(
    retains_leading_zero_in_grayscale,
    "a {\n  color: grayscale(0.5);\n}\n",
    "a{color:grayscale(0.5)}",
    grass::Options::default().style(grass::OutputStyle::Compressed)
);
test!(
    retains_zero_without_decimal,
    "a {\n  color: 0.0;\n  color: 0;\n}\n",
    "a{color:0;color:0}",
    grass::Options::default().style(grass::OutputStyle::Compressed)
);
test!(
    color_can_be_three_hex,
    "a {\n  color: white;\n}\n",
    "a{color:#fff}",
    grass::Options::default().style(grass::OutputStyle::Compressed)
);
test!(
    color_cant_be_three_hex_but_hex_is_shorter,
    "a {\n  color: aquamarine;\n}\n",
    "a{color:#7fffd4}",
    grass::Options::default().style(grass::OutputStyle::Compressed)
);
test!(
    slash_list,
    "@use 'sass:list'; a {\n  color: list.slash(a, b, c);\n}\n",
    "a{color:a/b/c}",
    grass::Options::default().style(grass::OutputStyle::Compressed)
);
test!(
    retains_space_between_calc_operations,
    "a {\n  width: calc(100% + 32px);\n}\n",
    "a{width:calc(100% + 32px)}",
    grass::Options::default().style(grass::OutputStyle::Compressed)
);
test!(
    cyan_normalized_to_aqua,
    "a {\n  color: cyan;\n}\n",
    "a{color:aqua}",
    grass::Options::default().style(grass::OutputStyle::Compressed)
);
