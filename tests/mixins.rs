#![cfg(test)]

#[macro_use]
mod macros;

test!(
    basic_mixin,
    "@mixin a {\n  color: red;\n}\n\nb {\n  @include a;\n}\n",
    "b {\n  color: red;\n}\n"
);
test!(empty_mixin, "@mixin a {}\n\nb {\n  @include a;\n}\n", "");
test!(
    just_a_comment,
    "@mixin foo() {\n  /* begin foo */\n}\n\na {\n    @include foo();\n}\n",
    "a {\n  /* begin foo */\n}\n"
);
test!(
    mixin_two_styles,
    "@mixin a {\n  color: red;\n  color: blue;\n}\n\nb {\n  @include a;\n}\n",
    "b {\n  color: red;\n  color: blue;\n}\n"
);
test!(
    mixin_ruleset,
    "@mixin a {\n  b {\n    color: red;\n  }\n}\nb {\n  @include a;\n}\n",
    "b b {\n  color: red;\n}\n"
);
test!(
    mixin_two_rulesets,
    "@mixin a {\n  b {\n    color: red;\n  }\n  c {\n    color: blue;\n  }\n}\nd {\n  @include a;\n}\n",
    "d b {\n  color: red;\n}\nd c {\n  color: blue;\n}\n"
);
test!(
    mixin_ruleset_and_style,
    "@mixin a {\n  b {\n    color: red;\n  }\n  color: blue;\n}\nd {\n  @include a;\n}\n",
    "d {\n  color: blue;\n}\nd b {\n  color: red;\n}\n"
);
test!(
    mixin_style_and_ruleset,
    "@mixin a {\n  color: blue;\n  b {\n    color: red;\n}\n}\nd {\n  @include a;\n}\n",
    "d {\n  color: blue;\n}\nd b {\n  color: red;\n}\n"
);
test!(
    mixin_nested_rulesets,
    "@mixin a {\n  b {\n    c {\n      color: red;\n}\n}\n}\nd {\n  @include a;\n}\n",
    "d b c {\n  color: red;\n}\n"
);
test!(
    mixin_removes_empty_ruleset,
    "@mixin a {\n  color: red; b {\n}\n}\nd {\n  @include a;\n}\n",
    "d {\n  color: red;\n}\n"
);
test!(
    mixin_variable_scope_one_ruleset,
    "@mixin a {\n  $a: blue;\nb {\n  $a: red;\n}  color: $a\n}\nd {\n  @include a;\n}\n",
    "d {\n  color: red;\n}\n"
);
test!(
    mixin_no_args,
    "@mixin a {\n  color: red;\n}\nd {\n  @include a();\n}\n",
    "d {\n  color: red;\n}\n"
);
test!(
    mixin_single_arg,
    "@mixin a($b) {\n  color: $b;\n}\nd {\n  @include a(red);\n}\n",
    "d {\n  color: red;\n}\n"
);
test!(
    mixin_two_args,
    "@mixin a($b, $c) {\n  color: $b;\n  color: $c\n}\nd {\n  @include a(red, blue);\n}\n",
    "d {\n  color: red;\n  color: blue;\n}\n"
);
test!(
    mixin_arg_trailing_comma,
    "@mixin a($b, $c,) {\n  color: $b;\n  color: $c\n}\nd {\n  @include a(red, blue);\n}\n",
    "d {\n  color: red;\n  color: blue;\n}\n"
);
test!(
    mixin_property_interpolation,
    "@mixin a($b) {\n  #{$b}: red;\n}\nd {\n  @include a(color);\n}\n",
    "d {\n  color: red;\n}\n"
);
test!(
    mixin_style_interpolation,
    "@mixin a($b) {\n  color: #{$b};\n}\nd {\n  @include a(red);\n}\n",
    "d {\n  color: red;\n}\n"
);
test!(
    mixin_simple_default_value,
    "@mixin a($b: red) {\n  color: $b;\n}\nd {\n  @include a;\n}\n",
    "d {\n  color: red;\n}\n"
);
test!(
    mixin_second_value_default,
    "@mixin a($a, $b: blue) {\n  color: $a $b;\n}\nd {\n  @include a(red);\n}\n",
    "d {\n  color: red blue;\n}\n"
);
test!(
    mixin_two_default_values,
    "@mixin a($a: red, $b: blue) {\n  color: $a $b;\n}\nd {\n  @include a;\n}\n",
    "d {\n  color: red blue;\n}\n"
);
test!(
    mixin_override_default_value_positionally,
    "@mixin a($a: red) {\n  color: $a;\n}\nd {\n  @include a(blue);\n}\n",
    "d {\n  color: blue;\n}\n"
);
test!(
    mixin_keyword_arg,
    "@mixin a($a) {\n  color: $a;\n}\nd {\n  @include a($a: blue);\n}\n",
    "d {\n  color: blue;\n}\n"
);
test!(
    mixin_keyword_arg_override_default,
    "@mixin a($a: red) {\n  color: $a;\n}\nd {\n  @include a($a: blue);\n}\n",
    "d {\n  color: blue;\n}\n"
);
test!(
    mixin_keyword_applies_to_second_arg,
    "@mixin a($a: red, $b) {\n  color: $a $b;\n}\nd {\n  @include a($b: blue);\n}\n",
    "d {\n  color: red blue;\n}\n"
);
test!(
    mixin_two_keywords,
    "@mixin a($a, $b) {\n  color: $a $b;\n}\nd {\n  @include a($a: red, $b: blue);\n}\n",
    "d {\n  color: red blue;\n}\n"
);
test!(
    mixin_two_keywords_wrong_direction,
    "@mixin a($a, $b) {\n  color: $a $b;\n}\nd {\n  @include a($b: blue, $a: red);\n}\n",
    "d {\n  color: red blue;\n}\n"
);
test!(
    variable_in_call_args,
    "@mixin a($a) {\n  color: $a;\n}\nd {\n  $c: red;\n  @include a($c);\n}\n",
    "d {\n  color: red;\n}\n"
);
test!(
    comment_before_positional_call_arg,
    "@mixin a($a) {\n  color: $a;\n}\nd {\n  @include a(/*foo*/red);\n}\n",
    "d {\n  color: red;\n}\n"
);
test!(
    comment_after_positional_call_arg,
    "@mixin a($a) {\n  color: $a;\n}\nd {\n  @include a(red/*foo*/);\n}\n",
    "d {\n  color: red;\n}\n"
);
test!(
    comment_before_keyword_call_arg_val,
    "@mixin a($a) {\n  color: $a;\n}\nd {\n  @include a($a: /*foo*/red);\n}\n",
    "d {\n  color: red;\n}\n"
);
test!(
    comment_after_keyword_call_arg_val,
    "@mixin a($a) {\n  color: $a;\n}\nd {\n  @include a($a: red/*foo*/);\n}\n",
    "d {\n  color: red;\n}\n"
);
test!(
    comment_before_keyword_call_arg_name,
    "@mixin a($a) {\n  color: $a;\n}\nd {\n  @include a(/*foo*/$a: red);\n}\n",
    "d {\n  color: red;\n}\n"
);
test!(
    comment_after_keyword_call_arg_name,
    "@mixin a($a) {\n  color: $a;\n}\nd {\n  @include a($a/*foo*/: red);\n}\n",
    "d {\n  color: red;\n}\n"
);
test!(
    toplevel_include,
    "@mixin a {\n  a {\n    color: red;\n  }\n}\n\n@include a;\n",
    "a {\n  color: red;\n}\n"
);
