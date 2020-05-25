#![cfg(test)]

#[macro_use]
mod macros;

test!(
    keyframes_content_inside_keyframes,
    "@mixin foo {\n  @keyframes {\n    @content;\n  }\n}\na {\n  @include foo {\n    color: red;\n  };\n}",
    "@mixin foo {\n  @keyframes {\n    @content;\n  }\na {\n  @include foo {\n    color: red;\n  };\n}"
);

test!(
    keyframes_empty_keyframes_is_retained,
    "@keyframes {}\n",
    "@keyframes {}\n"
);

test!(keyframes_keyframes_inside_ruleset,
    "a {\n  @keyframes {}\n}",
    "a {\n  @keyframes {}\n}"
);

test!(
    keyframes_keyframes_inside_ruleset_with_other_styles,
    "a {\n  color: red;\n  @keyframes {}\n  color: green;\n}",
    "a {\n  color: red;\n  @keyframes {}\n  color: green;\n}"
);

test!(
    keyframes_keyframes_lowercase_to,
    "@keyframes {to {color: red;}}",
    "@keyframes {to {color: red;}}"
);

test!(
    keyframes_keyframes_lowercase_from,
    "@keyframes {from {color: red;}}",
    "@keyframes {from {color: red;}}"
);

test!(
    keyframes_keyframes_capital_to,
    "@keyframes {TO {color: red;}}",
    "@keyframes {TO {color: red;}}"
);

test!(
    keyframes_keyframes_capital_from,
    "@keyframes {FROM {color: red;}}",
    "@keyframes {FROM {color: red;}}"
);

error!(
    keyframes_invalid_keyword_selectors,
    "@keyframes {foo {}}",
    r#"Expected "to" or "from""#
);

error!(
    keyframes_number_selector_without_percent,
    "@keyframes {10 {}}",
    r#"Expected "%""#
);

test!(
    keyframes_keyframes_simple_percent_selector,
    "@keyframes {0% {color: red;}}",
    "@keyframes {0% {color: red;}}"
);

test!(
    keyframes_keyframes_comma_separated_percent_selectors,
    "@keyframes {0%, 5%, 10%, 15% {color: red;}}",
    "@keyframes {0%, 5%, 10%, 15% {color: red;}}"
);

test!(
    keyframes_with_parameters,
    "@keyframes foo {}",
    "@keyframes foo {}"
);

test!(
    keyframes_variable_in_parameters,
    "$foo: 1;\n @keyframes $foo {}",
    "$foo: 1;\n @keyframes $foo {}"
);

test!(
    keyframes_arithmetic_in_parameters,
    "@keyframes 1 + 2 {}"
);

test!(
    keyframes_interpolation_in_parameters,
    "$foo: 1;\n @keyframes #{$foo + 2} {}",
    "$foo: 1;\n @keyframes #{$foo + 2} {}"
);

test!(
    keyframes_multiline_comment_inside_keyframes,
    "@keyframes foo {/**/}",
    "@keyframes foo {/**/}"
);

test!(
    keyframes_multiline_comment_inside_keyframes_params,
    "@keyframes fo/**/o {}",
    "@keyframes fo/**/o {}"
);

test!(
    keyframes_silent_comment_inside_keyframes_params,
    "@keyframes fo//o {}",
    "@keyframes fo//o {}"
);

test!(
    keyframes_nested_rulesets,
    "@keyframes {\n  to {\n    to {\n      color: red;\n    }\n  }\n}",
    "@keyframes {\n  to {\n    to {\n      color: red;\n    }\n  }\n}"
);

error!(
    keyframes_invalid_nested_rulesets,
    "@keyframes {\n  to {\n    a {}\n  }\n}",
    r#"Expected "to" or "from""#
);

test!(
    keyframes_multiple_rulesets,
    "@keyframes {\n  to {\n    color: red;\n  }\n  from {\n    color: green;\n  }\n}",
    "@keyframes {\n  to {\n    color: red;\n  }\n  from {\n    color: green;\n  }\n}"
);

test!(
    keyframes_multiline_params,
    "@keyframes fo\n\no {}",
    "@keyframes fo\no {}"
);

test!(
    keyframes_test_valid_interpolated_selector,
    "@keyframes {\n  #{to} {\n    color: red;\n  }\n}",
    "@keyframes {\n  to {\n    color: red;\n  }\n}"
);
