#[macro_use]
mod macros;

test!(
    content_inside_keyframes,
    "@mixin foo {
        @keyframes {
          @content;
        }
      }
      a {
        @include foo {
          color: red;
        };
      }",
    "@keyframes {\n  color: red;\n}\n"
);

test!(
    empty_keyframes_is_emitted_exact,
    "@keyframes {}",
    "@keyframes {}\n"
);
test!(
    keyframes_is_at_root,
    "a {\n    @keyframes {}\n}\n",
    "@keyframes {}\n"
);
test!(
    keyframes_inside_ruleset_with_other_styles,
    "a {
        color: red;
        @keyframes {}
        color: green;
    }",
    "a {\n  color: red;\n  color: green;\n}\n@keyframes {}\n"
);
test!(
    keyframes_lowercase_to,
    "@keyframes {to {color: red;}}",
    "@keyframes {\n  to {\n    color: red;\n  }\n}\n"
);
test!(
    keyframes_lowercase_from,
    "@keyframes {from {color: red;}}",
    "@keyframes {\n  from {\n    color: red;\n  }\n}\n"
);
test!(
    keyframes_uppercase_to,
    "@keyframes {TO {color: red;}}",
    "@keyframes {\n  to {\n    color: red;\n  }\n}\n"
);
test!(
    keyframes_uppercase_from,
    "@keyframes {FROM {color: red;}}",
    "@keyframes {\n  from {\n    color: red;\n  }\n}\n"
);
error!(
    keyframes_invalid_selector_beginning_with_f,
    "@keyframes {foo {}}", "Error: Expected \"to\" or \"from\"."
);
error!(
    keyframes_invalid_selector_beginning_with_t,
    "@keyframes {too {}}", "Error: Expected \"to\" or \"from\"."
);
error!(
    keyframes_invalid_selector_beginning_with_ascii_char,
    "@keyframes {a {}}", "Error: Expected \"to\" or \"from\"."
);
error!(
    keyframes_invalid_selector_number_missing_percent,
    "@keyframes {10 {}}", "Error: expected \"%\"."
);
test!(
    keyframes_simple_percent_selector,
    "@keyframes {0% {color: red;}}",
    "@keyframes {\n  0% {\n    color: red;\n  }\n}\n"
);
test!(
    keyframes_comma_separated_percent_selectors,
    "@keyframes {0%, 5%, 10%, 15% {color: red;}}",
    "@keyframes {\n  0%, 5%, 10%, 15% {\n    color: red;\n  }\n}\n"
);
test!(
    keyframes_empty_with_name,
    "@keyframes foo {}",
    "@keyframes foo {}\n"
);
test!(
    keyframes_variable_in_name,
    "@keyframes $foo {}",
    "@keyframes $foo {}\n"
);
test!(
    keyframes_arithmetic_in_name,
    "@keyframes 1 + 2 {}",
    "@keyframes 1 + 2 {}\n"
);
test!(
    keyframes_interpolation_in_name,
    "@keyframes #{1 + 2} {}",
    "@keyframes 3 {}\n"
);
test!(
    keyframes_contains_multiline_comment,
    "@keyframes foo {/**/}",
    "@keyframes foo {\n  /**/\n}\n"
);
test!(
    keyframes_multiple_rulesets,
    "@keyframes {
        to {
            color: red;
        }
        from {
            color: green;
        }
    }",
    "@keyframes {\n  to {\n    color: red;\n  }\n  from {\n    color: green;\n  }\n}\n"
);
test!(
    keyframes_vendor_prefix,
    "@-webkit-keyframes foo {
        0% {
            color: red;
        }
    }",
    "@-webkit-keyframes foo {\n  0% {\n    color: red;\n  }\n}\n"
);
test!(
    keyframes_percent_has_e,
    "@keyframes foo {
        1e2% {
            color: red;
        }
    }",
    "@keyframes foo {\n  1e2% {\n    color: red;\n  }\n}\n"
);
test!(
    keyframes_percent_has_plus_e,
    "@keyframes foo {
        1e+2% {
            color: red;
        }
    }",
    "@keyframes foo {\n  1e+2% {\n    color: red;\n  }\n}\n"
);
test!(
    keyframes_percent_has_negative_e,
    "@keyframes foo {
        1e-2% {
            color: red;
        }
    }",
    "@keyframes foo {\n  1e-2% {\n    color: red;\n  }\n}\n"
);
test!(
    keyframes_allow_decimal_selector,
    "@keyframes foo {
        12.5% {
            color: red;
        }
    }",
    "@keyframes foo {\n  12.5% {\n    color: red;\n  }\n}\n"
);
test!(
    keyframes_hash_in_name,
    "@keyframes #identifier {
      to {
        color: red;
      }
    }",
    "@keyframes #identifier {\n  to {\n    color: red;\n  }\n}\n"
);
test!(
    keyframes_interpolated_selector,
    "@keyframes foo {
      #{t}o {
        color: red;
      }
    }",
    "@keyframes foo {\n  to {\n    color: red;\n  }\n}\n"
);
error!(
    keyframes_denies_selector_with_hash,
    "@keyframes foo {
      #to {
        color: red;
      }
    }",
    "Error: Expected number."
);
error!(
    keyframes_nothing_after_forward_slash_in_selector,
    "@keyframes foo { a/", "Error: expected \"{\"."
);
error!(
    keyframes_no_ident_after_forward_slash_in_selector,
    "@keyframes foo { a/ {} }", "Error: Expected \"to\" or \"from\"."
);
error!(
    keyframes_nothing_after_selector,
    "@keyframes foo { a", "Error: expected \"{\"."
);
test!(
    e_alone,
    "@keyframes foo {
      1e3% {
        color: red;
      }
    }",
    "@keyframes foo {\n  1e3% {\n    color: red;\n  }\n}\n"
);
test!(
    e_with_plus,
    "@keyframes foo {
      1e+3% {
        color: red;
      }
    }",
    "@keyframes foo {\n  1e+3% {\n    color: red;\n  }\n}\n"
);
test!(
    e_with_minus,
    "@keyframes foo {
      1e-3% {
        color: red;
      }
    }",
    "@keyframes foo {\n  1e-3% {\n    color: red;\n  }\n}\n"
);
test!(
    e_with_decimal_plus,
    "@keyframes foo {
      1.5e+3% {
        color: red;
      }
    }",
    "@keyframes foo {\n  1.5e+3% {\n    color: red;\n  }\n}\n"
);
test!(
    e_with_decimal_no_number_after_decimal,
    "@keyframes foo {
      1.e3% {
        color: red;
      }
    }",
    "@keyframes foo {\n  1.e3% {\n    color: red;\n  }\n}\n"
);
test!(
    uppercase_e,
    "@keyframes foo {
      1E3% {
        color: red;
      }
    }",
    "@keyframes foo {\n  1e3% {\n    color: red;\n  }\n}\n"
);
test!(
    escaped_e,
    "@keyframes foo {
      1\\65 3% {
        color: red;
      }
    }",
    "@keyframes foo {\n  1e3% {\n    color: red;\n  }\n}\n"
);
test!(
    uppercase_escaped_e,
    "@keyframes foo {
      1\\45 3% {
        color: red;
      }
    }",
    "@keyframes foo {\n  1e3% {\n    color: red;\n  }\n}\n"
);
test!(
    style_rule_before_keyframes,
    "a {
        color: red;
    }

    @keyframes spinner-border {
        to {
            color: red;
        }
    }",
    "a {\n  color: red;\n}\n\n@keyframes spinner-border {\n  to {\n    color: red;\n  }\n}\n"
);
test!(
    style_rule_after_keyframes,
    "@keyframes spinner-border {
        to {
            color: red;
        }
    }

    a {
        color: red;
    }",
    "@keyframes spinner-border {\n  to {\n    color: red;\n  }\n}\na {\n  color: red;\n}\n"
);
test!(
    percent_selector_leading_plus,
    "@keyframes foo {
        +5% {
            color: red;
        }
    }",
    "@keyframes foo {\n  +5% {\n    color: red;\n  }\n}\n"
);
error!(
    invalid_escape_in_place_of_e,
    "@keyframes foo {
      1\\110000 3% {
        color: red;
      }
    }",
    r#"Error: expected "%"."#
);
error!(
    percent_selector_no_number_after_e,
    "@keyframes foo {
        5e% {
            color: red;
        }
    }",
    r#"Error: Expected digit."#
);
error!(
    selector_is_empty_after_interpolation_is_resolved,
    "@keyframes foo {
        #{null} {}
    }",
    r#"Error: Expected number."#
);

// todo: span for this
// @keyframes foo {
//   1\1100000000000000 3% {
//     // color: \110000;
//   }
// }
