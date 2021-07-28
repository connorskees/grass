#[macro_use]
mod macros;

test!(
    basic_toplevel,
    "@media foo {\n  a {\n    color: red;\n  }\n}\n",
    "@media foo {\n  a {\n    color: red;\n  }\n}\n"
);
error!(
    no_params,
    "@media {\n  a {\n    color: red;\n  }\n}\n", "Error: Expected identifier."
);
test!(
    basic_nested,
    "a {\n  @media foo {\n  color: red;\n  }\n}\n",
    "@media foo {\n  a {\n    color: red;\n  }\n}\n"
);
test!(empty_body, "@media (min-width: 2px) {}", "");
test!(
    newlines_are_not_emitted_for_child_styles,
    "a {
        @media screen {
            b {
                color: red;
            }
            c {
                color: green;
            }
        }
    }",
    "@media screen {\n  a b {\n    color: red;\n  }\n  a c {\n    color: green;\n  }\n}\n"
);
test!(
    multiple_identifiers_in_query,
    "@media not screen {
        a {
            color: red;
        }
    }",
    "@media not screen {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    multiple_identifiers_in_query_second_is_and,
    "@media print and (foo: 1 2 3) {
        a {
            color: red;
        }
    }",
    "@media print and (foo: 1 2 3) {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    single_identifier_inside_parens,
    "@media (color) {a {color: red;}}",
    "@media (color) {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    quoted_colon_in_parens,
    "@media screen and (\":\") {
        a {
            color: red;
        }
    }",
    "@media screen and (:) {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    multiline_comments_everywhere,
    "@media/**/foo/**/and/**/(/**/bar/**/)/**/{
        a {
            color: red;
        }
    }",
    "@media foo and (bar) {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    comparison_in_query,
    "@media (100px < 400px) {
        a {
            interpolation: in-parens
        }
    }",
    "@media (100px < 400px) {\n  a {\n    interpolation: in-parens;\n  }\n}\n"
);
test!(
    interpolated_comparison_in_query,
    "@media (#{100px < 400px}) {
        a {
            interpolation: in-parens
        }
    }",
    "@media (true) {\n  a {\n    interpolation: in-parens;\n  }\n}\n"
);
test!(
    single_eq_in_query,
    "@media (height=600px) {
        a {
            b: c
        }
    }
    ",
    "@media (height = 600px) {\n  a {\n    b: c;\n  }\n}\n"
);
test!(
    double_eq_in_query,
    "@media (height==600px) {
        a {
            b: c
        }
    }
    ",
    "@media (false) {\n  a {\n    b: c;\n  }\n}\n"
);
test!(
    newline_emitted_for_different_toplevel_rulesets,
    "@media print {
      a {
        color: red;
      }
    
      b {
        color: green;
      }
    }",
    "@media print {\n  a {\n    color: red;\n  }\n\n  b {\n    color: green;\n  }\n}\n"
);
test!(
    newline_emitted_before_media_when_following_ruleset,
    "a {
      color: red;
    }
    @media print {
      a {
        color: red;
      }
    }",
    "a {\n  color: red;\n}\n\n@media print {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    no_newline_emitted_between_two_media_rules,
    "@media print {
      a {
        color: red;
      }
    }
    @media print {
      a {
        color: red;
      }
    }",
    "@media print {\n  a {\n    color: red;\n  }\n}\n@media print {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    no_newline_emitted_between_two_media_rules_when_in_same_ruleset,
    "a {
      @media foo {
        color: red;
      }
    
      @media bar {
        color: green;
      }
    }",
    "@media foo {\n  a {\n    color: red;\n  }\n}\n@media bar {\n  a {\n    color: green;\n  }\n}\n"
);
test!(
    allows_interpolated_at_rule,
    "@#{media} (true) {
      a {
        color: red;
      }
    }",
    "@media (true) {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    no_newline_between_two_media_following_ruleset,
    "a {
      color: red;
    }
    
    @media (min-width: 0px) {
      a {
        color: red;
      }
    }
    
    @media (min-width: 0px) {
      a {
        color: red;
      }
    }",
    "a {\n  color: red;\n}\n\n@media (min-width: 0px) {\n  a {\n    color: red;\n  }\n}\n@media (min-width: 0px) {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    no_newline_after_media_after_ruleset,
    "a {
      color: red;
    }
    
    @media (min-width: 0px) {
      b {
        color: red;
      }
    }
    
    d {
      color: red;
    }",
    "a {\n  color: red;\n}\n\n@media (min-width: 0px) {\n  b {\n    color: red;\n  }\n}\nd {\n  color: red;\n}\n"
);
test!(
    no_newline_after_media_when_outer,
    "@media (min-width: 0px) {
      b {
        color: red;
      }
    }
    
    d {
      color: red;
    }",
    "@media (min-width: 0px) {\n  b {\n    color: red;\n  }\n}\nd {\n  color: red;\n}\n"
);
test!(
    newline_after_media_when_inner,
    "a {
      @media (max-width: 0px) {
        color: red;
      }
    }
    
    a {
      color: red;
    }",
    "@media (max-width: 0px) {\n  a {\n    color: red;\n  }\n}\n\na {\n  color: red;\n}\n"
);
test!(
  #[ignore = "we move to top of media"]
    plain_import_inside_media_is_not_moved_to_top,
    r#"@media foo {
      a {
        color: red;
      }
    
      @import "foo.css";
    }"#,
    "@media foo {\n  a {\n    color: red;\n  }\n\n  @import \"foo.css\";\n}\n"
);

error!(
    media_feature_missing_closing_paren,
    "@media foo and (bar:a", "Error: expected \")\"."
);
error!(
    media_feature_missing_curly_brace_after_hash,
    "@media foo and # {}", "Error: expected \"{\"."
);
