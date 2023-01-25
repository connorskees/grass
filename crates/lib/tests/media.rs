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
    "@media print {\n  a {\n    color: red;\n  }\n  b {\n    color: green;\n  }\n}\n"
);
test!(
    removes_media_if_all_children_are_blank,
    "@media foo {
      a {}
    }",
    ""
);
test!(
    correct_order_of_children_when_merging,
    "@media (foo) {
      @media (bar) {
        a {
          color: red;
        }
      }

      a {
        color: red;
      }
    }",
    "@media (foo) and (bar) {\n  a {\n    color: red;\n  }\n}\n@media (foo) {\n  a {\n    color: red;\n  }\n}\n"
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
    nested_media_with_compatible_queries,
    "@media (foo) {
      @media (bar) {
        a {
          color: red;
        }
      }
    }",
    "@media (foo) and (bar) {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    nested_media_with_incompatible_queries,
    "@media foo {
      @media bar {
        a {
          color: red;
        }
      }
    }",
    ""
);
test!(
    removes_media_if_all_children_are_placeholder,
    "@media foo {
      %a {
        color: red;
      }
    }",
    ""
);
test!(
    plain_import_inside_media_is_not_moved_to_top,
    r#"@media foo {
      a {
        color: red;
      }
    
      @import "foo.css";
    }"#,
    "@media foo {\n  a {\n    color: red;\n  }\n  @import \"foo.css\";\n}\n"
);
error!(
    media_feature_missing_closing_paren,
    "@media foo and (bar:a", "Error: expected \")\"."
);
error!(
    media_feature_missing_curly_brace_after_hash,
    "@media foo and # {}", "Error: expected \"{\"."
);
error!(
    // note: dart-sass gives error "Expected expression"
    nothing_after_not_in_parens,
    "@media (not", "Error: Expected whitespace."
);
error!(
    nothing_after_and,
    "@media foo and", "Error: Expected whitespace."
);
error!(nothing_after_or, "@media foo or", r#"Error: expected "{"."#);
error!(
    no_parens_after_and,
    "@media foo and bar {
        a {
            color: red;
        }
    }",
    "Error: expected media condition in parentheses."
);
test!(
    query_starts_with_interpolation,
    "@media #{foo} {
      a {
        color: red;
      }
    }",
    "@media foo {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    query_is_parens_with_comma,
    "@media (foo, bar) {
      a {
        color: red;
      }
    }",
    "@media (foo, bar) {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    query_is_parens_with_space_before_comma,
    "@media (foo , bar) {
      a {
        color: red;
      }
    }",
    "@media (foo, bar) {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    query_and_first_has_no_parens,
    "@media foo and (bar) {
      a {
        color: red;
      }
    }",
    "@media foo and (bar) {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    query_comma_separated_list_both_parens,
    "@media (foo), (bar) {
      a {
        color: red;
      }
    }",
    "@media (foo), (bar) {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    query_comma_separated_list_both_parens_space_before_paren,
    "@media (foo) , (bar) {
      a {
        color: red;
      }
    }",
    "@media (foo), (bar) {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    query_comma_separated_list_loud_comments,
    "@media /**/foo/**/,/**/bar/**/ {
      a {
        color: red;
      }
    }",
    "@media foo, bar {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    query_not_paren,
    "@media not (color) {
      a {
        color: red;
      }
    }",
    "@media not (color) {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    many_parens,
    "@media (((color))) {
      a {
        color: red;
      }
    }",
    "@media (((color))) {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    many_parens_around_and,
    "@media ((screen and (color))) {
      a {
        color: red;
      }
    }",
    "@media ((color)) {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    newline_between_media_rules_declared_at_root_inside_each,
    "@each $a in 1 2 3 {
        a {
            @media foo {
                b {
                    color: $a;
                }
            }

            color: foo;
        }
    }",
    "a {\n  color: foo;\n}\n@media foo {\n  a b {\n    color: 1;\n  }\n}\n\na {\n  color: foo;\n}\n@media foo {\n  a b {\n    color: 2;\n  }\n}\n\na {\n  color: foo;\n}\n@media foo {\n  a b {\n    color: 3;\n  }\n}\n"
);
test!(
    newline_between_media_rules_declared_at_root_inside_each_with_preceding_style_rule,
    "@each $a in 1 2 {
        a {
            color: red;
        }

        @media foo {
            a {
                color: $a;
            }
        }
    }",
    "a {\n  color: red;\n}\n\n@media foo {\n  a {\n    color: 1;\n  }\n}\na {\n  color: red;\n}\n\n@media foo {\n  a {\n    color: 2;\n  }\n}\n"
);
test!(
    no_newline_between_media_rules_when_invisble_rule_between,
    "a {}

      @media (min-width: 5px) {
          a {
              color: 1;
          }
      }

      a {}

      @media (min-width: 5px) {
          a {
              color: 1;
          }
      }",
    "@media (min-width: 5px) {\n  a {\n    color: 1;\n  }\n}\n@media (min-width: 5px) {\n  a {\n    color: 1;\n  }\n}\n"
);
test!(
    two_media_rules_in_content_block,
    "@mixin foo() {
        @content;
    }

    @include foo {
        @media foo {
            a {
                color: red;
            }
        }
        @media foo {
            b {
                color: red;
            }
        }
    }",
    "@media foo {\n  a {\n    color: red;\n  }\n}\n@media foo {\n  b {\n    color: red;\n  }\n}\n"
);
test!(
    splits_child_nodes_when_preceding_media,
    "@media (foo) {
        @media (prefers-reduced-motion: reduce) {
            a {
                transition: none;
            }
        }

        a {
            color: red;
        }

        a {
            color: red;
        }
    }",
    "@media (foo) and (prefers-reduced-motion: reduce) {\n  a {\n    transition: none;\n  }\n}\n@media (foo) {\n  a {\n    color: red;\n  }\n}\n@media (foo) {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    doesnt_split_child_nodes_when_trailing_media,
    "@media (foo) {
        a {
            color: red;
        }

        a {
            color: red;
        }

        @media (prefers-reduced-motion: reduce) {
            a {
                transition: none;
            }
        }
    }",
    "@media (foo) {\n  a {\n    color: red;\n  }\n  a {\n    color: red;\n  }\n}\n@media (foo) and (prefers-reduced-motion: reduce) {\n  a {\n    transition: none;\n  }\n}\n"
);
test!(
    #[ignore = "our is_invisible_check inside css tree is flawed here"]
    doesnt_split_child_nodes_when_leading_but_invisible_media,
    "@media (foo) {
        @media (prefers-reduced-motion: reduce) {}

        a {
            color: red;
        }

        a {
            color: red;
        }
    }",
    "@media (foo) {\n  a {\n    color: red;\n  }\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    media_has_url_in_parens,
    "@media (url) {
        a {
            color: red;
        }
    }",
    "@media (url) {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    #[ignore = "our is_invisible_check inside css tree is flawed here"]
    media_does_not_split_when_child_rule_has_invisible_media,
    "@media (min-width: 1px) {
        .first {
            font-weight: 100;

            @media (min-width: 2px) {}
        }

        .second {
            font-weight: 200;
        }
    }",
    "@media (min-width: 1px) {\n  .first {\n    font-weight: 100;\n  }\n  .second {\n    font-weight: 200;\n  }\n}\n"
);
test!(
    escaped_nullbyte_in_query,
    r#"@media (min-width:\0) {
        a {
            color: red;
        }
    }"#,
    "@media (min-width: \\0 ) {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    simple_unmergeable,
    "a {
        @media a {
            @media b {
                color: red;
            }
        }
    }",
    ""
);
test!(
    query_is_identifier_and_not_parens,
    "@media screen and not (foo) {
        a {
            color: red;
        }
    }",
    "@media screen and not (foo) {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    query_is_identifier_identifier_and_parens,
    "@media only screen and (foo) {
        a {
            color: red;
        }
    }",
    "@media only screen and (foo) {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    query_is_paren_and_paren,
    "@media (foo) and (bar) {
        a {
            color: red;
        }
    }",
    "@media (foo) and (bar) {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    query_is_paren_or_paren,
    "@media (foo) or (bar) {
        a {
            color: red;
        }
    }",
    "@media (foo) or (bar) {\n  a {\n    color: red;\n  }\n}\n"
);
error!(
    media_query_has_quoted_closing_paren,
    r#"@media ('a)'w) {
        a {
            color: red;
        }
    }"#,
    "Error: expected no more input."
);
error!(
    empty_query_after_resolving_interpolation,
    "@media #{null} {}", "Error: Expected identifier."
);
