#[macro_use]
mod macros;

test!(
    simple_nested,
    ".foo {\n  @at-root {\n    .bar {a: b}\n  }\n}\n",
    ".bar {\n  a: b;\n}\n"
);
test!(
    with_selector,
    ".foo {\n  @at-root .bar {a: b}\n}\n",
    ".bar {\n  a: b;\n}\n"
);
test!(
    with_selector_in_mixin,
    "@mixin bar {\n  @at-root .bar {a: b}\n}\n\n.foo {\n  @include bar;\n}\n",
    ".bar {\n  a: b;\n}\n"
);
test!(
    with_super_selector,
    ".foo {\n  @at-root & {\n    a: b;\n  }\n}\n",
    ".foo {\n  a: b;\n}\n"
);
test!(
    nested_with_super_selector,
    ".foo {\n  @at-root & {\n    .bar {\n      @at-root & {\n        a: b;\n      }\n    }\n  }\n}\n",
    ".foo .bar {\n  a: b;\n}\n"
);
test!(
    deeply_nested_with_rulesets_and_styles,
    ".foo {\n  @at-root .bar {\n    a: b;\n    c {\n      d: e;\n      foo {\n        bar: baz;\n      }\n      h: j;\n    }\n    f: g;\n  }\n}\n",
    ".bar {\n  a: b;\n  f: g;\n}\n.bar c {\n  d: e;\n  h: j;\n}\n.bar c foo {\n  bar: baz;\n}\n"
);
test!(
    super_selector_inside_with_nothing,
    "foo {\n  @at-root {\n    & {\n      color: bar;\n    }\n  }\n}\n",
    "foo {\n  color: bar;\n}\n"
);
test!(
    interpolated_super_selector_with_nothing,
    "test {\n  @at-root {\n    #{&}post {\n      foo {\n        bar: baz;\n      }\n    }\n  }\n}\n",
    "testpost foo {\n  bar: baz;\n}\n"
);
test!(
    with_ampersand_single,
    "test {\n  @at-root {\n    #{&}post {\n      foo {\n        bar: baz;\n      }\n    }\n  }\n}\n",
    "testpost foo {\n  bar: baz;\n}\n"
);
test!(
    root_interpolated_ampersand,
    "@at-root {\n  #{&}post {\n    foo {\n      bar: baz;\n    }\n  }\n}\n",
    "post foo {\n  bar: baz;\n}\n"
);
test!(
    nested_prefix_interpolated_ampersand,
    "test {\n  @at-root {\n    pre#{&} {\n      foo {\n        bar: baz;\n      }\n    }\n  }\n}\n",
    "pretest foo {\n  bar: baz;\n}\n"
);
test!(
    nested_alone_interpolated_ampersand,
    "test {\n  @at-root {\n    #{&} {\n      foo {\n        bar: baz;\n      }\n    }\n  }\n}\n",
    "test foo {\n  bar: baz;\n}\n"
);
test!(
    style_before_at_root,
    "a {}\n\n@at-root {\n    @-ms-viewport { width: device-width; }\n}\n",
    "@-ms-viewport {\n  width: device-width;\n}\n"
);
test!(
    newline_between_style_rules_with_same_parent_but_first_is_in_at_root,
    "a {
      @at-root {
        b {
          color: red;
        }
      }
    
      b {
        color: red;
      }
    }",
    "b {\n  color: red;\n}\n\na b {\n  color: red;\n}\n"
);
test!(
    at_root_between_other_styles_is_emitted_with_same_order,
    "a {
      a {
          color: red;
      }

      @at-root {
          b {
              color: red;
          }
      }

      c {
          color: red;
      }
    }",
    "a a {\n  color: red;\n}\nb {\n  color: red;\n}\n\na c {\n  color: red;\n}\n"
);
test!(
    no_newline_between_style_rules_when_there_exists_a_selector,
    "@at-root a {
      a {
        color: red;
      }
    
      a {
        color: red;
      }
    }",
    "a a {\n  color: red;\n}\na a {\n  color: red;\n}\n"
);
test!(
    newline_between_style_rules_when_there_does_not_exist_a_selector,
    "@at-root {
      a {
        color: red;
      }
    
      a {
        color: red;
      }
    }",
    "a {\n  color: red;\n}\n\na {\n  color: red;\n}\n"
);
test!(
    at_root_inside_media,
    "@media screen {
      @at-root a {
        color: red;
      }
    }",
    "@media screen {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    at_root_inside_media_inside_style_rule,
    "b {
      @media screen {
        @at-root a {
          color: red;
        }
      }
    }",
    "@media screen {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    simple_at_root_query,
    "a {
        @at-root (with: rule) {
            b: c;
        }
    }",
    "a {\n  b: c;\n}\n"
);
test!(
    without_media_inside_media_rule,
    "@media (min-width: 1337px) {
        .foo {
            content: baz;
        }

        @at-root (without: media) {
            .foo {
                content: bar;
            }
        }
    }",
    "@media (min-width: 1337px) {\n  .foo {\n    content: baz;\n  }\n}\n.foo {\n  content: bar;\n}\n"
);
test!(
    with_media_inside_media_rule_inside_supports_rule,
    "@media (min-width: 1337px) {
        @supports (color: red) {
            @at-root (with: media) {
                .foo {
                    content: bar;
                }
            }
        }
    }",
    "@media (min-width: 1337px) {\n  .foo {\n    content: bar;\n  }\n}\n"
);
test!(
    with_media_and_supports_inside_media_rule_inside_supports_rule,
    "@media (min-width: 1337px) {
        @supports (color: red) {
            @at-root (with: media supports) {
                .foo {
                    content: bar;
                }
            }
        }
    }",
    "@media (min-width: 1337px) {\n  @supports (color: red) {\n    .foo {\n      content: bar;\n    }\n  }\n}\n"
);
test!(
    without_keyframes_inside_keyframes,
    "@keyframes animation {
        @at-root (without: keyframes) {
            to {
                color: red;
            }
        }
    }",
    "@keyframes animation {}\nto {\n  color: red;\n}\n"
);
test!(
    at_root_has_its_own_scope,
    "$root_default: initial;
    $root_implicit: initial;
    $root_explicit: initial !global;

    @at-root {
        $root_implicit: outer;
        $root_explicit: outer !global;
        $root_default: outer !default;
        $local_implicit: outer;
        $local_explicit: outer !global;
        $local_default: outer !default;

        @at-root {
            $root_implicit: inner;
            $root_explicit: inner !global;
            $root_default: inner !default;
            $local_implicit: inner;
            $local_explicit: inner !global;
            $local_default: inner !default;
        }
    }

    result {
        root_default: $root_default;
        root_implicit: $root_implicit;
        root_explicit: $root_explicit;

        @if variable-exists(local_default) {
            local_default: $local_default;
        }

        @if variable-exists(local_implicit) {
            local_implicit: $local_implicit;
        }

        @if variable-exists(local_explicit) {
            local_explicit: $local_explicit;
        }
    }",
    "result {\n  root_default: initial;\n  root_implicit: initial;\n  root_explicit: inner;\n  local_explicit: inner;\n}\n"
);
test!(
    #[ignore = "we currently emit the empty unknown-at-rule"]
    inside_style_inside_unknown_at_rule,
    "@unknown {
        .foo {
            @at-root .bar {
                a: b
            }
        }
    }",
    "@unknown {\n  .bar {\n    a: b;\n  }\n}\n"
);
test!(
    query_begins_with_interpolation,
    "a {
        @at-root (#{wi}th: rule) {
            color: red;
        }
    }",
    "a {\n  color: red;\n}\n"
);
error!(
    missing_closing_curly_brace,
    "@at-root {", "Error: expected \"}\"."
);
error!(
    style_at_toplevel_without_selector,
    "@at-root { color: red; }", "Error: expected \"{\"."
);
error!(
    extend_inside_at_root_would_be_put_at_root_of_document,
    "a {
        @at-root {
            @extend b;
        }
    }",
    "Error: @extend may only be used within style rules."
);
error!(
    selector_is_empty_after_interpolation_is_resolved,
    "@at-root #{null} {}", "Error: expected selector."
);
error!(
    // todo: dart-sass gives error r#"Error: Expected "with" or "without"."#
    query_is_empty_parens_after_interpolation_is_resolved,
    "@at-root (#{null}) {}", r#"Error: Expected "without"."#
);
