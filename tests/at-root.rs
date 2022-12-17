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
error!(
    #[ignore = "we do not currently validate missing closing curly braces"]
    missing_closing_curly_brace,
    "@at-root {", "Error: expected \"}\"."
);
error!(
    style_at_toplevel_without_selector,
    "@at-root { color: red; }", "Error: Found style at the toplevel inside @at-root."
);
