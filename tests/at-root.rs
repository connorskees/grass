#![cfg(test)]

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
