#![cfg(test)]

#[macro_use]
mod macros;

test!(
    simple_nested,
    ".foo {\n  @at-root {\n    .bar {a: b}\n  }\n}\n"
    ".bar {\n  a: b;\n}\n"
);
test!(
    with_selector,
    ".foo {\n  @at-root .bar {a: b}\n}\n"
    ".bar {\n  a: b;\n}\n"
);
test!(
    with_selector_in_mixin,
    "@mixin bar {\n  @at-root .bar {a: b}\n}\n\n.foo {\n  @include bar;\n}\n"
    ".bar {\n  a: b;\n}\n"
);
test!(
    with_super_selector,
    ".foo {\n  @at-root & {\n    a: b;\n  }\n}\n"
    ".foo {\n  a: b;\n}\n"
);
test!(
    nested_with_super_selector,
    ".foo {\n  @at-root & {\n    .bar {\n      @at-root & {\n        a: b;\n      }\n    }\n  }\n}\n"
    ".foo .bar {\n  a: b;\n}\n"
);
