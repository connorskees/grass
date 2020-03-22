#![cfg(test)]

#[macro_use]
mod macros;

test!(
    if_toplevel_true,
    "@if true {\n  a {\n    color: foo;\n}\n}\n",
    "a {\n  color: foo;\n}\n"
);
test!(
    if_inner_true,
    "a {\n  @if true {\n    color: foo;\n}\n}\n",
    "a {\n  color: foo;\n}\n"
);
test!(
    if_toplevel_false,
    "@if false {\n  a {\n    color: foo;\n}\n}\n",
    ""
);
test!(
    if_inner_false,
    "a {\n  @if false {\n    color: foo;\n}\n}\n",
    ""
);
test!(
    if_else_toplevel_true,
    "@if true {\n  a {\n    color: foo;\n}\n} @else {\n  b {\n    color: bar\n}\n}\n",
    "a {\n  color: foo;\n}\n"
);
test!(
    if_else_inner_true,
    "a {\n  @if true {\n    color: foo;\n} @else {\n    color: bar\n}\n}\n",
    "a {\n  color: foo;\n}\n"
);
test!(
    if_else_toplevel_false,
    "@if false {\n  a {\n    color: foo;\n}\n} @else {\n  a {\n    color: bar\n}\n}\n",
    "a {\n  color: bar;\n}\n"
);
test!(
    if_else_inner_false,
    "a {\n  @if false {\n    color: foo;\n} @else {\n    color: bar\n}\n}\n",
    "a {\n  color: bar;\n}\n"
);
error!(
    no_brace_after_else,
    "@if false {} @else -}", "Error: expected \"{\"."
);
