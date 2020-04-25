#![cfg(test)]

#[macro_use]
mod macros;

error!(
    variable_after_varargs,
    "@function foo($a..., $b) {\n  @return $a;\n}\n", "Error: expected \")\"."
);
error!(
    varargs_one_period,
    "@function foo($a.) {\n  @return $a;\n}\n", "Error: expected \".\"."
);
error!(
    varargs_two_periods,
    "@function foo($a..) {\n  @return $a;\n}\n", "Error: expected \".\"."
);
test!(
    mixin_varargs_are_comma_separated,
    "@mixin foo($a...) {\n  color: $a;\n}\n\na {\n  @include foo(1, 2, 3, 4, 5);\n}\n",
    "a {\n  color: 1, 2, 3, 4, 5;\n}\n"
);
test!(
    function_varargs_are_comma_separated,
    "@function foo($a...) {\n  @return $a;\n}\n\na {\n  color: foo(1, 2, 3, 4, 5);\n}\n",
    "a {\n  color: 1, 2, 3, 4, 5;\n}\n"
);
test!(
    default_args_are_lazily_evaluated,
    "$da: a;\n\n@mixin foo($x: $da) {\n  color: $x;\n}\n$da: b;\n\na {\n  @include foo();\n}\n",
    "a {\n  color: b;\n}\n"
);
test!(
    variable_being_subtracted,
    "$index: 1;\n\n@function foo($a) {\n  @return $a;\n}\n\na {\n  color: foo($index - 1);\n}\n",
    "a {\n  color: 0;\n}\n"
);
test!(
    parens_in_default_arg_value,
    "@function foo($arg1: bar()) {\n    @return true;\n}\n\na {\n    color: foo();\n}\n",
    "a {\n  color: true;\n}\n"
);
