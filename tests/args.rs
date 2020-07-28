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
test!(
    self_referential_default_arg_value,
    "@function foo($a, $b: $a) {\n    @return $b;\n}\n\na {\n    color: foo(2);\n}\n",
    "a {\n  color: 2;\n}\n"
);
test!(
    arg_errors_are_lazy_for_if,
    "a {\n  color: if(false, unit(foo), red);\n}\n",
    "a {\n  color: red;\n}\n"
);
error!(
    #[ignore = "expects incorrect char, '{'"]
    nothing_after_open,
    "a { color:rgb(; }", "Error: expected \")\"."
);
error!(
    nothing_after_open_paren_in_fn_args,
    "@function foo(", "Error: expected \")\"."
);
error!(
    args_are_evaluated_eagerly,
    "@function foo($a) {\n    @return foo;\n}\n\na {\n    color: foo(unit(bar));\n}\n",
    "Error: $number: bar is not a number."
);
test!(
    variable_is_only_hyphens,
    "$--: red;

    a {
        color: foo($--);
    }",
    "a {\n  color: foo(red);\n}\n"
);
test!(
    no_space_after_colon_in_keyword_arg,
    "@function foo($a) {
        @return $a;
    }

    $b: red;

    a {
        color: foo($a:$b);
    }",
    "a {\n  color: red;\n}\n"
);
test!(
    comment_after_comma_in_func_args,
    "@mixin a(
      $foo,//foo
    ) {
        color: $foo;
    }

    a {
        @include a(red);
    }",
    "a {\n  color: red;\n}\n"
);
