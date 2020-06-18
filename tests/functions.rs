#![cfg(test)]

#[macro_use]
mod macros;

test!(
    return_num,
    "@function a() {\n  @return 1;\n}\n\nb {\ncolor: a();\n}\n",
    "b {\n  color: 1;\n}\n"
);
test!(
    return_spaced_list,
    "@function a() {\n  @return a b;\n}\n\nb {\ncolor: a();\n}\n",
    "b {\n  color: a b;\n}\n"
);
test!(
    single_arg,
    "@function a($c) {\n  @return $c;\n}\n\nb {\ncolor: a(1);\n}\n",
    "b {\n  color: 1;\n}\n"
);
test!(
    return_variable,
    "@function a($a) {\n  @return $a;\n}\n\nb {\ncolor: a(1);\n}\n",
    "b {\n  color: 1;\n}\n"
);
test!(
    function_call_as_arg,
    "@function a($a) {\n  @return $a;\n}\n\nb {\ncolor: a(a(2));\n}\n",
    "b {\n  color: 2;\n}\n"
);
test!(
    function_named_arg_value_variable,
    "$x: red;\n\n@function a($a) {\n  @return $a;\n}\n\nb {\ncolor: a($a: $x);\n}\n",
    "b {\n  color: red;\n}\n"
);
test!(
    function_trailing_comma,
    "@function a($a) {\n  @return $a;\n}\n\nb {\ncolor: a(red,);\n}\n",
    "b {\n  color: red;\n}\n"
);
test!(
    return_no_semicolon,
    "@function a() {\n  @return 1\n}\n\nb {\ncolor: a();\n}\n",
    "b {\n  color: 1;\n}\n"
);
test!(
    two_returns,
    "@function a() {\n  @return 1; @return 2;\n}\n\nb {\ncolor: a();\n}\n",
    "b {\n  color: 1;\n}\n"
);
test!(
    value_after_variable,
    "$x: 0;\na {\n  color: if($x != 0, a, b);\n}\n",
    "a {\n  color: b;\n}\n"
);
test!(
    function_decl_in_ruleset,
    "a {\n  @function foo() {\n    @return 3;\n  }\n  color: foo();\n}\n",
    "a {\n  color: 3;\n}\n"
);
test!(
    function_decl_in_foreign_ruleset,
    "a {\n  @function foo() {\n    @return 3;\n  }\n}\nb {\n  color: foo();\n}\n",
    "b {\n  color: foo();\n}\n"
);
test!(
    global_function_in_scope,
    "@function f() {\n  @return g();\n}\n@function g() {\n  @return false;\n}\na {\n  color: f();\n  color: g();\n}\n",
    "a {\n  color: false;\n  color: false;\n}\n"
);
test!(
    square_bracket_comma_separated,
    "@function foo($a) {\n  @return $a;\n}\n\na {\n  color: foo([a, b]);\n}\n",
    "a {\n  color: [a, b];\n}\n"
);
test!(
    eats_quoted_content,
    "a {\n  color: unquote(\"a, b, c, d\");\n}\n",
    "a {\n  color: a, b, c, d;\n}\n"
);
test!(
    variable_declaration,
    "@function str-replace($string, $search, $replace: \"\") {\n  $index: $string;\n  @return $index;\n}\n\na {\n  color: str-replace(\"a#b#c\", \"#\", \":\");\n}",
    "a {\n  color: \"a#b#c\";\n}\n"
);
error!(
    missing_name,
    "@function() {}", "Error: Expected identifier."
);
error!(
    args_do_not_start_with_var,
    "@function foo(FOO) {}", "Error: expected \")\"."
);
error!(
    double_comma_args,
    "@function foo($a,$b,,) {}", "Error: expected \")\"."
);
error!(
    body_missing_closing_curly_brace,
    "@function foo() {", "Error: expected \"}\"."
);
