#[macro_use]
mod macros;

test!(
    color_equals_color,
    "a {\n  color: red == red;\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    color_does_not_equal_color,
    "a {\n  color: red != red;\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    unquoted_ident_eq_unquoted_ident,
    "a {\n  color: foo == foo;\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    dblquoted_ident_eq_unquoted_ident,
    "a {\n  color: \"foo\" == foo;\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    dblquoted_ident_eq_sglquoted_ident,
    "a {\n  color: \"foo\" == 'foo';\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    dblquoted_eq_number,
    "a {\n  color: \"foo\" == 1;\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    uncomparable_units,
    "a {\n  color: 1rem==1px;\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    first_unit_none,
    "a {\n  color: 1==1px;\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    second_unit_none,
    "a {\n  color: 1rem==1;\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    different_quoting_inside_list_eq,
    "a {\n  color: (\"foo\",) == (foo,);\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    different_quoting_inside_list_ne,
    "a {\n  color: (\"foo\",) != (foo,);\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    map_keys_equivalent,
    "a {\n  color: (0mm: a)==(0cm: a);\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    true_true_eq,
    "a {\n  color: true == true;\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    false_false_eq,
    "a {\n  color: false == false;\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    true_false_eq,
    "a {\n  color: true == false;\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    false_true_eq,
    "a {\n  color: false == true;\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    true_true_ne,
    "a {\n  color: true != true;\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    false_false_ne,
    "a {\n  color: false != false;\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    true_false_ne,
    "a {\n  color: true != false;\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    false_true_ne,
    "a {\n  color: true != false;\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    important_important_eq,
    "a {\n  color: !important == !important;\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    important_important_ne,
    "a {\n  color: !important != !important;\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    map_color_eq,
    "a {\n  color: (a: b) == red;\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    map_color_ne,
    "a {\n  color: (a: b) != red;\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    bracketed_list_color_eq,
    "a {\n  color: [] == red;\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    bracketed_list_color_ne,
    "a {\n  color: [] != red;\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    function_ref_color_eq,
    "a {\n  color: get-function(\"red\") == red;\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    function_ref_color_ne,
    "a {\n  color: get-function(\"red\") != red;\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    nan_nan_eq,
    "a {\n  color: (0/0) == (0/0);\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    nan_nan_ne,
    "a {\n  color: (0/0) != (0/0);\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    string_bool_ne,
    "a {\n  color: hi != false;\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    lists_differ_only_in_separator_eq,
    "a {\n  color: (1 2 3) == (1, 2, 3);\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    lists_differ_only_in_separator_ne,
    "a {\n  color: (1 2 3) != (1, 2, 3);\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    maps_differ_in_length_eq,
    "a {\n  color: (a: b) == (a: b, c: d);\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    maps_differ_in_length_ne,
    "a {\n  color: (a: b) != (a: b, c: d);\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    eq_does_unit_conversion,
    "a {\n  color: 1in==2.54cm;\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    ne_does_unit_conversion,
    "a {\n  color: 1in!=2.54cm;\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    arglist_unquoted_string_eq,
    "@function foo($a...) {
      @return $a == bar;
    }
    
    a {
      color: foo(1, 2, 3);
    }",
    "a {\n  color: false;\n}\n"
);
test!(
    arglist_equals_self,
    "@function foo($a...) {
      @return $a == $a;
    }
    
    a {
      color: foo(1, 2, 3);
    }",
    "a {\n  color: true;\n}\n"
);
test!(
    arglist_equals_self_when_splat_through_other_function,
    "@function bar($a, $b...) {
      @return $a == $b;
    }
    
    @function foo($a...) {
      @return bar($a, $a...);
    }
    
    a {
      color: foo(1, 2, 3);
    }",
    "a {\n  color: true;\n}\n"
);
test!(
    arglist_equals_does_not_equal_self_when_not_splat,
    "@function bar($a, $b...) {
      @return $a == $b;
    }
    
    @function foo($a...) {
      @return bar($a, $a);
    }
    
    a {
      color: foo(1, 2, 3);
    }",
    "a {\n  color: false;\n}\n"
);
test!(
    arglist_equals_comma_separated_list,
    "@function foo($a...) {
      @return $a == (1, 2, 3);
    }
    
    a {
      color: foo(1, 2, 3);
    }",
    "a {\n  color: true;\n}\n"
);
test!(
    arglist_does_not_equal_space_separated_list,
    "@function foo($a...) {
      @return $a == (1 2 3);
    }
    
    a {
      color: foo(1, 2, 3);
    }",
    "a {\n  color: false;\n}\n"
);
test!(
    number_equality_is_fuzzy,
    "a {
      color: .9999999999999999999999999999999==.99999999999999999999999999999998;
    }",
    "a {\n  color: true;\n}\n"
);
test!(
    calculation_equality_converts_units,
    "a {
      color: calc(1in + 1rem) == calc(2.54cm + 1rem);
    }",
    "a {\n  color: true;\n}\n"
);
