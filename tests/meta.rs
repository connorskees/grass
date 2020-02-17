#![cfg(test)]

#[macro_use]
mod macros;

test!(
    if_true,
    "a {\n  color: if(true, 1, 2)\n}\n",
    "a {\n  color: 1;\n}\n"
);
test!(
    if_equal,
    "a {\n  color: if(1 == 1, 1, 2)\n}\n",
    "a {\n  color: 1;\n}\n"
);
test!(
    if_not_equal,
    "a {\n  color: if(1 != 1, 1, 2)\n}\n",
    "a {\n  color: 2;\n}\n"
);
test!(
    if_named_args,
    "a {\n  color: if($condition: true, $if-true: 1, $if-false: 2)\n}\n",
    "a {\n  color: 1;\n}\n"
);
test!(
    if_false,
    "a {\n  color: if(false, 1, 2);\n}\n",
    "a {\n  color: 2;\n}\n"
);
test!(
    feature_exists_at_error_dbl_quoted,
    "a {\n  color: feature-exists(\"at-error\")\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    feature_exists_at_error_sgl_quoted,
    "a {\n  color: feature-exists('at-error')\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    feature_exists_at_error_no_quotes,
    "a {\n  color: feature-exists(at-error)\n}\n",
    "a {\n  color: true;\n}\n"
);
// Uncomment as more features are added
// test!(
//     feature_exists_global_variable_shadowing,
//     "a {\n  color: feature-exists(global-variable-shadowing)\n}\n",
//     "a {\n  color: true;\n}\n"
// );
// test!(
//     feature_exists_extend_selector_pseudoclass,
//     "a {\n  color: feature-exists(extend-selector-pseudoclass)\n}\n",
//     "a {\n  color: true;\n}\n"
// );
// test!(
//     feature_exists_units_level_3,
//     "a {\n  color: feature-exists(units-level-3)\n}\n",
//     "a {\n  color: true;\n}\n"
// );
// test!(
//     feature_exists_custom_property,
//     "a {\n  color: feature-exists(custom-property)\n}\n",
//     "a {\n  color: true;\n}\n"
// );
test!(
    feature_exists_nonsense,
    "a {\n  color: feature-exists(foo)\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    feature_exists_at_error_named_arg,
    "a {\n  color: feature-exists($feature: at-error)\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    unit_px,
    "a {\n  color: unit(1px)\n}\n",
    "a {\n  color: \"px\";\n}\n"
);
test!(
    unit_none,
    "a {\n  color: unit(1)\n}\n",
    "a {\n  color: \"\";\n}\n"
);
test!(
    unit_non_numeric,
    "a {\n  color: unit(red)\n}\n",
    "a {\n  color: \"\";\n}\n"
);
test!(
    unit_named_args,
    "a {\n  color: unit($number: 1px)\n}\n",
    "a {\n  color: \"px\";\n}\n"
);
test!(
    type_of_number,
    "a {\n  color: type-of(1)\n}\n",
    "a {\n  color: number;\n}\n"
);
test!(
    type_of_number_unit,
    "a {\n  color: type-of(1px)\n}\n",
    "a {\n  color: number;\n}\n"
);
test!(
    type_of_unquoted,
    "a {\n  color: type-of(foo)\n}\n",
    "a {\n  color: string;\n}\n"
);
test!(
    type_of_sgl_unquoted,
    "a {\n  color: type-of('red')\n}\n",
    "a {\n  color: string;\n}\n"
);
test!(
    type_of_dbl_unquoted,
    "a {\n  color: type-of(\"red\")\n}\n",
    "a {\n  color: string;\n}\n"
);
test!(
    type_of_3_hex_color,
    "a {\n  color: type-of(#fff)\n}\n",
    "a {\n  color: color;\n}\n"
);
test!(
    type_of_6_hex_color,
    "a {\n  color: type-of(#ffffff)\n}\n",
    "a {\n  color: color;\n}\n"
);
test!(
    type_of_named_color,
    "a {\n  color: type-of(red)\n}\n",
    "a {\n  color: color;\n}\n"
);
// test!(
//     type_of_spaced_list,
//     "a {\n  color: type-of(1 2 3)\n}\n",
//     "a {\n  color: list;\n}\n"
// );
test!(
    type_of_true,
    "a {\n  color: type-of(true)\n}\n",
    "a {\n  color: bool;\n}\n"
);
test!(
    type_of_false,
    "a {\n  color: type-of(false)\n}\n",
    "a {\n  color: bool;\n}\n"
);
test!(
    type_of_null,
    "a {\n  color: type-of(null)\n}\n",
    "a {\n  color: null;\n}\n"
);
test!(
    type_of_ident_plus_ident,
    "a {\n  color: type-of(hi + bye)\n}\n",
    "a {\n  color: string;\n}\n"
);
test!(
    unitless_px,
    "a {\n  color: unitless(1px)\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    unitless_num,
    "a {\n  color: unitless(1)\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    unitless_string,
    "a {\n  color: unitless(foo)\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    inspect_unquoted_string,
    "a {\n  color: inspect(foo)\n}\n",
    "a {\n  color: foo;\n}\n"
);
test!(
    inspect_dbl_quoted_string,
    "a {\n  color: inspect(\"foo\")\n}\n",
    "a {\n  color: \"foo\";\n}\n"
);
test!(
    inspect_sgl_quoted_string,
    "a {\n  color: inspect('foo')\n}\n",
    "a {\n  color: 'foo';\n}\n"
);
test!(
    inspect_unitless_number,
    "a {\n  color: inspect(1)\n}\n",
    "a {\n  color: 1;\n}\n"
);
test!(
    inspect_px_number,
    "a {\n  color: inspect(1px)\n}\n",
    "a {\n  color: 1px;\n}\n"
);
test!(
    inspect_color_3_hex,
    "a {\n  color: inspect(#fff)\n}\n",
    "a {\n  color: #fff;\n}\n"
);
test!(
    inspect_color_6_hex,
    "a {\n  color: inspect(#ffffff)\n}\n",
    "a {\n  color: #ffffff;\n}\n"
);
test!(
    inspect_color_name,
    "a {\n  color: inspect(red)\n}\n",
    "a {\n  color: red;\n}\n"
);
test!(
    inspect_true,
    "a {\n  color: inspect(true)\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    inspect_false,
    "a {\n  color: inspect(false)\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    inspect_null,
    "a {\n  color: inspect(null)\n}\n",
    "a {\n  color: null;\n}\n"
);
test!(
    variable_does_exist,
    "$a: red; a {\n  color: variable-exists(a)\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    variable_does_not_exist,
    "a {\n  color: variable-exists(a)\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    variable_exists_named,
    "$a: red; a {\n  color: variable-exists($name: a)\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    mixin_does_exist,
    "@mixin a{} a {\n  color: mixin-exists(a)\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    mixin_does_not_exist,
    "a {\n  color: mixin-exists(a)\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    mixin_exists_named,
    "@mixin a{} a {\n  color: mixin-exists($name: a)\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    function_does_exist,
    "@function a(){} a {\n  color: function-exists(a)\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    function_does_not_exist,
    "a {\n  color: function-exists(a)\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    function_exists_named,
    "@function a(){} a {\n  color: function-exists($name: a)\n}\n",
    "a {\n  color: true;\n}\n"
);
// test!(
//     inspect_empty_list,
//     "a {\n  color: inspect(())\n}\n",
//     "a {\n  color: ();\n}\n"
// );
// test!(
//     inspect_spaced_list,
//     "a {\n  color: inspect(1 2 3)\n}\n",
//     "a {\n  color: 1 2 3;\n}\n"
// );
// test!(
//     inspect_comma_list,
//     "a {\n  color: inspect(1, 2, 3)\n}\n",
//     "a {\n  color: 1, 2, 3;\n}\n"
// );
