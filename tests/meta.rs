#![cfg(test)]

#[macro_use]
mod macros;

test!(
    if_true,
    "a {\n  color: if(true, 1, 2)\n}\n",
    "a {\n  color: 1;\n}\n"
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
