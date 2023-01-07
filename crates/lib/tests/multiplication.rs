#[macro_use]
mod macros;

error!(
    map_lhs_mul,
    "a {color: (a: b) * 1;}", "Error: Undefined operation \"(a: b) * 1\"."
);
error!(
    map_rhs_mul,
    "a {color: 1 * (a: b);}", "Error: Undefined operation \"1 * (a: b)\"."
);
error!(
    function_lhs_mul,
    "a {color: get-function(lighten) * 1;}",
    "Error: Undefined operation \"get-function(\"lighten\") * 1\"."
);
error!(
    function_rhs_mul,
    "a {color: 1 * get-function(lighten);}",
    "Error: Undefined operation \"1 * get-function(\"lighten\")\"."
);
error!(
    null_mul_number,
    "a {color: null * 1;}", "Error: Undefined operation \"null * 1\"."
);
error!(
    calculation_mul_calculation,
    "a {color: calc(1rem + 1px) * calc(1rem + 1px);}",
    r#"Error: Undefined operation "calc(1rem + 1px) * calc(1rem + 1px)"."#
);
error!(
    num_mul_calculation,
    "a {color: 1 * calc(1rem + 1px);}", r#"Error: Undefined operation "1 * calc(1rem + 1px)"."#
);
test!(
    num_mul_nan,
    "a {\n  color: 1 * (0/0);\n}\n",
    "a {\n  color: NaN;\n}\n"
);
test!(
    nan_mul_num,
    "a {\n  color: (0/0) * 1;\n}\n",
    "a {\n  color: NaN;\n}\n"
);
test!(
    nan_mul_nan,
    "a {\n  color: (0/0) * (0/0);\n}\n",
    "a {\n  color: NaN;\n}\n"
);
