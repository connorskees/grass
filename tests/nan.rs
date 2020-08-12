#[macro_use]
mod macros;

error!(
    unitless_nan_str_slice_start_at,
    "a {\n  color: str-slice(\"\", (0/0));\n}\n", "Error: NaN is not an int."
);
error!(
    unitless_nan_str_slice_end_at,
    "a {\n  color: str-slice(\"\", 0, (0/0));\n}\n", "Error: NaN is not an int."
);
error!(
    unitless_nan_str_insert_index,
    "a {\n  color: str-insert(\"\", \"\", (0/0));\n}\n", "Error: $index: NaN is not an int."
);
test!(
    unitless_nan_percentage_number,
    "a {\n  color: percentage((0/0));\n}\n",
    "a {\n  color: NaN%;\n}\n"
);
test!(
    unitless_nan_abs_number,
    "a {\n  color: abs((0/0));\n}\n",
    "a {\n  color: NaN;\n}\n"
);
error!(
    unitless_nan_round_number,
    "a {\n  color: round((0/0));\n}\n", "Error: Infinity or NaN toInt"
);
error!(
    unitless_nan_ceil_number,
    "a {\n  color: ceil((0/0));\n}\n", "Error: Infinity or NaN toInt"
);
error!(
    unitless_nan_floor_number,
    "a {\n  color: floor((0/0));\n}\n", "Error: Infinity or NaN toInt"
);
error!(
    unitless_nan_random_limit,
    "a {\n  color: random((0/0));\n}\n", "Error: $limit: NaN is not an int."
);
error!(
    unitless_nan_nth_n,
    "a {\n  color: nth([a], (0/0));\n}\n", "Error: $n: NaN is not an int."
);
error!(
    unitless_nan_set_nth_n,
    "a {\n  color: set-nth([a], (0/0), b);\n}\n", "Error: $n: NaN is not an int."
);
test!(
    unitless_nan_min_first_arg,
    "$n: (0/0);\na {\n  color: min($n, 1px);\n}\n",
    "a {\n  color: NaN;\n}\n"
);
test!(
    unitless_nan_min_last_arg,
    "$n: (0/0);\na {\n  color: min(1px, $n);\n}\n",
    "a {\n  color: 1px;\n}\n"
);
test!(
    unitless_nan_min_middle_arg,
    "$n: (0/0);\na {\n  color: min(1px, $n, 0);\n}\n",
    "a {\n  color: 0;\n}\n"
);
test!(
    unitless_nan_max_first_arg,
    "$n: (0/0);\na {\n  color: max($n, 1px);\n}\n",
    "a {\n  color: NaN;\n}\n"
);
test!(
    unitless_nan_max_last_arg,
    "$n: (0/0);\na {\n  color: max(1px, $n);\n}\n",
    "a {\n  color: 1px;\n}\n"
);
test!(
    unitless_nan_max_middle_arg,
    "$n: (0/0);\na {\n  color: max(1px, $n, 0);\n}\n",
    "a {\n  color: 1px;\n}\n"
);
