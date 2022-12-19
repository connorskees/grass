#[macro_use]
mod macros;

error!(
    clamp_empty_args,
    "a {\n  color: clamp();\n}\n", "Error: Expected number, variable, function, or calculation."
);
error!(
    clamp_parens_in_args,
    "a {\n  color: clamp((()));\n}\n",
    "Error: Expected number, variable, function, or calculation."
);
error!(
    clamp_single_arg,
    "a {\n  color: clamp(1);\n}\n", "Error: 3 arguments required, but only 1 was passed."
);
test!(
    clamp_all_unitless,
    "a {\n  color: clamp(1, 2, 3);\n}\n",
    "a {\n  color: 2;\n}\n"
);
test!(
    clamp_all_same_unit,
    "a {\n  color: clamp(1px, 2px, 3px);\n}\n",
    "a {\n  color: 2px;\n}\n"
);
test!(
    clamp_last_non_comparable,
    "a {\n  color: clamp(1px, 2px, 3vh);\n}\n",
    "a {\n  color: clamp(1px, 2px, 3vh);\n}\n"
);
