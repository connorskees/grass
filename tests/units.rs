#![cfg(test)]

#[macro_use]
mod macros;

test!(unit_none, "a {\n  height: 1;\n}\n");
test!(unit_not_attached, "a {\n  height: 1 px;\n}\n");
test!(unit_px, "a {\n  height: 1px;\n}\n");
test!(unit_em, "a {\n  height: 1em;\n}\n");
test!(unit_rem, "a {\n  height: 1rem;\n}\n");
test!(unit_percent, "a {\n  height: 1%;\n}\n");
test!(
    unit_times_none,
    "a {\n  color: 3px * 2;\n}\n",
    "a {\n  color: 6px;\n}\n"
);
test!(
    none_times_unit,
    "a {\n  color: 2 * 3px;\n}\n",
    "a {\n  color: 6px;\n}\n"
);
test!(
    unit_fn_unit_times_none,
    "a {\n  color: unit(1px * 1);\n}\n",
    "a {\n  color: \"px\";\n}\n"
);
test!(
    unit_fn_none_times_unit,
    "a {\n  color: unit(1 * 1px);\n}\n",
    "a {\n  color: \"px\";\n}\n"
);
test!(
    unit_fn_unit_times_unit,
    "a {\n  color: unit(1px*1px);\n}\n",
    "a {\n  color: \"px*px\";\n}\n"
);
