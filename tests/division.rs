#![cfg(test)]

#[macro_use]
mod macros;

test!(
    none_div_none,
    "a {\n  color: (35 / 7);\n}\n",
    "a {\n  color: 5;\n}\n"
);
test!(
    unit_div_none,
    "a {\n  color: (35% / 7);\n}\n",
    "a {\n  color: 5%;\n}\n"
);
test!(
    unit_div_unit,
    "a {\n  color: (35% / 7%);\n}\n",
    "a {\n  color: 5;\n}\n"
);
test!(
    unit_conversion,
    "a {\n  color: (35px / 7in);\n}\n",
    "a {\n  color: 0.0520833333;\n}\n"
);
// error!(
//     none_div_unit,
//     "a {\n  color: (35 / 7%);\n}\n",
//     "Error: 5%^-1 isn't a valid CSS value."
// );
