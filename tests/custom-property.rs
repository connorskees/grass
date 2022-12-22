#[macro_use]
mod macros;

test!(
    interpolated_null_is_removed,
    "a {\n  --btn-font-family: #{null};\n}\n",
    "a {\n  --btn-font-family: ;\n}\n"
);
test!(
    no_space_after_colon,
    "a {\n  --btn-font-family:null;\n}\n",
    "a {\n  --btn-font-family:null;\n}\n"
);
test!(
    only_whitespace,
    "a {\n  --btn-font-family: ;\n}\n",
    "a {\n  --btn-font-family: ;\n}\n"
);
test!(
    silent_comment,
    "a {\n  --btn-font-family: // ;\n}\n",
    "a {\n  --btn-font-family: // ;\n}\n"
);
test!(
    interpolated_name_isnt_custom_property,
    "a {\n  #{--prop}:0.75;\n}\n",
    "a {\n  --prop: 0.75;\n}\n"
);
test!(
    interpolated_name_is_custom_property_if_dashes_not_part_of_interpolation,
    "a {\n  --#{prop}:0.75;\n}\n",
    "a {\n  --prop:0.75;\n}\n"
);
test!(
    prop_value_starts_with_u,
    "a {\n  --prop: underline;\n}\n",
    "a {\n  --prop: underline;\n}\n"
);
test!(
    prop_value_is_url,
    "a {\n  --prop: url();\n}\n",
    "a {\n  --prop: url();\n}\n"
);
test!(
    prop_value_starts_with_url,
    "a {\n  --prop: urlaa;\n}\n",
    "a {\n  --prop: urlaa;\n}\n"
);
test!(
    prop_value_is_url_without_parens,
    "a {\n  --prop: url;\n}\n",
    "a {\n  --prop: url;\n}\n"
);
test!(
    preserves_newlines_in_value,
    "a {\n    --without-semicolon: {\n        a: b\n    }\n}\n",
    "a {\n  --without-semicolon: {\n      a: b\n  } ;\n}\n"
);
error!(
    nothing_after_colon,
    "a {\n  --btn-font-family:;\n}\n", "Error: Expected token."
);
