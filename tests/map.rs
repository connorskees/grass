#![cfg(test)]

#[macro_use]
mod macros;
test!(
    map_get_key_exists,
    "a {\n  color: map-get((a: b), a);\n}\n",
    "a {\n  color: b;\n}\n"
);
test!(
    map_get_key_does_not_exist,
    "a {\n  color: map-get((a: b), foo);\n}\n",
    ""
);
error!(
    map_get_non_map,
    "a {\n  color: map-get(foo, foo);\n}\n", "Error: $map: foo is not a map."
);
test!(
    map_has_key_true,
    "a {\n  color: map-has-key((a: b), a);\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    map_has_key_false,
    "a {\n  color: map-has-key((a: b), foo);\n}\n",
    "a {\n  color: false;\n}\n"
);
error!(
    map_has_key_non_map,
    "a {\n  color: map-has-key(foo, foo);\n}\n", "Error: $map: foo is not a map."
);
test!(
    map_keys_one,
    "a {\n  color: map-keys((a: b));\n}\n",
    "a {\n  color: a;\n}\n"
);
error!(
    map_keys_non_map,
    "a {\n  color: map-keys(foo);\n}\n", "Error: $map: foo is not a map."
);
test!(
    map_values_one,
    "a {\n  color: map-values((a: b));\n}\n",
    "a {\n  color: b;\n}\n"
);
error!(
    map_values_non_map,
    "a {\n  color: map-values(foo);\n}\n", "Error: $map: foo is not a map."
);
test!(
    map_merge_one,
    "a {\n  color: inspect(map-merge((a: b), (c: d)));\n}\n",
    "a {\n  color: (a: b, c: d);\n}\n"
);
error!(
    map_merge_map1_non_map,
    "a {\n  color: map-merge(foo, (a: b));\n}\n", "Error: $map1: foo is not a map."
);
error!(
    map_merge_map2_non_map,
    "a {\n  color: map-merge((a: b), foo);\n}\n", "Error: $map2: foo is not a map."
);
test!(
    map_dbl_quoted_key,
    "a {\n  color: map-get((\"a\": b), \"a\"));\n}\n",
    "a {\n  color: b;\n}\n"
);
test!(
    map_key_quoting_ignored,
    "a {\n  color: map-get((\"a\": b), 'a'));\n}\n",
    "a {\n  color: b;\n}\n"
);
test!(
    map_arbitrary_number_of_entries,
    "a {\n  color: inspect((a: b, c: d, e: f, g: h, i: j, h: k, l: m, n: o));\n}\n",
    "a {\n  color: (a: b, c: d, e: f, g: h, i: j, h: k, l: m, n: o);\n}\n"
);
test!(
    map_length,
    "a {\n  color: length((a: b, c: d, e: f));\n}\n",
    "a {\n  color: 3;\n}\n"
);
