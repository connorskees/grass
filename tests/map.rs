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
test!(
    map_keys_are_comma_separated,
    "a {\n  color: map-keys((a: b, c: d));\n}\n",
    "a {\n  color: a, c;\n}\n"
);
test!(
    map_keys_empty,
    "a {\n  color: inspect(map-keys(()));\n}\n",
    "a {\n  color: ();\n}\n"
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
test!(
    map_values_empty,
    "a {\n  color: inspect(map-values(()));\n}\n",
    "a {\n  color: ();\n}\n"
);
test!(
    map_values_are_comma_separated,
    "a {\n  color: map-values((a: b, c: d));\n}\n",
    "a {\n  color: b, d;\n}\n"
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
test!(
    map_merge_both_empty,
    "a {\n  color: inspect(map-merge((), ()));\n}\n",
    "a {\n  color: ();\n}\n"
);
test!(
    map_merge_same_keys,
    "a {\n  color: inspect(map-merge((c: d, e: f), (c: 1, e: 2)));\n}\n",
    "a {\n  color: (c: 1, e: 2);\n}\n"
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
    "a {\n  color: map-get((\"a\": b), \"a\");\n}\n",
    "a {\n  color: b;\n}\n"
);
test!(
    map_key_quoting_ignored,
    "a {\n  color: map-get((\"a\": b), 'a');\n}\n",
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
error!(
    map_get_one_arg,
    "a {\n  color: map-get(1);\n}\n", "Error: Missing argument $key."
);
error!(
    map_has_key_one_arg,
    "a {\n  color: map-has-key(1);\n}\n", "Error: Missing argument $key."
);
test!(
    map_remove_one,
    "a {\n  color: inspect(map-remove((\"foo\": 1, \"bar\": 2), \"bar\"));\n}\n",
    "a {\n  color: (\"foo\": 1);\n}\n"
);
test!(
    map_remove_two,
    "a {\n  color: inspect(map-remove((\"foo\": 1, \"bar\": 2, \"baz\": 3), \"bar\", \"baz\"));\n}\n",
    "a {\n  color: (\"foo\": 1);\n}\n"
);
error!(
    duplicate_key_in_declaration,
    "a {\n  $a: (foo: a, foo: b);;\n}\n", "Error: Duplicate key."
);
