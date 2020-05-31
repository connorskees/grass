#![cfg(test)]

#[macro_use]
mod macros;

test!(
    more_specific_class_compound,
    "a {\n  color: is-superselector(\".foo\", \".foo.bar\");\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    less_specific_class_compound,
    "a {\n  color: is-superselector(\".foo.bar\", \".foo\");\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    more_specific_class_complex,
    "a {\n  color: is-superselector(\".bar\", \".foo .bar\");\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    less_specific_class_complex,
    "a {\n  color: is-superselector(\".foo .bar\", \".bar\");\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    two_in_sub,
    "a {\n  color: is-superselector(\"c\", \"c, d\");\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    two_in_super,
    "a {\n  color: is-superselector(\"c, d\", \"c\");\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    two_in_both_equal,
    "a {\n  color: is-superselector(\"c, d\", \"c, d\");\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    two_in_both_subset,
    "a {\n  color: is-superselector(\"c, d\", \"c.e, d.f\");\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    two_in_both_superset,
    "a {\n  color: is-superselector(\"c.e, d.f\", \"c, d\");\n}\n",
    "a {\n  color: false;\n}\n"
);
test!(
    two_in_sub_satisfied_by_one_super,
    "a {\n  color: is-superselector(\".c\", \"d.c, e.c\");\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    three_in_super_match_one,
    "a {\n  color: is-superselector(\"c, d, e\", \"d\");\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    three_in_super_match_two,
    "a {\n  color: is-superselector(\"c, d, e\", \"e, c\");\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    three_in_super_match_three,
    "a {\n  color: is-superselector(\"c, d, e\", \"d, c, e\");\n}\n",
    "a {\n  color: true;\n}\n"
);
test!(
    three_in_super_miss_one,
    "a {\n  color: is-superselector(\"c, d, e\", \"c, f\");\n}\n",
    "a {\n  color: false;\n}\n"
);
