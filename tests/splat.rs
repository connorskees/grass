#![cfg(test)]

#[macro_use]
mod macros;

test!(
    splat_list_two_elements,
    "@function foo($a, $b) {
        @return $a+$b;
    }
    a {
        color: foo([1, 2]...);
    }",
    "a {\n  color: 3;\n}\n"
);
test!(
    splat_map_single_key,
    "@function foo($a) {
        @return $a;
    }
    a {
        color: foo((a: b)...);
    }",
    "a {\n  color: b;\n}\n"
);
test!(
    splat_single_value,
    "@function foo($a) {
        @return $a;
    }
    a {
        color: foo(1...);
    }",
    "a {\n  color: 1;\n}\n"
);
error!(
    splat_missing_last_period,
    "@function foo($a) {
        @return $a;
    }
    a {
        color: foo(1..);
    }",
    "Error: expected \".\"."
);
error!(
    splat_with_named_arg,
    "@function foo($a) {
        @return $a;
    }
    a {
        color: foo($a: 1...);
    }",
    "Error: expected \")\"."
);
