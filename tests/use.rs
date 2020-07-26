#![cfg(test)]

#[macro_use]
mod macros;

error!(
    after_style,
    "a {}
    @use \"foo\";
    ",
    "Error: @use rules must be written before any other rules."
);
error!(
    interpolation_in_as_identifier,
    "@use \"sass:math\" as m#{a}th;",
    "Error: expected \";\"."
);
error!(
    use_as_quoted_string,
    "@use \"sass:math\" as \"math\";",
    "Error: Expected identifier."
);
error!(
    use_as_missing_s,
    "@use \"sass:math\" a math;",
    "Error: expected \";\"."
);
error!(
    unknown_module_get_variable,
    "a { color: foo.$bar; }",
    "Error: There is no module with the namespace \"foo\"."
);
error!(
    unknown_module_get_function,
    "a { color: foo.bar(); }",
    "Error: There is no module with the namespace \"foo\"."
);
error!(
    unknown_function,
    "@use \"sass:math\";\na { color: math.bar(); }",
    "Error: Undefined function."
);
test!(
    use_as,
    "@use \"sass:math\" as foo;
    a {
        color: foo.clamp(0, 1, 2);
    }",
    "a {\n  color: 1;\n}\n"
);
test!(
    use_as_uppercase,
    "@use \"sass:math\" AS foo;
    a {
        color: foo.clamp(0, 1, 2);
    }",
    "a {\n  color: 1;\n}\n"
);
