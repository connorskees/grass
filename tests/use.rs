#![cfg(test)]

use std::io::Write;

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
    "@use \"sass:math\" as m#{a}th;", "Error: expected \";\"."
);
error!(
    use_as_quoted_string,
    "@use \"sass:math\" as \"math\";", "Error: Expected identifier."
);
error!(
    use_as_missing_s,
    "@use \"sass:math\" a math;", "Error: expected \";\"."
);
error!(
    unknown_module_get_variable,
    "a { color: foo.$bar; }", "Error: There is no module with the namespace \"foo\"."
);
error!(
    unknown_module_get_function,
    "a { color: foo.bar(); }", "Error: There is no module with the namespace \"foo\"."
);
error!(
    unknown_function,
    "@use \"sass:math\";\na { color: math.bar(); }", "Error: Undefined function."
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
test!(
    use_as_universal,
    "@use \"sass:math\" as *;
    a {
        color: cos(2);
    }",
    "a {\n  color: -0.4161468365;\n}\n"
);

#[test]
fn use_user_defined_same_directory() {
    let input = "@use \"use_user_defined_same_directory\";\na {\n color: use_user_defined_same_directory.$a;\n}";
    tempfile!(
        "use_user_defined_same_directory.scss",
        "$a: red; a { color: $a; }"
    );
    assert_eq!(
        "a {\n  color: red;\n}\n\na {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default()).expect(input)
    );
}

#[test]
fn use_user_defined_as() {
    let input = "@use \"use_user_defined_as\" as module;\na {\n color: module.$a;\n}";
    tempfile!("use_user_defined_as.scss", "$a: red; a { color: $a; }");
    assert_eq!(
        "a {\n  color: red;\n}\n\na {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default()).expect(input)
    );
}

#[test]
fn use_user_defined_function() {
    let input = "@use \"use_user_defined_function\" as module;\na {\n color: module.foo(red);\n}";
    tempfile!(
        "use_user_defined_function.scss",
        "@function foo($a) { @return $a; }"
    );
    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default()).expect(input)
    );
}
