#[macro_use]
mod macros;

test!(
    basic_keywords,
    "@function foo($args...) {
        @return inspect(keywords($args));
    }
    a {
        color: foo($a: 1, $b: 2, $c: 3);
    }",
    "a {\n  color: (a: 1, b: 2, c: 3);\n}\n"
);
test!(
    access_keywords_in_variable,
    "@function foo($args...) {
        $a: keywords($args);
        @return 2;
    }
    a {
        color: foo($a: 1, $b: 2, $c: 3);
    }",
    "a {\n  color: 2;\n}\n"
);
error!(
    keywords_not_accessed,
    "@function foo($args...) {
        @return 2;
    }
    a {
        color: foo($a: 1, $b: 2, $c: 3);
    }",
    "Error: No arguments named $a, $b or $c."
);
test!(
    keywords_in_meta_module,
    r#"
    @use "sass:meta";
    @function foo($args...) {
        @return inspect(meta.keywords($args));
    }

    a {
        color: foo($a: 1, $b: 2, $c: 3);
    }"#,
    "a {\n  color: (a: 1, b: 2, c: 3);\n}\n"
);
