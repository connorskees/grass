use std::io::Write;

use macros::TestFs;

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
error!(
    module_function_missing_open_parens,
    "@use \"sass:math\";\na { color: math.floor; }", "Error: expected \"(\"."
);
error!(
    module_not_quoted_string,
    "@use a", "Error: Expected string."
);
error!(
    use_file_name_is_invalid_identifier,
    r#"@use "a b";"#, r#"Error: The default namespace "a b" is not a valid Sass identifier."#
);
error!(
    use_empty_string,
    r#"@use "";"#, r#"Error: The default namespace "" is not a valid Sass identifier."#
);
error!(
    configure_builtin_module,
    r#"@use "sass:math" with ($e: 5);"#, r#"Error: Built-in modules can't be configured."#
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
test!(
    use_single_quotes,
    "@use 'sass:math';
    a {
        color: math.cos(2);
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
fn private_variable_begins_with_underscore() {
    let mut fs = TestFs::new();

    fs.add_file(
        "_a.scss",
        r#"
        $_foo: red;
        a { color: $_foo; }
    "#,
    );

    let input = r#"
        @use "a" as module;
        b {
            color: module.$_foo;
        }
    "#;

    assert_err!(
        input,
        "Error: Private members can't be accessed from outside their modules.",
        &grass::Options::default().fs(&fs)
    );
}

#[test]
fn private_variable_begins_with_hyphen() {
    let mut fs = TestFs::new();

    fs.add_file(
        "_a.scss",
        r#"
        $-foo: red;
        a { color: $-foo; }
    "#,
    );

    let input = r#"
        @use "a" as module;
        b {
            color: module.$-foo
        }
    "#;

    assert_err!(
        input,
        "Error: Private members can't be accessed from outside their modules.",
        &grass::Options::default().fs(&fs)
    );
}

#[test]
fn private_function() {
    let mut fs = TestFs::new();

    fs.add_file(
        "_a.scss",
        r#"
        @function _foo($a) { @return $a; }
        a { color: _foo(red); }
    "#,
    );

    let input = r#"
        @use "a" as module;
        b {
            color: module._foo(green)
        }
    "#;

    assert_err!(
        input,
        "Error: Private members can't be accessed from outside their modules.",
        &grass::Options::default().fs(&fs)
    );
}

#[test]
fn global_variable_exists_private() {
    let mut fs = TestFs::new();

    fs.add_file(
        "_a.scss",
        r#"
        $foo: red;
        $_foo: red;
    "#,
    );

    let input = r#"
        @use "a" as module;
        a {
            color: global-variable-exists($name: foo, $module: module);
            color: global-variable-exists($name: _foo, $module: module);
        }
    "#;

    assert_eq!(
        "a {\n  color: true;\n  color: false;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
fn use_user_defined_as() {
    let mut fs = TestFs::new();

    fs.add_file(
        "_a.scss",
        r#"
        $a: red; a { color: $a; }
    "#,
    );

    let input = r#"
        @use "a" as module;
        a {
            color: module.$a;
        }
    "#;

    assert_eq!(
        "a {\n  color: red;\n}\n\na {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
fn use_user_defined_function() {
    let mut fs = TestFs::new();

    fs.add_file(
        "_a.scss",
        r#"
        @function foo($a) { @return $a; }
    "#,
    );

    let input = r#"
        @use "a" as module;
        a {
            color: module.foo(red);
        }
    "#;

    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
fn use_idempotent_no_alias() {
    let mut fs = TestFs::new();

    fs.add_file("_a.scss", r#""#);

    let input = r#"
        @use "a";
        @use "a";
    "#;

    assert_err!(
        input,
        "Error: There's already a module with namespace \"a\".",
        grass::Options::default().fs(&fs)
    );
}

#[test]
fn use_idempotent_with_alias() {
    let mut fs = TestFs::new();

    fs.add_file("_a.scss", r#""#);
    fs.add_file("_b.scss", r#""#);

    let input = r#"
        @use "a" as foo;
        @use "b" as foo;
    "#;

    assert_err!(
        input,
        "Error: There's already a module with namespace \"foo\".",
        grass::Options::default().fs(&fs)
    );
}

#[test]
fn use_idempotent_builtin() {
    let input = "@use \"sass:math\";\n@use \"sass:math\";\n";

    assert_err!(
        "Error: There's already a module with namespace \"math\".",
        input
    );
}

#[test]
fn use_with_simple() {
    let input = "@use \"use_with_simple\" with ($a: red);\na {\n color: use_with_simple.$a;\n}";
    tempfile!("use_with_simple.scss", "$a: green !default;");
    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default()).expect(input)
    );
}

#[test]
fn use_as_with() {
    let input = "@use \"use_as_with\" as module with ($a: red);\na {\n color: module.$a;\n}";
    tempfile!("use_as_with.scss", "$a: green !default;");
    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default()).expect(input)
    );
}

#[test]
fn use_whitespace_and_comments() {
    let input = "@use  /**/  \"use_whitespace_and_comments\"  /**/  as  /**/  foo  /**/  with  /**/  (  /**/  $a  /**/  :  /**/  red  /**/  );";
    tempfile!(
        "use_whitespace_and_comments.scss",
        "$a: green !default; a { color: $a }"
    );
    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default()).expect(input)
    );
}

#[test]
fn use_loud_comment_after_close_paren_with() {
    let input = r#"@use "b" as foo with ($a : red)  /**/  ;"#;
    tempfile!(
        "use_loud_comment_after_close_paren_with.scss",
        "$a: green !default; a { color: $a }"
    );
    assert_err!(r#"Error: expected ";"."#, input);
}

#[test]
fn use_with_builtin_module() {
    let input = "@use \"sass:math\" with ($e: 2.7);";

    assert_err!("Error: Built-in modules can't be configured.", input);
}

#[test]
fn use_with_variable_never_used() {
    let input = "@use \"use_with_variable_never_used\" with ($a: red);";
    tempfile!("use_with_variable_never_used.scss", "");

    assert_err!(
        "Error: This variable was not declared with !default in the @used module.",
        input
    );
}

#[test]
fn use_with_same_variable_multiple_times() {
    let input = "@use \"use_with_same_variable_multiple_times\" as foo with ($a: b, $a: c);";
    tempfile!("use_with_same_variable_multiple_times.scss", "");

    assert_err!(
        "Error: The same variable may only be configured once.",
        input
    );
}

#[test]
fn use_variable_redeclaration_var_dne() {
    let input = "@use \"use_variable_redeclaration_var_dne\" as mod;\nmod.$a: red;";
    tempfile!("use_variable_redeclaration_var_dne.scss", "");

    assert_err!("Error: Undefined variable.", input);
}

#[test]
fn use_variable_redeclaration_global() {
    let input = "@use \"use_variable_redeclaration_global\" as mod;\nmod.$a: red !global;";
    tempfile!("use_variable_redeclaration_global.scss", "$a: green;");

    assert_err!(
        "Error: !global isn't allowed for variables in other modules.",
        input
    );
}

#[test]
fn use_variable_redeclaration_simple() {
    let input =
        "@use \"use_variable_redeclaration_simple\" as mod;\nmod.$a: red; a { color: mod.$a; }";
    tempfile!("use_variable_redeclaration_simple.scss", "$a: green;");

    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default()).expect(input)
    );
}

#[test]
fn use_variable_redeclaration_default() {
    let input = "@use \"use_variable_redeclaration_default\" as mod;\nmod.$a: 1 % red !default; a { color: mod.$a; }";
    tempfile!("use_variable_redeclaration_default.scss", "$a: green;");

    assert_eq!(
        "a {\n  color: green;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default()).expect(input)
    );
}

#[test]
fn use_variable_redeclaration_private() {
    let input = "@use \"use_variable_redeclaration_private\" as mod;\nmod.$-a: red;";
    tempfile!("use_variable_redeclaration_private.scss", "$a: green;");

    assert_err!(
        "Error: Private members can't be accessed from outside their modules.",
        input
    );
}

#[test]
fn use_cannot_see_modules_imported_by_other_modules() {
    let input = r#"
       @use "use_cannot_see_modules_imported_by_other_modules__a" as a;
       @use "use_cannot_see_modules_imported_by_other_modules__b" as b;"#;

    tempfile!(
        "use_cannot_see_modules_imported_by_other_modules__a.scss",
        "$a: green;"
    );
    tempfile!(
        "use_cannot_see_modules_imported_by_other_modules__b.scss",
        "a { color: a.$a; }"
    );

    assert_err!("Error: There is no module with the namespace \"a\".", input);
}
#[test]
fn use_can_see_modules_imported_by_other_modules_when_aliased_as_star() {
    let input = r#"
       @use "use_can_see_modules_imported_by_other_modules_when_aliased_as_star__a" as *;
       a { color: math.$e; }
    "#;

    tempfile!(
        "use_can_see_modules_imported_by_other_modules_when_aliased_as_star__a.scss",
        "@use \"sass:math\";"
    );

    assert_err!(
        r#"Error: There is no module with the namespace "math"."#,
        input
    );
}

#[test]
fn use_modules_imported_by_other_modules_does_not_cause_conflict() {
    let input = r#"
       @use "use_modules_imported_by_other_modules_does_not_cause_conflict__a" as a;
       @use "use_modules_imported_by_other_modules_does_not_cause_conflict__b" as b;"#;

    tempfile!(
        "use_modules_imported_by_other_modules_does_not_cause_conflict__a.scss",
        "$a: red;"
    );
    tempfile!(
        "use_modules_imported_by_other_modules_does_not_cause_conflict__b.scss",
        "@use \"use_modules_imported_by_other_modules_does_not_cause_conflict__a\" as a; a { color: a.$a; }"
    );

    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default()).expect(input)
    );
}

#[test]
fn use_mixin_can_use_scope_from_own_module() {
    let input = r#"
        @use "use_mixin_can_use_scope_from_own_module__a" as a;
        @include a.foo();
    "#;

    tempfile!(
        "use_mixin_can_use_scope_from_own_module__a.scss",
        "$a: red;

        @mixin foo() {
          a {
            color: $a;
          }
        }"
    );

    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default()).expect(input)
    );
}

#[test]
fn use_function_can_use_scope_from_own_module() {
    let input = r#"
        @use "use_function_can_use_scope_from_own_module__a" as a;

        a {
            color: a.foo();
        }
    "#;

    tempfile!(
        "use_function_can_use_scope_from_own_module__a.scss",
        "$a: red;

        @function foo() {
            @return $a;
        }"
    );

    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default()).expect(input)
    );
}

#[test]
fn use_variable_redeclaration_builtin() {
    let input = "@use \"sass:math\";\nmath.$e: red;";

    assert_err!("Error: Cannot modify built-in variable.", input);
}

#[test]
fn use_variable_declaration_between_use() {
    let input = r#"
        $a: red;
        $b: green;
        @use "sass:math";
        $b: red;
        @use "sass:meta";
        a {
            color: $a $b;
        }"#;

    assert_eq!(
        "a {\n  color: red red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default()).expect(input)
    );
}

#[test]
fn include_mixin_with_star_namespace() {
    let mut fs = TestFs::new();

    fs.add_file(
        "a.scss",
        r#"@mixin foo() {
            a {
                color: red;
            }
        }"#,
    );

    let input = r#"
        @use "a" as *;

        @include foo();
    "#;

    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
fn include_variable_with_star_namespace() {
    let mut fs = TestFs::new();

    fs.add_file("a.scss", r#"$a: red;"#);

    let input = r#"
        @use "a" as *;

        a {
            color: $a;
        }
    "#;

    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
fn include_function_with_star_namespace() {
    let mut fs = TestFs::new();

    fs.add_file(
        "a.scss",
        r#"@function foo() {
            @return red;
        }"#,
    );

    let input = r#"
        @use "a" as *;

        a {
            color: foo();
        }
    "#;

    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
fn use_with_through_forward_multiple() {
    let mut fs = TestFs::new();

    fs.add_file(
        "_used.scss",
        r#"
            @forward "left" with ($a: from used !default);
            @forward "right" with ($b: from used !default);
        "#,
    );
    fs.add_file(
        "_left.scss",
        r#"
            $a: from left !default;

            in-left {
                c: $a
            }
        "#,
    );
    fs.add_file(
        "_right.scss",
        r#"
            $b: from left !default;

            in-right {
                d: $b
            }
        "#,
    );

    let input = r#"
        @use "used" with ($a: from input, $b: from input);
    "#;

    assert_eq!(
        "in-left {\n  c: from input;\n}\n\nin-right {\n  d: from input;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
fn module_functions_empty() {
    let mut fs = TestFs::new();

    fs.add_file("_other.scss", r#""#);

    let input = r#"
        @use "sass:meta";
        @use "other";

        a {
            b: meta.inspect(meta.module-functions("other"))
        }
    "#;

    assert_eq!(
        "a {\n  b: ();\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
fn module_functions_through_forward() {
    let mut fs = TestFs::new();

    fs.add_file(
        "_a.scss",
        r#"
        @forward "b";
    "#,
    );
    fs.add_file(
        "_b.scss",
        r#"
        @function foo() {}
    "#,
    );

    let input = r#"
        @use "sass:meta";
        @use "a";

        a {
            b: meta.inspect(meta.module-functions("a"))
        }
    "#;

    assert_eq!(
        "a {\n  b: (\"foo\": get-function(\"foo\"));\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
fn use_variable_declared_in_this_and_other_module() {
    let mut fs = TestFs::new();

    fs.add_file(
        "_a.scss",
        r#"
        $a: blue;
    "#,
    );

    let input = r#"
        $a: red;
        @use "a" as *;

        a {
            color: $a;
        }
    "#;

    assert_err!(
        input,
        "Error: This module and the new module both define a variable named \"$a\".",
        grass::Options::default().fs(&fs)
    );
}

#[test]
#[ignore = "we don't check for this"]
fn use_variable_declared_in_two_modules() {
    let mut fs = TestFs::new();

    fs.add_file(
        "_a.scss",
        r#"
        $a: blue;
    "#,
    );

    fs.add_file(
        "_b.scss",
        r#"
        $a: red;
    "#,
    );

    let input = r#"
        @use "a" as *;
        @use "b" as *;

        a {
            color: $a;
        }
    "#;

    assert_err!(
        input,
        "Error: This variable is available from multiple global modules.",
        grass::Options::default().fs(&fs)
    );
}

#[test]
fn import_module_using_same_builtin_module() {
    let mut fs = TestFs::new();

    fs.add_file(
        "_a.scss",
        r#"
        @use "sass:meta";
    "#,
    );

    fs.add_file(
        "_b.scss",
        r#"
        $a: red;
    "#,
    );

    let input = r#"
        @use "sass:meta";
        @import "a";
    "#;

    assert_eq!(
        "",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
fn import_module_using_same_builtin_module_has_styles() {
    let mut fs = TestFs::new();

    fs.add_file(
        "_a.scss",
        r#"
        @use "sass:meta";

        a {
            color: red;
        }
    "#,
    );

    fs.add_file(
        "_b.scss",
        r#"
        $a: red;
    "#,
    );

    let input = r#"
        @use "sass:meta";
        @import "a";
    "#;

    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
#[ignore = "we don't hermetically evaluate @extend"]
fn use_module_with_extend() {
    let mut fs = TestFs::new();

    fs.add_file(
        "_a.scss",
        r#"
        a {
            @extend b;
        }
    "#,
    );

    let input = r#"
        @use "a";
        b {
            color: red;
        }
    "#;

    assert_err!(
        input,
        "Error: The target selector was not found.",
        grass::Options::default().fs(&fs)
    );
}

// todo: refactor these tests to use testfs where possible
