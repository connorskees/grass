use std::io::Write;

use macros::TestFs;

#[macro_use]
mod macros;

#[test]
fn basic_forward() {
    let input = r#"
        @use "basic_forward__b";

        a {
            color: basic_forward__b.$a;
        }
    "#;
    tempfile!("basic_forward__b.scss", r#"@forward "basic_forward__a";"#);
    tempfile!("basic_forward__a.scss", r#"$a: red;"#);
    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default()).expect(input)
    );
}

#[test]
fn basic_forward_with_configuration() {
    let input = r#"
        @use "basic_forward_with_configuration__b";

        a {
            color: basic_forward_with_configuration__b.$a;
        }
    "#;
    tempfile!(
        "basic_forward_with_configuration__b.scss",
        r#"@forward "basic_forward_with_configuration__a" with ($a: green);"#
    );
    tempfile!(
        "basic_forward_with_configuration__a.scss",
        r#"$a: red !default;"#
    );
    assert_eq!(
        "a {\n  color: green;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default()).expect(input)
    );
}

#[test]
fn basic_forward_with_configuration_no_default_error() {
    let input = r#"
        @use "basic_forward_with_configuration_no_default_error__b";

        a {
            color: basic_forward_with_configuration_no_default_error__b.$a;
        }
    "#;
    tempfile!(
        "basic_forward_with_configuration_no_default_error__b.scss",
        r#"@forward "basic_forward_with_configuration_no_default_error__a" with ($a: green);"#
    );
    tempfile!(
        "basic_forward_with_configuration_no_default_error__a.scss",
        r#"$a: red;"#
    );
    assert_err!(
        "Error: This variable was not declared with !default in the @used module.",
        input
    );
}

// todo: same test for fns and mixins?
#[test]
fn can_redeclare_forwarded_upstream_vars() {
    let input = r#"
        @use "can_redeclare_forwarded_upstream_vars__a" as a;
        @use "can_redeclare_forwarded_upstream_vars__b" as b;

        a {
            color: a.$a;
            color: b.$a;
        }
    "#;
    tempfile!(
        "can_redeclare_forwarded_upstream_vars__b.scss",
        r#"
        @forward "can_redeclare_forwarded_upstream_vars__a";

        $a: midstream;
    "#
    );
    tempfile!(
        "can_redeclare_forwarded_upstream_vars__a.scss",
        r#"$a: upstream;"#
    );
    assert_eq!(
        "a {\n  color: upstream;\n  color: midstream;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default()).expect(input)
    );
}

#[test]
fn through_forward_with_as() {
    let mut fs = TestFs::new();

    fs.add_file(
        "_downstream.scss",
        r#"@forward "midstream" with ($b-a: configured);"#,
    );
    fs.add_file("_midstream.scss", r#"@forward "upstream" as b-*;"#);
    fs.add_file(
        "_upstream.scss",
        r#"
            $a: original !default;
            c {d: $a}
        "#,
    );

    let input = r#"@use "downstream";"#;

    assert_eq!(
        "c {\n  d: configured;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}
#[test]
fn through_forward_with_unconfigured() {
    let mut fs = TestFs::new();

    fs.add_file(
        "_downstream.scss",
        r#"@forward "midstream" with ($a: from downstream);"#,
    );
    fs.add_file(
        "_midstream.scss",
        r#"@forward "upstream" with ($b: from midstream !default);"#,
    );
    fs.add_file(
        "_upstream.scss",
        r#"
            $a: from upstream !default;
            $b: from upstream !default;
            c {
                a: $a;
                b: $b;
            }
        "#,
    );

    let input = r#"@use "downstream";"#;

    assert_eq!(
        "c {\n  a: from downstream;\n  b: from midstream;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
fn member_visibility_variable_declaration() {
    let mut fs = TestFs::new();

    fs.add_file("_midstream.scss", r#"@forward "upstream" hide d;"#);
    fs.add_file(
        "_upstream.scss",
        r#"
            $a: old value;

            @function get-a() {@return $a}
        "#,
    );

    let input = r#"
        @use "midstream";

        midstream.$a: new value;

        b {c: midstream.get-a()};
    "#;

    assert_eq!(
        "b {\n  c: new value;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
#[ignore = "forward is still WIP"]
fn member_import_precedence_top_level() {
    let mut fs = TestFs::new();

    fs.add_file("_midstream.scss", r#"@forward "upstream";"#);
    fs.add_file(
        "_upstream.scss",
        r#"
            $a: in-upstream;
        "#,
    );

    let input = r#"
        $a: in-input;

        @import "midstream";

        b {c: $a}
    "#;

    assert_eq!(
        "b {\n  c: in-upstream;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
fn member_as_function() {
    let mut fs = TestFs::new();

    fs.add_file("_midstream.scss", r#"@forward "upstream" as d-*;"#);
    fs.add_file(
        "_upstream.scss",
        r#"
            @function c() {
                @return e;
            }
        "#,
    );

    let input = r#"
        @use "midstream";

        a {
            b: midstream.d-c();
        }
    "#;

    assert_eq!(
        "a {\n  b: e;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
fn member_as_mixin() {
    let mut fs = TestFs::new();

    fs.add_file("_midstream.scss", r#"@forward "upstream" as b-*;"#);
    fs.add_file(
        "_upstream.scss",
        r#"
            @mixin a() {
                c {
                    d: e
                }
            }
        "#,
    );

    let input = r#"
        @use "midstream";

        @include midstream.b-a;
    "#;

    assert_eq!(
        "c {\n  d: e;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
fn member_as_variable_use() {
    let mut fs = TestFs::new();

    fs.add_file("_midstream.scss", r#"@forward "upstream" as d-*;"#);
    fs.add_file(
        "_upstream.scss",
        r#"
            $c: e;
        "#,
    );

    let input = r#"
        @use "midstream";

        a {b: midstream.$d-c}
    "#;

    assert_eq!(
        "a {\n  b: e;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
fn member_as_variable_assignment_toplevel() {
    let mut fs = TestFs::new();

    fs.add_file("_midstream.scss", r#"@forward "upstream" as d-*;"#);
    fs.add_file(
        "_upstream.scss",
        r#"
            $a: old value;

            @function get-a() {@return $a}
        "#,
    );

    let input = r#"
        @use "midstream";

        midstream.$d-a: new value;

        b {c: midstream.d-get-a()};
    "#;

    assert_eq!(
        "b {\n  c: new value;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
fn forward_module_with_error() {
    let mut fs = TestFs::new();

    fs.add_file("_error.scss", r#"a { color: 1 + red; }"#);

    let input = r#"
        @forward "error";
    "#;

    assert_err!(
        input,
        r#"Error: Undefined operation "1 + red"."#,
        grass::Options::default().fs(&fs)
    );
}

#[test]
fn use_with_multi_load_forward() {
    let mut fs = TestFs::new();

    fs.add_file(
        "_midstream.scss",
        r#"
        @forward "upstream";
    "#,
    );
    fs.add_file(
        "_upstream.scss",
        r#"
    $a: original !default;
    "#,
    );

    let input = r#"
        @use "upstream" with ($a: configured);
        @use "midstream";
        b {c: midstream.$a}
    "#;

    assert_eq!(
        "b {\n  c: configured;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
fn forward_member_import_precedence_nested() {
    let mut fs = TestFs::new();

    fs.add_file(
        "_midstream.scss",
        r#"
        @forward "upstream";
    "#,
    );
    fs.add_file(
        "_upstream.scss",
        r#"
        $a: in-upstream;
    "#,
    );

    let input = r#"
        b {
            $a: in-input;
        
            @import "midstream";
        
            c: $a;
        }
    "#;

    assert_eq!(
        "b {\n  c: in-upstream;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
fn forward_with_through_forward_hide() {
    let mut fs = TestFs::new();

    fs.add_file(
        "_downstream.scss",
        r#"
        @forward "midstream" with ($a: configured);
    "#,
    );
    fs.add_file(
        "_midstream.scss",
        r#"
        @forward "upstream" hide $b;
    "#,
    );
    fs.add_file(
        "_upstream.scss",
        r#"
        $a: original !default;
        b {c: $a}
    "#,
    );

    let input = r#"
        @use "downstream";
    "#;

    assert_eq!(
        "b {\n  c: configured;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
fn forward_with_through_forward_show() {
    let mut fs = TestFs::new();

    fs.add_file(
        "_downstream.scss",
        r#"
        @forward "midstream" with ($a: configured);
    "#,
    );
    fs.add_file(
        "_midstream.scss",
        r#"
        @forward "upstream" show $a;
    "#,
    );
    fs.add_file(
        "_upstream.scss",
        r#"
        $a: original !default;
        b {c: $a}
    "#,
    );

    let input = r#"
        @use "downstream";
    "#;

    assert_eq!(
        "b {\n  c: configured;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
#[ignore = "incorrectly thinks there's a module loop"]
fn import_forwarded_first_no_use() {
    let mut fs = TestFs::new();

    fs.add_file(
        "first.scss",
        r#"
        $variable: value;
    "#,
    );
    fs.add_file(
        "first.import.scss",
        r#"
        @forward "first";
    "#,
    );
    fs.add_file(
        "second.scss",
        r#"
        a {
            b: $variable;
        }
    "#,
    );

    let input = r#"
        @import "first";
        @import "second";
    "#;

    assert_eq!(
        "a {\n  b: value;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
fn forward_same_module_with_and_without_prefix() {
    let mut fs = TestFs::new();

    fs.add_file(
        "_midstream.scss",
        r#"
            @forward "upstream";
            @forward "upstream" as b-*;
        "#,
    );
    fs.add_file(
        "_upstream.scss",
        r#"
            @mixin a() {
                c {
                    d: e
                }
            }
        "#,
    );

    let input = r#"
        @use "midstream";

        @include midstream.a;
    "#;

    assert_eq!(
        "c {\n  d: e;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

error!(
    after_style_rule,
    r#"
        a {}
        @forward "foo";
    "#,
    "Error: @forward rules must be written before any other rules."
);
