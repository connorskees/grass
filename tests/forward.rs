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
