use std::{io::Write, path::Path};

use macros::TestFs;

#[macro_use]
mod macros;

#[test]
fn null_fs_cannot_import() {
    let input = "@import \"__foo\";";
    tempfile!("__foo.scss", "");
    match grass::from_string(
        input.to_string(),
        &grass::Options::default().fs(&grass::NullFs),
    ) {
        Err(e)
            if e.to_string()
                .starts_with("Error: Can't find stylesheet to import.\n") =>
        {
            ()
        }
        Ok(..) => panic!("did not fail"),
        Err(e) => panic!("failed in the wrong way: {}", e),
    }
}

#[test]
fn imports_variable() {
    let mut fs = TestFs::new();

    fs.add_file("a.scss", r#"$a: red;"#);

    let input = r#"
        @import "a";
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
#[ignore = "we don't actually check if the semicolon exists"]
fn import_no_semicolon() {
    let input = "@import \"import_no_semicolon\"\na {\n color: $a;\n}";
    tempfile!("import_no_semicolon", "$a: red;");

    drop(input);
}

#[test]
fn import_no_quotes() {
    let input = "@import import_no_quotes";

    assert_err!("Error: Expected string.", input);
}

#[test]
fn single_quotes_import() {
    let mut fs = TestFs::new();

    fs.add_file("a.scss", r#"$a: red;"#);

    let input = r#"
        @import 'a';
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
fn comma_separated_import() {
    let mut fs = TestFs::new();

    fs.add_file("a.scss", r#"$a: red"#);
    fs.add_file("b.scss", r#"p { color: blue; }"#);

    let input = r#"
        @import 'a', 'b';

        a {
            color: $a;
        }
    "#;

    assert_eq!(
        "p {\n  color: blue;\n}\n\na {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
fn comma_separated_import_order() {
    let mut fs = TestFs::new();

    fs.add_file("a.scss", r#"p { color: red; }"#);
    fs.add_file("b.scss", r#"p { color: blue; }"#);

    let input = r#"
        @import "a", "b", url(third);
    "#;

    assert_eq!(
        "@import url(third);\np {\n  color: red;\n}\n\np {\n  color: blue;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
fn comma_separated_import_order_css() {
    let mut fs = TestFs::new();

    fs.add_file("a.css", r#"p { color: red; }"#);
    fs.add_file("b.css", r#"p { color: blue; }"#);

    let input = r#"
        @import "a.css", "b", url(third);
    "#;

    assert_eq!(
        "@import \"a.css\";\n@import url(third);\np {\n  color: blue;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
fn basic_load_path() {
    tempfile!(
        "basic_load_path__a.scss",
        "@import \"basic_load_path__b\";\na {\n color: $a;\n}",
        dir = "dir-basic_load_path__a"
    );
    tempfile!(
        "basic_load_path__b.scss",
        "$a: red;",
        dir = "dir-basic_load_path__b"
    );

    assert_eq!(
        "a {\n  color: red;\n}\n",
        grass::from_path(
            "dir-basic_load_path__a/basic_load_path__a.scss",
            &grass::Options::default().load_path(std::path::Path::new("dir-basic_load_path__b"))
        )
        .unwrap()
    );
}

#[test]
fn load_path_same_directory() {
    tempfile!(
        "load_path_same_directory__a.scss",
        "@import \"dir-load_path_same_directory__a/load_path_same_directory__b\";\na {\n color: $a;\n}",
        dir = "dir-load_path_same_directory__a"
    );
    tempfile!(
        "load_path_same_directory__b.scss",
        "$a: red;",
        dir = "dir-load_path_same_directory__a"
    );

    assert_eq!(
        "a {\n  color: red;\n}\n",
        grass::from_path(
            "dir-load_path_same_directory__a/load_path_same_directory__a.scss",
            &grass::Options::default().load_path(std::path::Path::new("."))
        )
        .unwrap()
    );
}

#[test]
fn comma_separated_import_trailing() {
    let mut fs = TestFs::new();

    fs.add_file("a.scss", r#"p { color: red; }"#);
    fs.add_file("b.scss", r#"p { color: blue; }"#);

    let input = r#"
        @import "a", "b", url(third),,,,,,,,;
    "#;

    assert_err!("Error: Expected string.", input);
}

#[test]
fn finds_name_scss() {
    let mut fs = TestFs::new();

    fs.add_file("a.scss", r#"$a: red;"#);

    let input = r#"
        @import "a";
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
fn finds_underscore_name_scss() {
    let mut fs = TestFs::new();
    fs.add_file("_a.scss", r#"$a: red;"#);

    let input = r#"
        @import "a";
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
fn chained_imports() {
    let mut fs = TestFs::new();

    fs.add_file("a.scss", r#"@import "b";"#);
    fs.add_file("b.scss", r#"@import "c";"#);
    fs.add_file("c.scss", r#"$a: red;"#);

    let input = r#"
        @import "a";
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
fn imports_plain_css() {
    let mut fs = TestFs::new();

    fs.add_file("a.css", r#"a { color: red; }"#);

    let input = r#"
        @import "a";
    "#;

    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
fn imports_import_only_scss() {
    let mut fs = TestFs::new();

    fs.add_file("a.import.scss", r#"a { color: red; }"#);

    let input = r#"
        @import "a";
    "#;

    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
fn imports_sass_file() {
    let mut fs = TestFs::new();

    fs.add_file("a.sass", "a\n\tcolor: red\n");

    let input = r#"
        @import "a";
    "#;

    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
fn imports_absolute_scss() {
    let mut fs = TestFs::new();

    fs.add_file("/foo/a.scss", r#"a { color: red; }"#);

    let input = r#"
        @import "/foo/a";
    "#;

    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
fn imports_same_file_twice() {
    let mut fs = TestFs::new();

    fs.add_file("a.scss", r#"a { color: red; }"#);

    let input = r#"
        @import "a";
        @import "a";
    "#;

    assert_eq!(
        "a {\n  color: red;\n}\n\na {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
fn imports_same_file_thrice() {
    let mut fs = TestFs::new();

    fs.add_file("a.scss", r#"a { color: red; }"#);

    let input = r#"
        @import "a";
        @import "a";
        @import "a";
    "#;

    assert_eq!(
        "a {\n  color: red;\n}\n\na {\n  color: red;\n}\n\na {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}
#[test]
fn imports_self() {
    let mut fs = TestFs::new();

    fs.add_file("input.scss", r#"@import "input";"#);

    let input = r#"
        @import "input";
    "#;

    assert_err!(
        input,
        "Error: This file is already being loaded.",
        &grass::Options::default().fs(&fs)
    );
}

#[test]
fn imports_explicit_file_extension() {
    let mut fs = TestFs::new();

    fs.add_file("a.scss", r#"a { color: red; }"#);

    let input = r#"
        @import "a.scss";
    "#;

    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default().fs(&fs)).expect(input)
    );
}

#[test]
fn potentially_conflicting_directory_and_file() {
    tempfile!(
        "index.scss",
        "$a: wrong;",
        dir = "potentially_conflicting_directory_and_file"
    );
    tempfile!(
        "_potentially_conflicting_directory_and_file.scss",
        "$a: right;"
    );

    let input = r#"
        @import "potentially_conflicting_directory_and_file";
        a {
            color: $a;
        }
    "#;

    assert_eq!(
        "a {\n  color: right;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default()).expect(input)
    );
}

#[test]
fn finds_index_file_no_underscore() {
    tempfile!(
        "index.scss",
        "$a: right;",
        dir = "finds_index_file_no_underscore"
    );

    let input = r#"
        @import "finds_index_file_no_underscore";
        a {
            color: $a;
        }
    "#;

    assert_eq!(
        "a {\n  color: right;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default()).expect(input)
    );
}

#[test]
fn finds_index_file_with_underscore() {
    tempfile!(
        "_index.scss",
        "$a: right;",
        dir = "finds_index_file_with_underscore"
    );

    let input = r#"
        @import "finds_index_file_with_underscore";
        a {
            color: $a;
        }
    "#;

    assert_eq!(
        "a {\n  color: right;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default()).expect(input)
    );
}

#[test]
fn potentially_conflicting_directory_and_file_from_load_path() {
    tempfile!(
        "_potentially_conflicting_directory_and_file_from_load_path.scss",
        "$a: right;",
        dir = "potentially_conflicting_directory_and_file_from_load_path__a"
    );
    tempfile!(
        "index.scss",
        "$a: wrong;",
        dir = "potentially_conflicting_directory_and_file_from_load_path__a/potentially_conflicting_directory_and_file_from_load_path"
    );

    let input = r#"
        @import "potentially_conflicting_directory_and_file_from_load_path";
        a {
            color: $a;
        }
    "#;

    assert_eq!(
        "a {\n  color: right;\n}\n",
        &grass::from_string(
            input.to_string(),
            &grass::Options::default().load_path(&Path::new(
                "potentially_conflicting_directory_and_file_from_load_path__a"
            ))
        )
        .expect(input)
    );
}

#[test]
fn chained_imports_in_directory() {
    let input = "@import \"chained_imports_in_directory__a\";\na {\n color: $a;\n}";
    tempfile!(
        "chained_imports_in_directory__a.scss",
        "@import \"chained_imports_in_directory__b\";"
    );
    tempfile!(
        "index.scss",
        "@import \"../chained_imports_in_directory__c\";",
        dir = "chained_imports_in_directory__b"
    );
    tempfile!("chained_imports_in_directory__c.scss", "$a: red;");
    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default()).expect(input)
    );
}

#[test]
fn explicit_file_extension_import_inside_index_file() {
    let input =
        "@import \"explicit_file_extension_import_inside_index_file\";\na {\n color: $a;\n}";
    tempfile!(
        "_a.scss",
        "$a: red;",
        dir = "explicit_file_extension_import_inside_index_file"
    );
    tempfile!(
        "_index.scss",
        "@import \"a.scss\";",
        dir = "explicit_file_extension_import_inside_index_file"
    );
    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default()).expect(input)
    );
}
error!(
    // note: dart-sass error is "expected more input."
    missing_input_after_import,
    "@import", "Error: Expected string."
);
error!(
    import_unquoted_http,
    "@import http://foo.com/;", "Error: Expected string."
);
error!(
    import_file_doesnt_exist,
    "@import \"idontexist\";", "Error: Can't find stylesheet to import."
);
error!(
    file_name_is_two_periods,
    "@import \"foo/..\";", "Error: Can't find stylesheet to import."
);
test!(
    import_beginning_with_http,
    "@import \"http://foo.com/\";",
    "@import \"http://foo.com/\";\n"
);
test!(
    import_beginning_with_http_no_ending_slash,
    "@import \"http://foo.com\";",
    "@import \"http://foo.com\";\n"
);
test!(
    import_beginning_with_https,
    "@import \"https://foo.com/\";",
    "@import \"https://foo.com/\";\n"
);
test!(
    import_ending_in_css,
    "@import \"foo.css\";",
    "@import \"foo.css\";\n"
);
test!(
    newline_in_plain_css,
    "@import \"fo\\\no.css\";",
    "@import \"fo\\\no.css\";\n"
);
test!(import_url, "@import url(foo..);", "@import url(foo..);\n");
test!(
    import_url_interpolation,
    "@import url(#{1+1}..);",
    "@import url(2..);\n"
);
test!(
    #[ignore = "we currently place plain @import ahead of loud comments that precede it"]
    import_multiline_comments_everywhere,
    "  /**/  @import  /**/  url(foo)  /**/  ;",
    "/**/\n@import url(foo);\n"
);
test!(
    plain_css_begins_with_two_slashes,
    "@import \"//fonts.googleapis.com/css?family=Droid+Sans\";",
    "@import \"//fonts.googleapis.com/css?family=Droid+Sans\";\n"
);
test!(
    plain_css_retains_backslash_for_escaped_space,
    r#"@import "hux\ bux.css";"#,
    "@import \"hux\\ bux.css\";\n"
);
test!(
    plain_css_is_moved_to_top_of_file,
    "a {
        color: red;
    }

    @import url(\"foo.css\");",
    "@import url(\"foo.css\");\na {\n  color: red;\n}\n"
);
test!(
    many_import_conditions,
    r#"@import "a" b c d(e) supports(f: g) h i j(k) l m (n: o), (p: q);"#,
    "@import \"a\" b c d(e) supports(f: g) h i j(k) l m (n: o), (p: q);\n"
);
test!(
    import_supports_condition_with_paren,
    r#"@import "a" supports(a(  /**/  ));"#,
    "@import \"a\" supports(a( /**/ ));\n"
);
error!(
    import_supports_condition_non_ident,
    r#"@import "a" supports(1a)"#, "Error: expected \":\"."
);
error!(unclosed_single_quote, r#"@import '"#, "Error: Expected '.");
error!(unclosed_double_quote, r#"@import ""#, "Error: Expected \".");
error!(
    dynamic_disallowed_inside_if,
    r#"@if true {
        @import "foo";
    }"#,
    "Error: This at-rule is not allowed here."
);
error!(
    dynamic_disallowed_inside_while,
    r#"@while true {
        @import "foo";
    }"#,
    "Error: This at-rule is not allowed here."
);
error!(
    dynamic_disallowed_inside_for,
    r#"@for $i from 0 through 1 {
        @import "foo";
    }"#,
    "Error: This at-rule is not allowed here."
);
error!(
    dynamic_disallowed_inside_each,
    r#"@each $i in a {
        @import "foo";
    }"#,
    "Error: This at-rule is not allowed here."
);
test!(
    static_allowed_inside_if,
    r#"@if true {
        @import "foo.css";
    }"#,
    "@import \"foo.css\";\n"
);
test!(
    static_allowed_inside_while,
    r#"
    $a: 0;
    @while $a == 0 {
        @import "foo.css";
        $a: 1;
    }"#,
    "@import \"foo.css\";\n"
);
test!(
    static_allowed_inside_for,
    r#"@for $i from 0 to 1 {
        @import "foo.css";
    }"#,
    "@import \"foo.css\";\n"
);
test!(
    static_allowed_inside_each,
    r#"@each $i in a {
        @import "foo.css";
    }"#,
    "@import \"foo.css\";\n"
);
error!(
    dynamic_disallowed_inside_mixin,
    r#"@mixin foo {
        @import "foo";
    }"#,
    "Error: This at-rule is not allowed here."
);

// todo: edge case tests for plain css imports moved to top
// todo: test for calling paths, e.g. `grass b\index.scss`
// todo: test for absolute paths (how?)
// todo: test for @import accessing things declared beforehand
// e.g. b { @import } | $a: red; @import
