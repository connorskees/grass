use std::io::Write;

#[macro_use]
mod macros;

#[test]
fn null_fs_cannot_import() {
    let input = "@import \"foo\";";
    tempfile!("foo.scss", "");
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
    let input = "@import \"imports_variable\";\na {\n color: $a;\n}";
    tempfile!("imports_variable", "$a: red;");
    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default()).expect(input)
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
    tempfile!("import_no_quotes", "$a: red;");

    assert_err!("Error: Expected string.", input);
}

#[test]
fn single_quotes_import() {
    let input = "@import 'single_quotes_import';\na {\n color: $a;\n}";
    tempfile!("single_quotes_import", "$a: red;");
    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default()).expect(input)
    );
}

#[test]
fn comma_separated_import() {
    let input = "@import 'comma_separated_import_first', 'comma_separated_import_second';\na {\n color: $a;\n}";
    tempfile!("comma_separated_import_first", "$a: red;");
    tempfile!("comma_separated_import_second", "p { color: blue; }");
    assert_eq!(
        "p {\n  color: blue;\n}\n\na {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default()).expect(input)
    );
}

#[test]
fn comma_separated_import_order() {
    let input =
        "@import 'comma_separated_import_order1', 'comma_separated_import_order2', url(third);";
    tempfile!("comma_separated_import_order1", "p { color: red; }");
    tempfile!("comma_separated_import_order2", "p { color: blue; }");
    assert_eq!(
        "@import url(third);\np {\n  color: red;\n}\n\np {\n  color: blue;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default()).expect(input)
    );
}

#[test]
fn comma_separated_import_order_css() {
    let input =
        "@import 'comma_separated_import_order1.css', 'comma_separated_import_order_css', url(third);";
    tempfile!("comma_separated_import_order1.css", "p { color: red; }");
    tempfile!("comma_separated_import_order_css", "p { color: blue; }");
    assert_eq!(
        "@import \"comma_separated_import_order1.css\";\n@import url(third);\np {\n  color: blue;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default()).expect(input)
    );
}

#[test]
fn comma_separated_import_trailing() {
    let input =
        "@import 'comma_separated_import_trailing1', 'comma_separated_import_trailing2', url(third),,,,,,,,;";
    tempfile!("comma_separated_import_trailing1", "p { color: red; }");
    tempfile!("comma_separated_import_trailing2", "p { color: blue; }");

    match grass::from_string(input.to_string(), &grass::Options::default()) {
        Ok(..) => panic!("did not fail"),
        Err(e) => assert_eq!(
            "Error: Expected expression.",
            e.to_string()
                .chars()
                .take_while(|c| *c != '\n')
                .collect::<String>()
                .as_str()
        ),
    }
}

#[test]
fn finds_name_scss() {
    let input = "@import \"finds_name_scss\";\na {\n color: $a;\n}";
    tempfile!("finds_name_scss.scss", "$a: red;");
    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default()).expect(input)
    );
}

#[test]
fn finds_underscore_name_scss() {
    let input = "@import \"finds_underscore_name_scss\";\na {\n color: $a;\n}";
    tempfile!("_finds_underscore_name_scss.scss", "$a: red;");
    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default()).expect(input)
    );
}

#[test]
fn chained_imports() {
    let input = "@import \"chained_imports__a\";\na {\n color: $a;\n}";
    tempfile!("chained_imports__a.scss", "@import \"chained_imports__b\";");
    tempfile!("chained_imports__b.scss", "@import \"chained_imports__c\";");
    tempfile!("chained_imports__c.scss", "$a: red;");
    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string(), &grass::Options::default()).expect(input)
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

error!(
    missing_input_after_import,
    "@import", "Error: expected more input."
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
    plain_css_is_moved_to_top_of_file,
    "a {
        color: red;
    }

    @import url(\"foo.css\");",
    "@import url(\"foo.css\");\na {\n  color: red;\n}\n"
);

// todo: edge case tests for plain css imports moved to top
// todo: test for calling paths, e.g. `grass b\index.scss`
// todo: test for absolute paths (how?)
