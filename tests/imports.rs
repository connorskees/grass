use std::io::Write;
use tempfile::Builder;

/// Create a temporary file with the given name
/// and contents.
///
/// This must be a macro rather than a function
/// because the tempfile will be deleted when it
/// exits scope
macro_rules! tempfile {
    ($name:literal, $content:literal) => {
        let mut f = Builder::new()
            .rand_bytes(0)
            .prefix("")
            .suffix($name)
            .tempfile_in("")
            .unwrap();
        write!(f, "{}", $content).unwrap();
    };
    ($name:literal, $content:literal, dir=$dir:literal) => {
        let _d = Builder::new()
            .rand_bytes(0)
            .prefix("")
            .suffix($dir)
            .tempdir_in("")
            .unwrap();
        let mut f = Builder::new()
            .rand_bytes(0)
            .prefix("")
            .suffix($name)
            .tempfile_in($dir)
            .unwrap();
        write!(f, "{}", $content).unwrap();
    };
}

#[test]
fn imports_variable() {
    let input = "@import \"imports_variable\";\na {\n color: $a;\n}";
    tempfile!("imports_variable", "$a: red;");
    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string()).expect(input)
    );
}

#[test]
fn import_no_semicolon() {
    let input = "@import \"import_no_semicolon\"\na {\n color: $a;\n}";
    tempfile!("import_no_semicolon", "$a: red;");
    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string()).expect(input)
    );
}

#[test]
fn import_no_quotes() {
    let input = "@import import_no_quotes";
    tempfile!("import_no_quotes", "$a: red;");
    match grass::from_string(input.to_string()) {
        Ok(..) => panic!("did not fail"),
        Err(e) => assert_eq!(
            "Error: Expected string.",
            e.to_string()
                .chars()
                .take_while(|c| *c != '\n')
                .collect::<String>()
                .as_str()
        ),
    }
}

#[test]
fn single_quotes_import() {
    let input = "@import 'single_quotes_import';\na {\n color: $a;\n}";
    tempfile!("single_quotes_import", "$a: red;");
    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string()).expect(input)
    );
}

#[test]
fn finds_name_scss() {
    let input = "@import \"finds_name_scss\";\na {\n color: $a;\n}";
    tempfile!("finds_name_scss.scss", "$a: red;");
    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string()).expect(input)
    );
}

#[test]
fn finds_underscore_name_scss() {
    let input = "@import \"finds_underscore_name_scss\";\na {\n color: $a;\n}";
    tempfile!("_finds_underscore_name_scss.scss", "$a: red;");
    assert_eq!(
        "a {\n  color: red;\n}\n",
        &grass::from_string(input.to_string()).expect(input)
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
        &grass::from_string(input.to_string()).expect(input)
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
        &grass::from_string(input.to_string()).expect(input)
    );
}

// todo: test for calling paths, e.g. `grass b\index.scss`
