use std::io::Write;
use tempfile::Builder;

use grass::StyleSheet;

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
}

#[test]
fn imports_variable() {
    let input = "@import \"imports_variable\";\na {\n color: $a;\n}";
    tempfile!("imports_variable", "$a: red;");
    assert_eq!(
        "a {\n  color: red;\n}\n",
        &StyleSheet::new(input.to_string()).expect(input)
    );
}

#[test]
fn import_no_semicolon() {
    let input = "@import \"import_no_semicolon\"\na {\n color: $a;\n}";
    tempfile!("import_no_semicolon", "$a: red;");
    assert_eq!(
        "a {\n  color: red;\n}\n",
        &StyleSheet::new(input.to_string()).expect(input)
    );
}

#[test]
fn single_quotes_import() {
    let input = "@import 'single_quotes_import';\na {\n color: $a;\n}";
    tempfile!("single_quotes_import", "$a: red;");
    assert_eq!(
        "a {\n  color: red;\n}\n",
        &StyleSheet::new(input.to_string()).expect(input)
    );
}

#[test]
fn finds_name_scss() {
    let input = "@import \"finds_name_scss\";\na {\n color: $a;\n}";
    tempfile!("finds_name_scss.scss", "$a: red;");
    assert_eq!(
        "a {\n  color: red;\n}\n",
        &StyleSheet::new(input.to_string()).expect(input)
    );
}

#[test]
fn finds_underscore_name_scss() {
    let input = "@import \"finds_underscore_name_scss\";\na {\n color: $a;\n}";
    tempfile!("_finds_underscore_name_scss.scss", "$a: red;");
    assert_eq!(
        "a {\n  color: red;\n}\n",
        &StyleSheet::new(input.to_string()).expect(input)
    );
}
