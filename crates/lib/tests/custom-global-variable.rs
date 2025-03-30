use grass::{from_string, Options};
use grass_compiler::sass_value::{QuoteKind, Value};

#[test]
fn lookup_custom_global_variable() {
    let opts = Options::default().add_custom_var("x", Value::String("foo".into(), QuoteKind::None));
    assert_eq!(
        from_string("a {\n  test: $x;\n}\n", &opts).ok(),
        Some("a {\n  test: foo;\n}\n".to_owned())
    );
}

#[test]
fn user_defined_takes_precedence_over_global_variable() {
    let opts = Options::default().add_custom_var("x", Value::String("foo".into(), QuoteKind::None));
    assert_eq!(
        from_string("$x: bar;\na {\n  test: $x;\n}\n", &opts).ok(),
        Some("a {\n  test: bar;\n}\n".to_owned())
    );
}
