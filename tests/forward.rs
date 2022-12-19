use std::io::Write;

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
