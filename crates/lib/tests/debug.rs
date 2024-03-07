use macros::TestLogger;

#[macro_use]
mod macros;

#[test]
fn simple_debug() {
    let input = "@debug 2";
    let logger = TestLogger::default();
    let options = grass::Options::default().logger(&logger);
    let output = grass::from_string(input.to_string(), &options).expect(input);
    assert_eq!(&output, "");
    assert_eq!(&[String::from("2")], logger.debug_messages().as_slice());
    assert_eq!(&[] as &[String], logger.warning_messages().as_slice());
}

#[test]
fn simple_debug_with_semicolon() {
    let input = "@debug 2;";
    let logger = TestLogger::default();
    let options = grass::Options::default().logger(&logger);
    let output = grass::from_string(input.to_string(), &options).expect(input);
    assert_eq!(&output, "");
    assert_eq!(&[String::from("2")], logger.debug_messages().as_slice());
    assert_eq!(&[] as &[String], logger.warning_messages().as_slice());
}

#[test]
fn debug_while_quiet() {
    let input = "@debug 2;";
    let logger = TestLogger::default();
    let options = grass::Options::default().logger(&logger).quiet(true);
    let output = grass::from_string(input.to_string(), &options).expect(input);
    assert_eq!(&output, "");
    assert_eq!(&[] as &[String], logger.debug_messages().as_slice());
    assert_eq!(&[] as &[String], logger.warning_messages().as_slice());
}
