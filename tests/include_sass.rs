#[cfg(feature = "macro")]
#[test]
fn basic() {
    let css: &str = grass::include!("./input.scss");

    assert!(css == "a {\n  color: red;\n}\n");
}
