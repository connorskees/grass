use std::io::Write;
use tempfile::Builder;

use grass::StyleSheet;

macro_rules! test_import {
    ($func:ident, $input:literal => $output:literal | $( $name:literal($content:literal) ),*) => {
        #[test]
        fn $func() {
            $(
                let mut f = Builder::new().rand_bytes(0).prefix("").suffix($name).tempfile_in("").unwrap();
                write!(f, $content).unwrap();
            )*
            let mut buf = Vec::new();
            StyleSheet::new($input)
            .expect(concat!("failed to parse in "))
            .print_as_css(&mut buf)
            .expect(concat!("failed to pretty print on ", $input));
            assert_eq!(
                String::from($output),
                String::from_utf8(buf).expect("produced invalid utf8")
            );
        }
    }
}

// we have to use test name as filename because tests are run multithreaded in the same directory, so some names may conflict
test_import!(imports_variable, "@import \"imports_variable\";\na {\n color: $a;\n}" => "a {\n  color: red;\n}\n" | "imports_variable"("$a: red;"));
test_import!(single_quotes_import, "@import 'single_quotes_import';\na {\n color: $a;\n}" => "a {\n  color: red;\n}\n" | "single_quotes_import"("$a: red;"));
test_import!(finds_name_scss, "@import \"finds_name_scss\";\na {\n color: $a;\n}" => "a {\n  color: red;\n}\n" | "finds_name_scss.scss"("$a: red;"));
test_import!(finds_underscore_name_scss, "@import \"finds_underscore_name_scss\";\na {\n color: $a;\n}" => "a {\n  color: red;\n}\n" | "_finds_underscore_name_scss.scss"("$a: red;"));
