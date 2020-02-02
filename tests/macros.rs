#![cfg(test)]

#[allow(unused_macros)]
macro_rules! test {
    ($func:ident, $input:literal) => {
        #[test]
        fn $func() {
            let mut buf = Vec::new();
            grass::StyleSheet::new($input)
                .expect(concat!("failed to parse on ", $input))
                .print_as_css(&mut buf)
                .expect(concat!("failed to pretty print on ", $input));
            assert_eq!(
                String::from($input),
                String::from_utf8(buf).expect("produced invalid utf8")
            );
        }
    };
    ($func:ident, $input:literal, $output:literal) => {
        #[test]
        fn $func() {
            let mut buf = Vec::new();
            grass::StyleSheet::new($input)
                .expect(concat!("failed to parse on ", $input))
                .print_as_css(&mut buf)
                .expect(concat!("failed to pretty print on ", $input));
            assert_eq!(
                String::from($output),
                String::from_utf8(buf).expect("produced invalid utf8")
            );
        }
    };
}
