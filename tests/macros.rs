#![cfg(test)]

#[macro_export]
macro_rules! test {
    ($func:ident, $input:expr) => {
        #[test]
        #[allow(non_snake_case)]
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
    ($func:ident, $input:expr, $output:expr) => {
        #[test]
        #[allow(non_snake_case)]
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

#[macro_export]
macro_rules! error {
    ($func:ident, $input:expr, $err:expr) => {
        #[test]
        #[allow(non_snake_case)]
        fn $func() {
            match grass::StyleSheet::new($input) {
                Ok(..) => panic!("did not fail"),
                Err(e) => assert_eq!($err, e.to_string().as_str()),
            }
        }
    };
}
