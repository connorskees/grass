#![cfg(test)]

#[macro_export]
macro_rules! test {
    ($( #[$attr:meta] ),*$func:ident, $input:expr) => {
        $(#[$attr])*
        #[test]
        #[allow(non_snake_case)]
        fn $func() {
            let mut buf = Vec::new();
            grass::StyleSheet::new($input.to_string())
                .expect(concat!("failed to parse on ", $input))
                .print_as_css(&mut buf)
                .expect(concat!("failed to pretty print on ", $input));
            assert_eq!(
                String::from($input),
                String::from_utf8(buf).expect("produced invalid utf8")
            );
        }
    };
    ($( #[$attr:meta] ),*$func:ident, $input:expr, $output:expr) => {
        $(#[$attr])*
        #[test]
        #[allow(non_snake_case)]
        fn $func() {
            let mut buf = Vec::new();
            grass::StyleSheet::new($input.to_string())
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

/// Verify the error *message*
/// Span and scope information are not yet tested
#[macro_export]
macro_rules! error {
    ($( #[$attr:meta] ),*$func:ident, $input:expr, $err:expr) => {
        $(#[$attr])*
        #[test]
        #[allow(non_snake_case)]
        fn $func() {
            match grass::StyleSheet::new($input.to_string()) {
                Ok(..) => panic!("did not fail"),
                Err(e) => assert_eq!($err, e.to_string().chars().take_while(|c| *c != '\n').collect::<String>().as_str()),
            }
        }
    };
}
