#![cfg(test)]

#[macro_export]
macro_rules! test {
    ($( #[$attr:meta] ),*$func:ident, $input:expr) => {
        $(#[$attr])*
        #[test]
        #[allow(non_snake_case)]
        fn $func() {
            let sass = grass::StyleSheet::new($input.to_string())
                .expect(concat!("failed to parse on ", $input));
            assert_eq!(
                String::from($input),
                sass
            );
        }
    };
    ($( #[$attr:meta] ),*$func:ident, $input:expr, $output:expr) => {
        $(#[$attr])*
        #[test]
        #[allow(non_snake_case)]
        fn $func() {
            let sass = grass::StyleSheet::new($input.to_string())
                .expect(concat!("failed to parse on ", $input));
            assert_eq!(
                String::from($output),
                sass
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
                Err(e) => assert_eq!($err, e.to_string()
                                                .chars()
                                                .take_while(|c| *c != '\n')
                                                .collect::<String>()
                                                .as_str()
                ),
            }
        }
    };
}
