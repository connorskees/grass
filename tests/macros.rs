use std::{
    borrow::Cow,
    collections::BTreeMap,
    path::{Path, PathBuf},
};

use grass::Fs;

#[macro_export]
macro_rules! test {
    (@base $( #[$attr:meta] ),*$func:ident, $input:expr, $output:expr, $options:expr) => {
        $(#[$attr])*
        #[test]
        #[allow(non_snake_case)]
        fn $func() {
            let sass = grass::from_string($input.to_string(), &$options)
                .expect(concat!("failed to parse on ", $input));
            assert_eq!(
                String::from($output),
                sass
            );
        }
    };
    ($( #[$attr:meta] ),*$func:ident, $input:expr, $output:expr, $options:expr) => {
        test!(@base $(#[$attr])* $func, $input, $output, $options);
    };
    ($( #[$attr:meta] ),*$func:ident, $input:expr, $output:expr) => {
        test!(@base $(#[$attr])* $func, $input, $output, grass::Options::default());
    };
}

/// Verify the error *message*
/// Span and scope information are not yet tested
#[macro_export]
macro_rules! error {
    ($( #[$attr:meta] ),*$func:ident, $input:expr, $err:expr) => {
        // $(#[$attr])*
        // #[test]
        // #[allow(non_snake_case)]
        // fn $func() {
        //     match grass::from_string($input.to_string(), &grass::Options::default()) {
        //         Ok(..) => panic!("did not fail"),
        //         Err(e) => assert_eq!($err, e.to_string()
        //                                         .chars()
        //                                         .take_while(|c| *c != '\n')
        //                                         .collect::<String>()
        //                                         .as_str()
        //         ),
        //     }
        // }
    };
}

/// Create a temporary file with the given name
/// and contents.
///
/// This must be a macro rather than a function
/// because the tempfile will be deleted when it
/// exits scope
#[macro_export]
macro_rules! tempfile {
    ($name:literal, $content:literal) => {
        let mut f = tempfile::Builder::new()
            .rand_bytes(0)
            .prefix("")
            .suffix($name)
            .tempfile_in("")
            .unwrap();
        write!(f, "{}", $content).unwrap();
    };
    ($name:literal, $content:literal, dir=$dir:literal) => {
        let _d = tempfile::Builder::new()
            .rand_bytes(0)
            .prefix("")
            .suffix($dir)
            .tempdir_in("")
            .unwrap();
        let mut f = tempfile::Builder::new()
            .rand_bytes(0)
            .prefix("")
            .suffix($name)
            .tempfile_in($dir)
            .unwrap();
        write!(f, "{}", $content).unwrap();
    };
}

#[macro_export]
macro_rules! assert_err {
    ($err:literal, $input:expr) => {
        match grass::from_string($input.to_string(), &grass::Options::default()) {
            Ok(..) => panic!("did not fail"),
            Err(e) => assert_eq!(
                $err,
                e.to_string()
                    .chars()
                    .take_while(|c| *c != '\n')
                    .collect::<String>()
                    .as_str()
            ),
        }
    };
}

#[derive(Debug)]
pub struct TestFs {
    files: BTreeMap<PathBuf, Cow<'static, str>>,
}

#[allow(unused)]
impl TestFs {
    pub fn new() -> Self {
        Self {
            files: BTreeMap::new(),
        }
    }

    pub fn add_file(&mut self, name: &'static str, contents: &'static str) {
        self.files
            .insert(PathBuf::from(name), Cow::Borrowed(contents));
    }
}

#[allow(unused)]
impl Fs for TestFs {
    fn is_file(&self, path: &Path) -> bool {
        self.files.contains_key(path)
    }

    fn is_dir(&self, path: &Path) -> bool {
        false
    }

    fn read(&self, path: &Path) -> std::io::Result<Vec<u8>> {
        Ok(self.files.get(path).unwrap().as_bytes().to_vec())
    }
}
