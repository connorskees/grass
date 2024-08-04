use std::{
    borrow::Cow,
    cell::RefCell,
    collections::BTreeMap,
    path::{Path, PathBuf},
};

use grass::{Fs, Logger};
use grass_compiler::codemap::SpanLoc;

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
    (@base $( #[$attr:meta] ),*$func:ident, $input:expr, $err:expr, $options:expr) => {
        $(#[$attr])*
        #[test]
        #[allow(non_snake_case)]
        fn $func() {
            match grass::from_string($input.to_string(), &$options) {
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
    ($( #[$attr:meta] ),*$func:ident, $input:expr, $err:expr) => {
        error!(@base $(#[$attr])* $func, $input, $err, grass::Options::default());
    };
    ($( #[$attr:meta] ),*$func:ident, $input:expr, $err:expr, $options:expr) => {
        error!(@base $(#[$attr])* $func, $input, $err, $options);
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
        let _d = if !std::path::Path::new($dir).is_dir() {
            Some(
                tempfile::Builder::new()
                    .rand_bytes(0)
                    .prefix("")
                    .suffix($dir)
                    .tempdir_in("")
                    .unwrap(),
            )
        } else {
            None
        };
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
    ($input:expr, $err:expr, $options:expr) => {
        match grass::from_string($input.to_string(), &$options) {
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

/// Suitable for simple import tests. Does not properly implement path resolution --
/// paths like `a/../b` will not work
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

#[derive(Debug, Default)]
struct TestLoggerState {
    debug_messages: Vec<String>,
    warning_messages: Vec<String>,
}

#[derive(Debug, Default)]
pub struct TestLogger(RefCell<TestLoggerState>);

#[allow(unused)]
impl TestLogger {
    pub fn debug_messages(&self) -> Vec<String> {
        self.0.borrow().debug_messages.clone()
    }

    pub fn warning_messages(&self) -> Vec<String> {
        self.0.borrow().warning_messages.clone()
    }
}

impl Logger for TestLogger {
    fn debug(&self, _location: SpanLoc, message: &str) {
        self.0.borrow_mut().debug_messages.push(message.into());
    }

    fn warn(&self, _location: SpanLoc, message: &str) {
        self.0.borrow_mut().warning_messages.push(message.into());
    }
}
