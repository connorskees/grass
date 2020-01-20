use crate::common::Pos;
use std::error::Error;
use std::fmt::{self, Display};
use std::io;
use std::string::FromUtf8Error;

#[derive(Debug)]
pub struct SassError {
    message: String,
    pos: Pos,
}

impl SassError {
    pub fn new<S: Into<String>>(message: S, pos: Pos) -> Self {
        SassError {
            message: message.into(),
            pos,
        }
    }
}

impl Display for SassError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error: {} at {}", self.message, self.pos)
    }
}

impl From<io::Error> for SassError {
    fn from(error: io::Error) -> Self {
        SassError {
            pos: Pos::new(),
            message: format!("{}", error),
        }
    }
}

impl From<FromUtf8Error> for SassError {
    fn from(error: FromUtf8Error) -> Self {
        SassError {
            pos: Pos::new(),
            message: format!("Invalid UTF-8 character \"\\x{:X?}\"", error.as_bytes()[0]),
        }
    }
}

impl From<SassError> for String {
    #[inline]
    fn from(error: SassError) -> String {
        error.message
    }
}

impl Error for SassError {
    fn description(&self) -> &'static str {
        "SASS parsing error"
    }
}
