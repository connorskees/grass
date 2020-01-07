use crate::common::Pos;
use std::error::Error;
use std::fmt::{self, Display};
use std::io;

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

    pub fn unexpected_eof(pos: Pos) -> Self {
        SassError {
            message: String::from("unexpected eof"),
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

impl Error for SassError {
    fn description(&self) -> &'static str {
        "SASS parsing error"
    }
}
