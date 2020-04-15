use std::error::Error;
use std::fmt::{self, Display};
use std::io;
use std::string::FromUtf8Error;

use codemap::{Span, SpanLoc};

pub type SassResult<T> = Result<T, SassError>;

#[derive(Debug)]
pub struct SassError {
    kind: SassErrorKind,
}

impl SassError {
    pub(crate) fn raw(self) -> (String, Span) {
        match self.kind {
            SassErrorKind::Raw(string, span) => (string, span),
            _ => todo!(),
        }
    }

    pub(crate) fn from_loc(message: String, loc: SpanLoc) -> Self {
        SassError {
            kind: SassErrorKind::ParseError { message, loc },
        }
    }
}

#[derive(Debug)]
enum SassErrorKind {
    /// A raw error with no additional metadata
    /// It contains only a `String` message and
    /// a span
    Raw(String, Span),
    ParseError {
        message: String,
        loc: SpanLoc,
    },
    IoError(io::Error),
    FmtError(fmt::Error),
    FromUtf8Error(String),
}

impl Display for SassError {
    // TODO: trim whitespace from start of line shown in error
    // TODO: color errors
    // TODO: integrate with codemap-diagnostics
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (message, loc) = match &self.kind {
            SassErrorKind::ParseError { message, loc } => (message, loc),
            _ => todo!(),
        };
        let line = loc.begin.line + 1;
        let col = loc.begin.column + 1;
        writeln!(f, "Error: {}", message)?;
        let padding = vec![' '; format!("{}", line).len() + 1]
            .iter()
            .collect::<String>();
        writeln!(f, "{}|", padding)?;
        writeln!(f, "{} | {}", line, loc.file.source_line(loc.begin.line))?;
        writeln!(
            f,
            "{}| {}{}",
            padding,
            vec![' '; loc.begin.column].iter().collect::<String>(),
            vec!['^'; loc.end.column - loc.begin.column]
                .iter()
                .collect::<String>()
        )?;
        writeln!(f, "{}|", padding)?;
        writeln!(f, "./{}:{}:{}", loc.file.name(), line, col)?;
        Ok(())
    }
}

impl From<io::Error> for SassError {
    fn from(error: io::Error) -> Self {
        SassError {
            kind: SassErrorKind::IoError(error),
        }
    }
}

impl From<std::fmt::Error> for SassError {
    fn from(error: std::fmt::Error) -> Self {
        SassError {
            kind: SassErrorKind::FmtError(error),
        }
    }
}

impl From<FromUtf8Error> for SassError {
    fn from(error: FromUtf8Error) -> Self {
        SassError {
            kind: SassErrorKind::FromUtf8Error(format!(
                "Invalid UTF-8 character \"\\x{:X?}\"",
                error.as_bytes()[0]
            )),
        }
    }
}

impl From<(&str, Span)> for SassError {
    #[inline]
    fn from(error: (&str, Span)) -> SassError {
        SassError {
            kind: SassErrorKind::Raw(error.0.to_owned(), error.1),
        }
    }
}

impl From<(String, Span)> for SassError {
    #[inline]
    fn from(error: (String, Span)) -> SassError {
        SassError {
            kind: SassErrorKind::Raw(error.0, error.1),
        }
    }
}

impl Error for SassError {
    fn description(&self) -> &'static str {
        "SASS parsing error"
    }
}
