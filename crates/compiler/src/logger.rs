use codemap::SpanLoc;
use std::fmt::Debug;

/// Sink for log messages
pub trait Logger: Debug {
    /// Logs message from a `@debug` statement
    fn debug(&self, location: SpanLoc, message: &str);

    /// Logs message from a `@warn` statement
    fn warning(&self, location: SpanLoc, message: &str);
}

/// Logs events to standard error
#[derive(Debug)]
pub struct StdLogger;

impl Logger for StdLogger {
    #[inline]
    fn debug(&self, location: SpanLoc, message: &str) {
        eprintln!(
            "{}:{} DEBUG: {}",
            location.file.name(),
            location.begin.line + 1,
            message
        );
    }

    #[inline]
    fn warning(&self, location: SpanLoc, message: &str) {
        eprintln!(
            "Warning: {}\n    ./{}:{}:{}",
            message,
            location.file.name(),
            location.begin.line + 1,
            location.begin.column + 1
        );
    }
}

/// Discards all log events
#[derive(Debug)]
pub struct NullLogger;

impl Logger for NullLogger {
    #[inline]
    fn debug(&self, _location: SpanLoc, _message: &str) {}

    #[inline]
    fn warning(&self, _location: SpanLoc, _message: &str) {}
}
