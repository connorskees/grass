use std::fmt::{self, Display};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Op {
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
    Plus,
    Minus,
    Mul,
    Div,
    Rem,
    And,
    Or,
    Not,
}

impl Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Equal => write!(f, "=="),
            Self::NotEqual => write!(f, "!="),
            Self::GreaterThanEqual => write!(f, ">="),
            Self::LessThanEqual => write!(f, "<="),
            Self::GreaterThan => write!(f, ">"),
            Self::LessThan => write!(f, "<"),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Rem => write!(f, "%"),
            Self::And => write!(f, "and"),
            Self::Or => write!(f, "or"),
            Self::Not => write!(f, "not"),
        }
    }
}

impl Op {
    /// Get order of precedence for an operator
    ///
    /// Higher numbers are evaluated first.
    /// Do not rely on the number itself, but rather the size relative to other numbers
    ///
    /// If precedence is equal, the leftmost operation is evaluated first
    pub fn precedence(self) -> usize {
        match self {
            Self::And | Self::Or | Self::Not => 0,
            Self::Equal
            | Self::NotEqual
            | Self::GreaterThan
            | Self::GreaterThanEqual
            | Self::LessThan
            | Self::LessThanEqual => 1,
            Self::Plus | Self::Minus => 2,
            Self::Mul | Self::Div | Self::Rem => 3,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Pos {
    line: u32,
    column: u32,
}

impl Pos {
    pub const fn new() -> Self {
        Pos { line: 1, column: 1 }
    }

    pub const fn line(self) -> u32 {
        self.line
    }

    pub const fn column(self) -> u32 {
        self.column
    }

    pub fn newline(&mut self) {
        self.line += 1;
        self.column = 0;
    }

    pub fn next_char(&mut self) {
        self.column += 1;
    }

    pub fn chars(&mut self, num: u32) {
        self.column += num;
    }
}

impl Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line:{} col:{}", self.line, self.column)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) enum QuoteKind {
    Single,
    Double,
    None,
}

impl QuoteKind {
    /// SASS will prefer double quotes over single quotes after
    /// operations, e.g. `'foo' + red` => `"foored"`
    pub fn normalize(self) -> QuoteKind {
        match self {
            QuoteKind::Double | QuoteKind::Single => QuoteKind::Double,
            QuoteKind::None => QuoteKind::None,
        }
    }
}

impl Display for QuoteKind {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Single => write!(f, "'"),
            Self::Double => write!(f, "\""),
            Self::None => write!(f, ""),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Brackets {
    None,
    Bracketed,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ListSeparator {
    Space,
    Comma,
}

impl ListSeparator {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Space => " ",
            Self::Comma => ", ",
        }
    }

    pub fn name(self) -> &'static str {
        match self {
            Self::Space => "space",
            Self::Comma => "comma",
        }
    }
}

impl Display for ListSeparator {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Space => write!(f, " "),
            Self::Comma => write!(f, ", "),
        }
    }
}
