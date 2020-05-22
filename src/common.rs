use crate::interner::InternedString;
use std::fmt::{self, Display, Write};

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

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) enum QuoteKind {
    Quoted,
    None,
}

impl Display for QuoteKind {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Quoted => f.write_char('"'),
            Self::None => Ok(()),
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct QualifiedName {
    pub ident: String,
    pub namespace: Option<String>,
}

impl Display for QualifiedName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(namespace) = &self.namespace {
            write!(f, "{}|", namespace)?;
        }
        f.write_str(&self.ident)
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq, Copy)]
pub(crate) struct Identifier(InternedString);

impl Into<Identifier> for InternedString {
    fn into(self) -> Identifier {
        Identifier(InternedString::get_or_intern(
            self.resolve_ref().replace('_', "-"),
        ))
    }
}

impl From<String> for Identifier {
    fn from(s: String) -> Identifier {
        Identifier(InternedString::get_or_intern(s.replace('_', "-")))
    }
}

impl Into<Identifier> for &String {
    fn into(self) -> Identifier {
        Identifier(InternedString::get_or_intern(self.replace('_', "-")))
    }
}

impl Into<Identifier> for &str {
    fn into(self) -> Identifier {
        Identifier(InternedString::get_or_intern(self.replace('_', "-")))
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
