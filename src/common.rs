use std::fmt::{self, Display, Write};

use crate::interner::InternedString;

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

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum Brackets {
    None,
    Bracketed,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

/// In Sass, underscores and hyphens are considered equal
/// when inside identifiers.
///
/// This struct protects that invariant by normalizing all
/// underscores into hypens.
#[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub(crate) struct Identifier(InternedString);

impl From<String> for Identifier {
    fn from(s: String) -> Identifier {
        Identifier(InternedString::get_or_intern(if s.contains('_') {
            s.replace('_', "-")
        } else {
            s
        }))
    }
}

impl From<&String> for Identifier {
    fn from(s: &String) -> Identifier {
        if s.contains('_') {
            Identifier(InternedString::get_or_intern(s.replace('_', "-")))
        } else {
            Identifier(InternedString::get_or_intern(s))
        }
    }
}

impl From<&str> for Identifier {
    fn from(s: &str) -> Identifier {
        if s.contains('_') {
            Identifier(InternedString::get_or_intern(s.replace('_', "-")))
        } else {
            Identifier(InternedString::get_or_intern(s))
        }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Identifier {
    pub fn as_str(&self) -> &str {
        self.0.resolve_ref()
    }
}

/// Returns `name` without a vendor prefix.
///
/// If `name` has no vendor prefix, it's returned as-is.
pub(crate) fn unvendor(name: &str) -> &str {
    let bytes = name.as_bytes();

    if bytes.len() < 2 {
        return name;
    }

    if bytes.get(0_usize) != Some(&b'-') || bytes.get(1_usize) == Some(&b'-') {
        return name;
    }

    for i in 2..bytes.len() {
        if bytes.get(i) == Some(&b'-') {
            return &name[i + 1..];
        }
    }

    name
}
