use std::fmt::{self, Display};

/// The selector namespace.
///
/// If this is `None`, this matches all elements in the default namespace. If
/// it's `Empty`, this matches all elements that aren't in any
/// namespace. If it's `Asterisk`, this matches all elements in any namespace.
/// Otherwise, it matches all elements in the given namespace.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum Namespace {
    Empty,
    Asterisk,
    Other(String),
    None,
}

impl Display for Namespace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty => write!(f, "|"),
            Self::Asterisk => write!(f, "*|"),
            Self::Other(namespace) => write!(f, "{}|", namespace),
            Self::None => Ok(()),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct QualifiedName {
    pub ident: String,
    pub namespace: Namespace,
}

impl Display for QualifiedName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.namespace)?;
        f.write_str(&self.ident)
    }
}

pub(crate) struct Specificity(u32, u32);

impl Specificity {
    pub const fn new(min: u32, max: u32) -> Self {
        Specificity(min, max)
    }

    pub const fn min(&self) -> u32 {
        self.0
    }

    pub const fn max(&self) -> u32 {
        self.1
    }
}
