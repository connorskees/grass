use std::fmt;

use crate::atrule::AtRuleKind;
use crate::common::{Keyword, Op, Pos, Symbol, Whitespace};
use crate::utils::{IsComment, IsWhitespace};

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Token {
    pub pos: Pos,
    pub kind: TokenKind,
}

impl Token {
    pub fn is_symbol(&self, s: Symbol) -> bool {
        self.kind.is_symbol(s)
    }

    pub fn from_string(s: String) -> Self {
        Token {
            kind: TokenKind::Ident(s),
            pos: Pos::new(),
        }
    }

    pub fn from_symbol(s: Symbol) -> Self {
        Token {
            kind: TokenKind::Symbol(s),
            pos: Pos::new(),
        }
    }

    pub const fn pos(&self) -> Pos {
        self.pos
    }
}

impl IsWhitespace for Token {
    fn is_whitespace(&self) -> bool {
        if let TokenKind::Whitespace(_) = self.kind {
            return true;
        }
        false
    }
}

impl IsWhitespace for &Token {
    fn is_whitespace(&self) -> bool {
        if let TokenKind::Whitespace(_) = self.kind {
            return true;
        }
        false
    }
}

impl IsComment for Token {
    fn is_comment(&self) -> bool {
        if let TokenKind::MultilineComment(_) = self.kind {
            return true;
        }
        false
    }
}

impl IsComment for &Token {
    fn is_comment(&self) -> bool {
        if let TokenKind::MultilineComment(_) = self.kind {
            return true;
        }
        false
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum TokenKind {
    Ident(String),
    Symbol(Symbol),
    AtRule(AtRuleKind),
    Keyword(Keyword),
    Number(String),
    Whitespace(Whitespace),
    Variable(String),
    Op(Op),
    MultilineComment(String),
    Interpolation,
    Unknown(char),
}

impl TokenKind {
    pub fn is_symbol(&self, s: Symbol) -> bool {
        self == &TokenKind::Symbol(s)
    }
}

impl fmt::Display for TokenKind {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Ident(s) | TokenKind::Number(s) => write!(f, "{}", s),
            TokenKind::Symbol(s) => write!(f, "{}", s),
            TokenKind::AtRule(s) => write!(f, "{}", s),
            TokenKind::Op(s) => write!(f, "{}", s),
            TokenKind::Whitespace(s) => write!(f, "{}", s),
            TokenKind::Keyword(kw) => write!(f, "{}", kw),
            TokenKind::MultilineComment(s) => write!(f, "/*{}*/", s),
            TokenKind::Variable(s) => write!(f, "{}", s),
            TokenKind::Unknown(s) => write!(f, "{}", s),
            TokenKind::Interpolation => {
                panic!("we don't want to format TokenKind::Interpolation using Display")
            }
        }
    }
}
