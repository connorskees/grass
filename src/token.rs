use crate::common::Pos;
use crate::utils::IsWhitespace;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) struct Token {
    pub pos: Pos,
    pub kind: char,
}

impl Token {
    pub const fn new(pos: Pos, kind: char) -> Self {
        Self { pos, kind }
    }

    pub const fn pos(&self) -> Pos {
        self.pos
    }
}

impl IsWhitespace for Token {
    fn is_whitespace(&self) -> bool {
        if self.kind.is_whitespace() {
            return true;
        }
        false
    }
}

impl IsWhitespace for &Token {
    fn is_whitespace(&self) -> bool {
        if self.kind.is_whitespace() {
            return true;
        }
        false
    }
}

// impl IsComment for Token {
//     fn is_comment(&self) -> bool {
//         if let TokenKind::MultilineComment(_) = self.kind {
//             return true;
//         }
//         false
//     }
// }

// impl IsComment for &Token {
//     fn is_comment(&self) -> bool {
//         if let TokenKind::MultilineComment(_) = self.kind {
//             return true;
//         }
//         false
//     }
// }

// #[derive(Clone, Debug, Eq, PartialEq)]
// pub(crate) enum TokenKind {
//     Ident(String),
//     Symbol(Symbol),
//     AtRule(AtRuleKind),
//     Keyword(Keyword),
//     Number(String),
//     Whitespace(Whitespace),
//     Variable(String),
//     Op(Op),
//     MultilineComment(String),
//     Interpolation,
//     Error(SassError),
// }

// impl TokenKind {
//     pub fn is_symbol(&self, s: Symbol) -> bool {
//         self == &TokenKind::Symbol(s)
//     }
// }

// impl fmt::Display for TokenKind {
//     #[inline]
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             TokenKind::Ident(s) | TokenKind::Number(s) => write!(f, "{}", s),
//             TokenKind::Symbol(s) => write!(f, "{}", s),
//             TokenKind::AtRule(s) => write!(f, "{}", s),
//             TokenKind::Op(s) => write!(f, "{}", s),
//             TokenKind::Whitespace(s) => write!(f, "{}", s),
//             TokenKind::Keyword(kw) => write!(f, "{}", kw),
//             TokenKind::MultilineComment(s) => write!(f, "/*{}*/", s),
//             TokenKind::Variable(s) => write!(f, "{}", s),
//             TokenKind::Interpolation | TokenKind::Error(..) => {
//                 panic!("we don't want to format TokenKind::Interpolation using Display")
//             }
//         }
//     }
// }
