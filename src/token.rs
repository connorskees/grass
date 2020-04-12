use crate::utils::IsWhitespace;

use codemap::Span;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) struct Token {
    pub pos: Span,
    pub kind: char,
}

impl Token {
    pub const fn new(pos: Span, kind: char) -> Self {
        Self { pos, kind }
    }

    pub const fn pos(&self) -> Span {
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
