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
