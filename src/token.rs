use codemap::Span;

// todo: remove span from tokens

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
