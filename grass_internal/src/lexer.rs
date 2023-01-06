use std::{borrow::Cow, iter::Peekable, str::Chars, sync::Arc};

use codemap::{File, Span};

use crate::Token;

const FORM_FEED: char = '\x0C';

#[derive(Debug, Clone)]
// todo: remove lifetime as Cow is now superfluous
pub(crate) struct Lexer<'a> {
    buf: Cow<'a, [Token]>,
    /// The span to be used in the case that `buf` is empty
    empty_span: Span,
    cursor: usize,
}

impl<'a> Lexer<'a> {
    pub fn raw_text(&self, start: usize) -> String {
        self.buf[start..self.cursor]
            .iter()
            .map(|t| t.kind)
            .collect()
    }

    pub fn next_char_is(&self, c: char) -> bool {
        matches!(self.peek(), Some(Token { kind, .. }) if kind == c)
    }

    pub fn span_from(&mut self, start: usize) -> Span {
        let start = match self.buf.get(start) {
            Some(start) => start.pos,
            None => return self.current_span(),
        };
        self.cursor = self.cursor.saturating_sub(1);
        let end = self.current_span();
        self.cursor += 1;

        start.merge(end)
    }

    pub fn prev_span(&self) -> Span {
        match self.buf.get(self.cursor.saturating_sub(1)) {
            Some(tok) => tok.pos,
            None => match self.buf.last() {
                Some(tok) => tok.pos,
                None => self.empty_span,
            },
        }
    }

    pub fn current_span(&self) -> Span {
        match self.buf.get(self.cursor) {
            Some(tok) => tok.pos,
            None => match self.buf.last() {
                Some(tok) => tok.pos,
                None => self.empty_span,
            },
        }
    }

    pub fn peek(&self) -> Option<Token> {
        self.buf.get(self.cursor).copied()
    }

    /// Peeks the previous token without modifying the peek cursor
    pub fn peek_previous(&mut self) -> Option<Token> {
        self.buf.get(self.cursor.checked_sub(1)?).copied()
    }

    /// Peeks `n` from current peeked position without modifying cursor
    pub fn peek_n(&self, n: usize) -> Option<Token> {
        self.buf.get(self.cursor + n).copied()
    }

    /// Peeks `n` behind current peeked position without modifying cursor
    pub fn peek_n_backwards(&self, n: usize) -> Option<Token> {
        self.buf.get(self.cursor.checked_sub(n)?).copied()
    }

    /// Set cursor to position and reset peek
    pub fn set_cursor(&mut self, cursor: usize) {
        self.cursor = cursor;
    }

    pub fn cursor(&self) -> usize {
        self.cursor
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.buf.get(self.cursor).copied().map(|tok| {
            self.cursor += 1;
            tok
        })
    }
}

struct TokenLexer<'a> {
    buf: Peekable<Chars<'a>>,
    cursor: usize,
    file: Arc<File>,
}

impl<'a> Iterator for TokenLexer<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        let kind = match self.buf.next()? {
            FORM_FEED => '\n',
            '\r' => {
                if self.buf.peek() == Some(&'\n') {
                    self.cursor += 1;
                    self.buf.next();
                }
                '\n'
            }
            c => c,
        };
        let len = kind.len_utf8();
        let pos = self
            .file
            .span
            .subspan(self.cursor as u64, (self.cursor + len) as u64);
        self.cursor += len;
        Some(Token { pos, kind })
    }
}

impl<'a> Lexer<'a> {
    pub fn new_from_file(file: &Arc<File>) -> Self {
        let buf = TokenLexer {
            file: Arc::clone(file),
            buf: file.source().chars().peekable(),
            cursor: 0,
        }
        .collect();

        Self::new(buf, file.span.subspan(0, 0))
    }

    pub fn new(buf: Vec<Token>, empty_span: Span) -> Self {
        Lexer {
            buf: Cow::Owned(buf),
            cursor: 0,
            empty_span,
        }
    }
}
