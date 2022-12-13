use std::{borrow::Cow, iter::Peekable, str::Chars, sync::Arc};

use codemap::{File, Span};

use crate::Token;

const FORM_FEED: char = '\x0C';

#[derive(Debug, Clone)]
pub(crate) struct Lexer<'a> {
    buf: Cow<'a, [Token]>,
    cursor: usize,
    amt_peeked: usize,
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

    pub fn span_from(&self, start: usize) -> Span {
        let start = self.buf[start].pos;
        let end = self.current_span();

        start.merge(end)
    }

    pub fn current_span(&self) -> Span {
        self.buf
            .get(self.cursor)
            .copied()
            .unwrap_or(self.buf.last().copied().unwrap())
            .pos
    }

    fn peek_cursor(&self) -> usize {
        self.cursor + self.amt_peeked
    }

    pub fn peek(&self) -> Option<Token> {
        self.buf.get(self.peek_cursor()).copied()
    }

    pub fn reset_cursor(&mut self) {
        self.amt_peeked = 0;
    }

    pub fn peek_next(&mut self) -> Option<Token> {
        self.amt_peeked += 1;

        self.peek()
    }

    /// Peeks the previous token without modifying the peek cursor
    pub fn peek_previous(&mut self) -> Option<Token> {
        self.buf.get(self.peek_cursor().checked_sub(1)?).copied()
    }

    /// Peeks `n` from current peeked position, modifying the peek cursor
    pub fn peek_forward(&mut self, n: usize) -> Option<Token> {
        self.amt_peeked += n;

        self.peek()
    }

    /// Peeks `n` from current peeked position without modifying cursor
    pub fn peek_n(&self, n: usize) -> Option<Token> {
        self.buf.get(self.peek_cursor() + n).copied()
    }

    /// Peeks `n` behind current peeked position without modifying cursor
    pub fn peek_n_backwards(&self, n: usize) -> Option<Token> {
        self.buf.get(self.peek_cursor().checked_sub(n)?).copied()
    }

    pub fn peek_backward(&mut self, n: usize) -> Option<Token> {
        self.amt_peeked = self.amt_peeked.checked_sub(n)?;

        self.peek()
    }

    /// Set cursor to position and reset peek
    pub fn set_cursor(&mut self, cursor: usize) {
        self.cursor = cursor;
        self.amt_peeked = 0;
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
            self.amt_peeked = self.amt_peeked.saturating_sub(1);
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

        Self::new(buf)
    }

    pub fn new(buf: Vec<Token>) -> Self {
        Lexer {
            buf: Cow::Owned(buf),
            cursor: 0,
            amt_peeked: 0,
        }
    }

    pub fn new_ref(buf: &'a [Token]) -> Lexer<'a> {
        Lexer {
            buf: Cow::Borrowed(buf),
            cursor: 0,
            amt_peeked: 0,
        }
    }
}
