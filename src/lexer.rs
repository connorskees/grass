use std::iter::Peekable;
use std::str::Chars;
use std::sync::Arc;

use codemap::File;

use crate::Token;

const FORM_FEED: char = '\x0C';

#[derive(Debug, Clone)]
pub(crate) struct Lexer<'a> {
    buf: Peekable<Chars<'a>>,
    pos: usize,
    file: &'a Arc<File>,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        let kind = match self.buf.next()? {
            FORM_FEED => '\n',
            '\r' => {
                if self.buf.peek() == Some(&'\n') {
                    self.pos += 1;
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
            .subspan(self.pos as u64, (self.pos + len) as u64);
        self.pos += len;
        Some(Token { kind, pos })
    }
}

impl<'a> Lexer<'a> {
    pub fn new(file: &'a Arc<File>) -> Lexer<'a> {
        Lexer {
            buf: file.source().chars().peekable(),
            pos: 0,
            file,
        }
    }
}
