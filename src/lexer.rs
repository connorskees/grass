use std::iter::Peekable;
use std::str::Chars;

use crate::common::Pos;
use crate::Token;

pub const FORM_FEED: char = '\x0C';

#[derive(Debug, Clone)]
pub(crate) struct Lexer<'a> {
    buf: Peekable<Chars<'a>>,
    pos: Pos,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        let kind = match self.buf.next()? {
            '\n' | FORM_FEED => {
                self.pos.newline();
                '\n'
            }
            '\r' => {
                if self.buf.peek() == Some(&'\n') {
                    self.buf.next();
                    '\n'
                } else {
                    '\n'
                }
            }
            '\0' => return None,
            c => c,
        };
        self.pos.next_char();
        Some(Token {
            kind,
            pos: self.pos,
        })
    }
}

impl<'a> Lexer<'a> {
    pub fn new(buf: &'a str) -> Lexer<'a> {
        Lexer {
            buf: buf.chars().peekable(),
            pos: Pos::new(),
        }
    }
}
