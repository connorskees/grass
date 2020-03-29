use std::iter::Peekable;
use std::str::Chars;
use std::sync::atomic::{AtomicBool, Ordering};

use crate::common::Pos;
use crate::Token;

pub static IS_UTF8: AtomicBool = AtomicBool::new(false);
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
            // c if c.is_control() => {
            //     return Some(Err("Expected expression.".into()))
            // }
            c if !c.is_ascii() => {
                IS_UTF8.store(true, Ordering::Relaxed);
                c
            }
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
    /*
    fn lex_exclamation(&mut self) -> TokenKind {
        self.buf.next();
        self.pos.next_char();
        macro_rules! assert_char {
            ($self:ident, $($char:literal)*) => {
                $(
                    assert_eq!($char, $self.buf.next().expect("expected char").to_ascii_lowercase(), "expected keyword `important`");
                )*
            }
        };
        match self.buf.peek() {
            Some('i') | Some('I') => {
                self.buf.next();
                assert_char!(self, 'm' 'p' 'o' 'r' 't' 'a' 'n' 't');
                TokenKind::Keyword(Keyword::Important)
            }
            Some('d') | Some('D') => {
                self.buf.next();
                assert_char!(self, 'e' 'f' 'a' 'u' 'l' 't');
                TokenKind::Keyword(Keyword::Default)
            }
            Some('g') | Some('G') => {
                self.buf.next();
                assert_char!(self, 'l' 'o' 'b' 'a' 'l');
                TokenKind::Keyword(Keyword::Global)
            }
            Some('=') => {
                self.buf.next();
                TokenKind::Op(Op::NotEqual)
            }
            _ => todo!("expected either `i` or `=` after `!`"),
        }
    }


    fn lex_back_slash(&mut self) -> (TokenKind, bool) {
        self.buf.next();
        self.pos.next_char();
        if self.buf.peek() == Some(&'\\') {
            self.buf.next();
            self.pos.next_char();
            self.should_emit_backslash = 1;
            (TokenKind::Symbol(Symbol::BackSlash), true)
        } else {
            let mut n = String::new();
            while let Some(c) = self.buf.peek() {
                if !c.is_ascii_hexdigit() || n.len() > 6 {
                    break;
                }
                n.push(*c);
                self.buf.next();
                self.pos.next_char();
            }

            if n.is_empty() {
                return (TokenKind::Symbol(Symbol::BackSlash), false);
            } else if n.len() == 1 {
                return (TokenKind::Ident(format!("\\{} ", n)), false);
            }

            let mut string = std::char::from_u32(u32::from_str_radix(&n, 16).unwrap())
                .unwrap()
                .to_string();
            self.devour_whitespace();
            if let TokenKind::Ident(s) = self.lex_ident() {
                string.push_str(&s);
            }
            (TokenKind::Ident(string), false)
        }
    }

    fn lex_hash(&mut self) -> TokenKind {
        self.buf.next();
        self.pos.next_char();
        if self.buf.peek() == Some(&'{') {
            self.buf.next();
            self.pos.next_char();
            return TokenKind::Interpolation;
        }
        TokenKind::Symbol(Symbol::Hash)
    }

    fn lex_ident(&mut self) -> TokenKind {
        let mut string = String::with_capacity(99);
        while let Some(c) = self.buf.peek() {
            if !c.is_alphanumeric() && c != &'-' && c != &'_' && c != &'\\' && c.is_ascii() {
                break;
            }
            if !c.is_ascii() {
                IS_UTF8.store(true, Ordering::Relaxed);
            }
            if c == &'\\' {
                match self.lex_back_slash() {
                    (TokenKind::Ident(s), _) => string.push_str(&s),
                    (TokenKind::Symbol(..), true) => {
                        self.should_emit_backslash = 2;
                        break;
                    }
                    (TokenKind::Symbol(..), false) => {
                        self.should_emit_backslash = 1;
                        break;
                    }
                    _ => unreachable!(),
                }
                continue;
            }
            let tok = self
                .buf
                .next()
                .expect("this is impossible because we have already peeked");
            self.pos.next_char();
            string.push(tok);
        }

        if let Ok(kw) = Keyword::try_from(string.as_ref()) {
            return TokenKind::Keyword(kw);
        }

        if string == "-" {
            return TokenKind::Symbol(Symbol::Minus);
        }

        TokenKind::Ident(string)
    }
    */
}
