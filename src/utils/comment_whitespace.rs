use crate::{lexer::Lexer, Token};

use super::peek_until_newline;

pub(crate) trait IsWhitespace {
    fn is_whitespace(&self) -> bool;
}

impl IsWhitespace for char {
    fn is_whitespace(&self) -> bool {
        self.is_ascii_whitespace()
    }
}

pub(crate) fn devour_whitespace(s: &mut Lexer) -> bool {
    let mut found_whitespace = false;
    while let Some(w) = s.peek() {
        if !w.is_whitespace() {
            break;
        }
        found_whitespace = true;
        s.next();
    }
    found_whitespace
}

pub(crate) fn peek_whitespace(s: &mut Lexer) -> bool {
    let mut found_whitespace = false;
    while let Some(w) = s.peek() {
        if !w.is_whitespace() {
            break;
        }
        found_whitespace = true;
        s.advance_cursor();
    }
    found_whitespace
}

pub(crate) fn peek_whitespace_or_comment(s: &mut Lexer) -> bool {
    let mut found_whitespace = false;
    while let Some(w) = s.peek() {
        match w.kind {
            ' ' | '\t' | '\n' => {
                found_whitespace = true;
                s.advance_cursor();
            }
            '/' => match s.peek_next() {
                Some(Token { kind: '/', .. }) => {
                    peek_until_newline(s);
                    found_whitespace = true;
                }
                Some(Token { kind: '*', .. }) => {
                    found_whitespace = true;
                    while let Some(tok) = s.peek_next() {
                        match tok.kind {
                            '*' => {
                                if matches!(s.peek_next(), Some(Token { kind: '/', .. })) {
                                    s.advance_cursor();
                                    break;
                                }
                            }
                            _ => continue,
                        }
                    }
                }
                _ => return found_whitespace,
            },
            _ => return found_whitespace,
        }
    }
    found_whitespace
}

/// Eat tokens until a newline
///
/// This exists largely to eat silent comments, "//"
/// We only have to check for \n as the lexing step normalizes all newline characters
///
/// The newline is consumed
pub(crate) fn read_until_newline(toks: &mut Lexer) {
    for tok in toks {
        if tok.kind == '\n' {
            return;
        }
    }
}
