use crate::common::Whitespace;
use crate::{Scope, Token, TokenKind};
use std::iter::Iterator;
use std::iter::Peekable;

pub trait IsWhitespace {
    fn is_whitespace(&self) -> bool;
}

pub fn devour_whitespace<I: Iterator<Item = W>, W: IsWhitespace>(s: &mut Peekable<I>) -> bool {
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

pub fn deref_variable(name: &str, scope: &Scope) -> Vec<Token> {
    let mut toks = scope
        .vars
        .get(name)
        .expect("todo! expected variable to exist")
        .iter()
        .peekable();
    let mut val = Vec::with_capacity(toks.len());
    while let Some(tok) = toks.next() {
        match &tok.kind {
            TokenKind::Variable(ref v) => val.extend(deref_variable(v, scope)),
            TokenKind::Whitespace(_) => {
                devour_whitespace(&mut toks);
                if toks.peek().is_some() {
                    val.push(Token {
                        kind: TokenKind::Whitespace(Whitespace::Space),
                        pos: tok.pos,
                    });
                }
            }
            _ => val.push(tok.clone()),
        }
    }
    val
}
