use crate::common::{Pos, Symbol, Whitespace};
use crate::{Scope, Token, TokenKind};
use std::iter::{Iterator, Peekable};

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

#[track_caller]
pub fn deref_variable(name: &str, scope: &Scope) -> Vec<Token> {
    let mut toks = scope
        .vars
        .get(name)
        .expect("todo! expected variable to exist")
        .iter()
        .peekable();
    let mut val = Vec::with_capacity(toks.len());
    while let Some(tok) = toks.next() {
        match tok.kind {
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

pub fn eat_interpolation<I: Iterator<Item = Token>>(
    tokens: &mut Peekable<I>,
    scope: &Scope,
) -> Vec<Token> {
    let mut val = Vec::new();
    for tok in tokens {
        match tok.kind {
            TokenKind::Symbol(Symbol::CloseCurlyBrace) => break,
            TokenKind::Symbol(Symbol::OpenCurlyBrace) => {
                todo!("invalid character in interpolation")
            }
            TokenKind::Variable(ref v) => val.extend(deref_variable(v, scope)),
            _ => val.push(tok),
        }
    }
    val
}

pub fn eat_variable_value<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    scope: &Scope,
) -> Result<Vec<Token>, (Pos, String)> {
    devour_whitespace(toks);
    // todo!(line might not end with semicolon)
    let iter1 = toks.take_while(|x| x.kind != TokenKind::Symbol(Symbol::SemiColon));
    let mut iter2 = Vec::new();
    for tok in iter1 {
        match tok.kind {
            TokenKind::Variable(ref name) => iter2.extend(deref_variable(name, scope)),
            _ => iter2.push(tok),
        };
    }
    devour_whitespace(toks);
    Ok(iter2)
}
