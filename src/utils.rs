use crate::common::{Pos, Symbol};
use crate::lexer::Lexer;
use crate::value::Value;
use crate::{Scope, Token, TokenKind};
use std::iter::{Iterator, Peekable};

pub(crate) trait IsWhitespace {
    fn is_whitespace(&self) -> bool;
}

pub(crate) fn devour_whitespace<I: Iterator<Item = W>, W: IsWhitespace>(
    s: &mut Peekable<I>,
) -> bool {
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

pub(crate) trait IsComment {
    fn is_comment(&self) -> bool;
}

pub(crate) fn devour_whitespace_or_comment<I: Iterator<Item = W>, W: IsWhitespace + IsComment>(
    s: &mut Peekable<I>,
) -> bool {
    let mut found_whitespace = false;
    while let Some(w) = s.peek() {
        if !w.is_whitespace() && !w.is_comment() {
            break;
        }
        found_whitespace = true;
        s.next();
    }
    found_whitespace
}

pub(crate) fn eat_interpolation<I: Iterator<Item = Token>>(
    tokens: &mut I,
    scope: &Scope,
) -> Vec<Token> {
    let mut val = Vec::new();
    while let Some(tok) = tokens.next() {
        match tok.kind {
            TokenKind::Symbol(Symbol::CloseCurlyBrace) => break,
            TokenKind::Symbol(Symbol::OpenCurlyBrace) => {
                todo!("invalid character in interpolation")
            }
            TokenKind::Variable(ref v) => val.extend(
                Lexer::new(&scope.vars.get(v).unwrap().to_string()).collect::<Vec<Token>>(),
            ),
            TokenKind::Interpolation => val.extend(eat_interpolation(tokens, scope)),
            _ => val.push(tok),
        }
    }
    val
}

pub(crate) fn eat_variable_value<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    scope: &Scope,
) -> Result<Value, (Pos, String)> {
    devour_whitespace(toks);
    // todo!(line might not end with semicolon)
    let iter1: Vec<Token> = toks
        .take_while(|x| x.kind != TokenKind::Symbol(Symbol::SemiColon))
        .collect();
    devour_whitespace(toks);
    Ok(Value::from_tokens(&mut iter1.into_iter().peekable(), scope).unwrap())
}
