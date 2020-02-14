use crate::common::{Keyword, Pos, Symbol};
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

pub(crate) fn parse_interpolation<I: Iterator<Item = Token>>(
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
            TokenKind::Variable(ref v) => val
                .extend(Lexer::new(&scope.get_var(v).unwrap().to_string()).collect::<Vec<Token>>()),
            TokenKind::Interpolation => val.extend(parse_interpolation(tokens, scope)),
            _ => val.push(tok),
        }
    }
    Lexer::new(
        &Value::from_tokens(&mut val.into_iter().peekable(), scope)
            .unwrap()
            .unquote()
            .to_string()
    )
    .collect::<Vec<Token>>()
}

pub(crate) struct VariableDecl {
    pub val: Value,
    pub default: bool,
}

impl VariableDecl {
    pub const fn new(val: Value, default: bool) -> VariableDecl {
        VariableDecl { val, default }
    }
}

pub(crate) fn eat_variable_value<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    scope: &Scope,
) -> Result<VariableDecl, (Pos, String)> {
    devour_whitespace(toks);
    let mut default = false;
    let mut raw: Vec<Token> = Vec::new();
    let mut nesting = 0;
    while let Some(tok) = toks.peek() {
        match tok.kind {
            TokenKind::Symbol(Symbol::SemiColon) => {
                toks.next();
                break;
            }
            TokenKind::Keyword(Keyword::Default) => {
                toks.next();
                default = true
            }
            TokenKind::Interpolation | TokenKind::Symbol(Symbol::OpenCurlyBrace) => {
                nesting += 1;
                raw.push(toks.next().unwrap());
            }
            TokenKind::Symbol(Symbol::CloseCurlyBrace) => {
                if nesting == 0 {
                    break;
                } else {
                    nesting -= 1;
                    raw.push(toks.next().unwrap());
                }
            }
            _ => raw.push(toks.next().unwrap()),
        }
    }
    devour_whitespace(toks);
    let val = Value::from_tokens(&mut raw.into_iter().peekable(), scope).unwrap();
    Ok(VariableDecl::new(val, default))
}
