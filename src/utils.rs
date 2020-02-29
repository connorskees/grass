use crate::common::{Keyword, QuoteKind, Symbol};
use crate::error::SassResult;
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
    tokens: &mut Peekable<I>,
    scope: &Scope,
) -> SassResult<Vec<Token>> {
    let mut val = String::new();
    while let Some(tok) = tokens.next() {
        match tok.kind {
            TokenKind::Symbol(Symbol::CloseCurlyBrace) => break,
            TokenKind::Symbol(Symbol::OpenCurlyBrace) => {
                todo!("invalid character in interpolation")
            }
            q @ TokenKind::Symbol(Symbol::DoubleQuote)
            | q @ TokenKind::Symbol(Symbol::SingleQuote) => {
                val.push_str(&parse_quoted_string(tokens, scope, &q)?.to_string())
            }
            TokenKind::Variable(ref v) => {
                val.push_str(&scope.get_var(v)?.clone().unquote().to_string())
            }
            TokenKind::Interpolation => val.push_str(
                &parse_interpolation(tokens, scope)?
                    .iter()
                    .map(|x| x.kind.to_string())
                    .collect::<String>(),
            ),
            _ => val.push_str(&tok.kind.to_string()),
        }
    }
    Ok(Lexer::new(
        &Value::from_tokens(&mut Lexer::new(&val).peekable(), scope)?
            .eval()?
            .unquote()
            .to_string(),
    )
    .collect())
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
) -> SassResult<VariableDecl> {
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

pub(crate) fn flatten_ident<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    scope: &Scope,
) -> SassResult<String> {
    let mut s = String::new();
    while let Some(tok) = toks.peek() {
        match tok.kind.clone() {
            TokenKind::Interpolation => {
                toks.next();
                s.push_str(
                    &parse_interpolation(toks, scope)?
                        .iter()
                        .map(|x| x.kind.to_string())
                        .collect::<String>(),
                )
            }
            TokenKind::Ident(ref i) => {
                toks.next();
                s.push_str(i)
            }
            TokenKind::Number(ref n) => {
                toks.next();
                s.push_str(n)
            }
            _ => break,
        }
    }
    Ok(s)
}

pub(crate) fn parse_quoted_string<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    scope: &Scope,
    q: &TokenKind,
) -> SassResult<Value> {
    let mut s = String::new();
    let mut is_escaped = false;
    let mut found_interpolation = false;
    while let Some(tok) = toks.next() {
        match tok.kind {
            TokenKind::Symbol(Symbol::DoubleQuote)
                if !is_escaped && q == &TokenKind::Symbol(Symbol::DoubleQuote) =>
            {
                break
            }
            TokenKind::Symbol(Symbol::DoubleQuote) if is_escaped => {
                s.push('"');
                is_escaped = false;
                continue;
            }
            TokenKind::Symbol(Symbol::SingleQuote)
                if !is_escaped && q == &TokenKind::Symbol(Symbol::SingleQuote) =>
            {
                break
            }
            TokenKind::Symbol(Symbol::SingleQuote) if is_escaped => {
                s.push('\'');
                is_escaped = false;
                continue;
            }
            TokenKind::Symbol(Symbol::BackSlash) if !is_escaped => is_escaped = true,
            TokenKind::Symbol(Symbol::BackSlash) => {
                is_escaped = false;
                s.push('\\');
                continue;
            }
            TokenKind::Interpolation if !is_escaped => {
                found_interpolation = true;
                s.push_str(
                    &parse_interpolation(toks, scope)?
                        .iter()
                        .map(|x| x.kind.to_string())
                        .collect::<String>(),
                );
                continue;
            }
            TokenKind::Interpolation => {
                s.push('#');
                s.push('{');
                is_escaped = false;
                continue;
            }
            _ => {}
        }
        if is_escaped && tok.kind != TokenKind::Symbol(Symbol::BackSlash) {
            is_escaped = false;
        }
        if tok.kind != TokenKind::Symbol(Symbol::BackSlash) {
            s.push_str(&tok.kind.to_string());
        }
    }
    let quotes = if found_interpolation {
        QuoteKind::Double
    } else {
        match q {
            TokenKind::Symbol(Symbol::DoubleQuote) => QuoteKind::Double,
            TokenKind::Symbol(Symbol::SingleQuote) => QuoteKind::Single,
            _ => unreachable!(),
        }
    };
    Ok(Value::Ident(s, quotes))
}
