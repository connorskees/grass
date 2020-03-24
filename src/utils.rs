use std::iter::{Iterator, Peekable};

use crate::common::{Keyword, QuoteKind, Symbol, Whitespace};
use crate::error::SassResult;
use crate::lexer::Lexer;
use crate::selector::Selector;
use crate::value::Value;
use crate::{Scope, Token, TokenKind};

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
    super_selector: &Selector,
) -> SassResult<Value> {
    let mut val = String::new();
    while let Some(tok) = tokens.next() {
        match tok.kind {
            TokenKind::Symbol(Symbol::CloseCurlyBrace) => break,
            TokenKind::Symbol(Symbol::OpenCurlyBrace) => {
                todo!("invalid character in interpolation")
            }
            q @ TokenKind::Symbol(Symbol::DoubleQuote)
            | q @ TokenKind::Symbol(Symbol::SingleQuote) => {
                val.push_str(&parse_quoted_string(tokens, scope, &q, super_selector)?.to_string())
            }
            TokenKind::Variable(ref v) => {
                val.push_str(&scope.get_var(v)?.clone().unquote().to_string())
            }
            TokenKind::Interpolation => val.push_str(
                &Lexer::new(&parse_interpolation(tokens, scope, super_selector)?.to_string())
                    .map(|x| x.kind.to_string())
                    .collect::<String>(),
            ),
            _ => val.push_str(&tok.kind.to_string()),
        }
    }
    if val.trim().is_empty() {
        return Ok(Value::Ident(String::new(), QuoteKind::None));
    }
    Ok(
        Value::from_tokens(&mut Lexer::new(&val).peekable(), scope, super_selector)?
            .eval()?
            .unquote(),
    )
}

pub(crate) struct VariableDecl {
    pub val: Value,
    pub default: bool,
    pub global: bool,
}

impl VariableDecl {
    pub const fn new(val: Value, default: bool, global: bool) -> VariableDecl {
        VariableDecl {
            val,
            default,
            global,
        }
    }
}

pub(crate) fn read_until_closing_quote<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    q: &TokenKind,
) -> Vec<Token> {
    let mut is_escaped = false;
    let mut t = Vec::new();
    for tok in toks {
        match tok.kind {
            TokenKind::Symbol(Symbol::DoubleQuote)
                if !is_escaped && q == &TokenKind::Symbol(Symbol::DoubleQuote) =>
            {
                t.push(tok);
                break;
            }
            TokenKind::Symbol(Symbol::DoubleQuote) if is_escaped => {
                t.push(tok);
                is_escaped = false;
                continue;
            }
            TokenKind::Symbol(Symbol::SingleQuote)
                if !is_escaped && q == &TokenKind::Symbol(Symbol::SingleQuote) =>
            {
                t.push(tok);
                break;
            }
            TokenKind::Symbol(Symbol::SingleQuote) if is_escaped => {
                t.push(tok);
                is_escaped = false;
                continue;
            }
            TokenKind::Symbol(Symbol::BackSlash) if !is_escaped => {
                t.push(tok);
                is_escaped = true
            },
            TokenKind::Symbol(Symbol::BackSlash) => {
                is_escaped = false;
                t.push(tok);
                continue;
            }
            _ => t.push(tok),
        }
    }
    t
}

pub(crate) fn read_until_semicolon_or_curly_brace<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
) -> Vec<Token> {
    let mut t = Vec::new();
    let mut nesting = 0;
    while let Some(tok) = toks.peek() {
        match tok.kind {
            TokenKind::Symbol(Symbol::SemiColon) => {
                toks.next();
                break;
            }
            TokenKind::Symbol(Symbol::DoubleQuote) | TokenKind::Symbol(Symbol::SingleQuote) => {
                let quote = toks.next().unwrap();
                t.push(quote.clone());
                t.extend(read_until_closing_quote(toks, &quote.kind));
            }
            TokenKind::Interpolation | TokenKind::Symbol(Symbol::OpenCurlyBrace) => {
                nesting += 1;
                t.push(toks.next().unwrap());
            }
            TokenKind::Symbol(Symbol::CloseCurlyBrace) => {
                if nesting == 0 {
                    break;
                } else {
                    nesting -= 1;
                    t.push(toks.next().unwrap());
                }
            }
            _ => t.push(toks.next().unwrap()),
        }
    }
    devour_whitespace(toks);
    t
}

pub(crate) fn eat_variable_value<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<VariableDecl> {
    devour_whitespace(toks);
    let mut default = false;
    let mut global = false;
    let mut raw = read_until_semicolon_or_curly_brace(toks)
        .into_iter()
        .filter(|t| match t.kind {
            TokenKind::Keyword(Keyword::Default) => {
                default = true;
                false
            }
            TokenKind::Keyword(Keyword::Global) => {
                global = true;
                false
            }
            _ => true,
        })
        .peekable();
    let val = Value::from_tokens(&mut raw, scope, super_selector)?;
    Ok(VariableDecl::new(val, default, global))
}

pub(crate) fn flatten_ident<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<String> {
    let mut s = String::new();
    while let Some(tok) = toks.peek() {
        match tok.kind.clone() {
            TokenKind::Interpolation => {
                toks.next();
                s.push_str(&parse_interpolation(toks, scope, super_selector)?.to_string())
            }
            TokenKind::Ident(ref i) => {
                toks.next();
                s.push_str(i)
            }
            TokenKind::Number(ref n) => {
                toks.next();
                s.push_str(n)
            }
            TokenKind::Symbol(Symbol::BackSlash) => {
                s.push('\\');
                toks.next();
                if let Some(tok) = toks.next() {
                    match tok.kind {
                        TokenKind::Symbol(Symbol::Plus) => s.push('+'),
                        TokenKind::Symbol(Symbol::BackSlash) => s.push('\\'),
                        _ => todo!("value after \\"),
                    }
                } else {
                    todo!()
                }
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
    super_selector: &Selector,
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
                s.push_str(&parse_interpolation(toks, scope, super_selector)?.to_string());
                continue;
            }
            TokenKind::Interpolation => {
                s.push('#');
                s.push('{');
                is_escaped = false;
                continue;
            }
            TokenKind::Whitespace(Whitespace::Newline) => return Err("Expected \".".into()),
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
