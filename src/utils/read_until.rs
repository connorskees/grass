use std::iter::Iterator;

use peekmore::PeekMoreIterator;

use crate::error::SassResult;
use crate::Token;

use super::{devour_whitespace, read_until_newline};

// Eat tokens until an open curly brace
//
// Does not consume the open curly brace
pub(crate) fn read_until_open_curly_brace<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
) -> SassResult<Vec<Token>> {
    let mut t = Vec::new();
    let mut n = 0;
    while let Some(tok) = toks.peek() {
        match tok.kind {
            '{' => n += 1,
            '}' => n -= 1,
            '/' => {
                let next = toks.next().unwrap();
                match toks.peek() {
                    Some(Token { kind: '/', .. }) => read_until_newline(toks),
                    _ => t.push(next),
                };
                continue;
            }
            '\\' => {
                t.push(toks.next().unwrap());
                if toks.peek().is_some() {
                    t.push(toks.next().unwrap());
                }
                continue;
            }
            q @ '"' | q @ '\'' => {
                t.push(toks.next().unwrap());
                t.extend(read_until_closing_quote(toks, q)?);
                continue;
            }
            _ => {}
        }
        if n == 1 {
            break;
        }

        t.push(toks.next().unwrap());
    }
    Ok(t)
}

pub(crate) fn read_until_closing_curly_brace<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
) -> SassResult<Vec<Token>> {
    let mut t = Vec::new();
    let mut nesting = 0;
    while let Some(tok) = toks.peek() {
        match tok.kind {
            q @ '"' | q @ '\'' => {
                t.push(toks.next().unwrap());
                t.extend(read_until_closing_quote(toks, q)?);
            }
            '{' => {
                nesting += 1;
                t.push(toks.next().unwrap());
            }
            '}' => {
                if nesting == 0 {
                    break;
                } else {
                    nesting -= 1;
                    t.push(toks.next().unwrap());
                }
            }
            '/' => {
                let next = toks.next().unwrap();
                match toks.peek() {
                    Some(Token { kind: '/', .. }) => {
                        read_until_newline(toks);
                        devour_whitespace(toks);
                    }
                    Some(..) | None => t.push(next),
                };
                continue;
            }
            '(' => {
                t.push(toks.next().unwrap());
                t.extend(read_until_closing_paren(toks)?);
            }
            '\\' => {
                t.push(toks.next().unwrap());
                if toks.peek().is_some() {
                    t.push(toks.next().unwrap());
                }
            }
            _ => t.push(toks.next().unwrap()),
        }
    }
    devour_whitespace(toks);
    Ok(t)
}

/// Read tokens into a vector until a matching closing quote is found
///
/// The closing quote is included in the output
pub(crate) fn read_until_closing_quote<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    q: char,
) -> SassResult<Vec<Token>> {
    let mut t = Vec::new();
    while let Some(tok) = toks.next() {
        match tok.kind {
            '"' if q == '"' => {
                t.push(tok);
                break;
            }
            '\'' if q == '\'' => {
                t.push(tok);
                break;
            }
            '\\' => {
                t.push(tok);
                if toks.peek().is_some() {
                    t.push(toks.next().unwrap());
                }
            }
            '#' => {
                t.push(tok);
                let next = toks.peek().unwrap();
                if next.kind == '{' {
                    t.push(toks.next().unwrap());
                    t.append(&mut read_until_closing_curly_brace(toks)?);
                }
            }
            _ => t.push(tok),
        }
    }
    if let Some(tok) = t.pop() {
        if tok.kind != q {
            return Err((format!("Expected {}.", q), tok.pos).into());
        }
        t.push(tok);
    }
    Ok(t)
}

pub(crate) fn read_until_semicolon_or_closing_curly_brace<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
) -> SassResult<Vec<Token>> {
    let mut t = Vec::new();
    let mut nesting = 0;
    while let Some(tok) = toks.peek() {
        match tok.kind {
            ';' => {
                break;
            }
            '\\' => {
                t.push(toks.next().unwrap());
                if toks.peek().is_some() {
                    t.push(toks.next().unwrap());
                }
            }
            '"' | '\'' => {
                let quote = toks.next().unwrap();
                t.push(quote);
                t.extend(read_until_closing_quote(toks, quote.kind)?);
            }
            '{' => {
                nesting += 1;
                t.push(toks.next().unwrap());
            }
            '}' => {
                if nesting == 0 {
                    break;
                } else {
                    nesting -= 1;
                    t.push(toks.next().unwrap());
                }
            }
            '/' => {
                let next = toks.next().unwrap();
                match toks.peek() {
                    Some(Token { kind: '/', .. }) => {
                        read_until_newline(toks);
                        devour_whitespace(toks);
                    }
                    _ => t.push(next),
                };
                continue;
            }
            _ => t.push(toks.next().unwrap()),
        }
    }
    devour_whitespace(toks);
    Ok(t)
}

pub(crate) fn read_until_closing_paren<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
) -> SassResult<Vec<Token>> {
    let mut t = Vec::new();
    let mut scope = 0;
    while let Some(tok) = toks.next() {
        match tok.kind {
            ')' => {
                if scope < 1 {
                    t.push(tok);
                    return Ok(t);
                } else {
                    scope -= 1;
                }
            }
            '(' => scope += 1,
            '"' | '\'' => {
                t.push(tok);
                t.extend(read_until_closing_quote(toks, tok.kind)?);
                continue;
            }
            '\\' => {
                t.push(tok);
                if toks.peek().is_some() {
                    t.push(toks.next().unwrap());
                }
                continue;
            }
            _ => {}
        }
        t.push(tok)
    }
    Ok(t)
}

pub(crate) fn read_until_closing_square_brace<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
) -> SassResult<Vec<Token>> {
    let mut t = Vec::new();
    let mut scope = 0;
    while let Some(tok) = toks.next() {
        // TODO: comments
        match tok.kind {
            ']' => {
                if scope < 1 {
                    t.push(tok);
                    return Ok(t);
                } else {
                    scope -= 1;
                }
            }
            '[' => scope += 1,
            '"' | '\'' => {
                t.push(tok);
                t.extend(read_until_closing_quote(toks, tok.kind)?);
                continue;
            }
            '\\' => {
                t.push(tok);
                if toks.peek().is_some() {
                    t.push(toks.next().unwrap());
                }
            }
            _ => {}
        }
        t.push(tok)
    }
    Ok(t)
}
