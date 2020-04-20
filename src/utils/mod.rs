use std::iter::Iterator;

use codemap::Spanned;

use peekmore::PeekMoreIterator;

use crate::error::SassResult;
use crate::selector::Selector;
use crate::value::Value;
use crate::{Scope, Token};

pub(crate) use chars::*;
pub(crate) use comment_whitespace::*;
pub(crate) use strings::*;
pub(crate) use variables::*;

mod chars;
mod comment_whitespace;
mod strings;
mod variables;

pub(crate) fn parse_interpolation<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<Spanned<Value>> {
    let val = Value::from_vec(read_until_closing_curly_brace(toks), scope, super_selector)?;
    toks.next();
    Ok(Spanned {
        node: val.node.eval(val.span)?.node.unquote(),
        span: val.span,
    })
}

// Eat tokens until an open curly brace
//
// Does not consume the open curly brace
pub(crate) fn read_until_open_curly_brace<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
) -> Vec<Token> {
    let mut val = Vec::new();
    let mut n = 0;
    while let Some(tok) = toks.peek() {
        match tok.kind {
            '{' => n += 1,
            '}' => n -= 1,
            '/' => {
                let next = toks.next().unwrap();
                match toks.peek().unwrap().kind {
                    '/' => read_until_newline(toks),
                    _ => val.push(next),
                };
                continue;
            }
            _ => {}
        }
        if n == 1 {
            break;
        }

        val.push(toks.next().unwrap());
    }
    val
}

pub(crate) fn read_until_closing_curly_brace<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
) -> Vec<Token> {
    let mut t = Vec::new();
    let mut nesting = 0;
    while let Some(tok) = toks.peek() {
        match tok.kind {
            q @ '"' | q @ '\'' => {
                t.push(toks.next().unwrap());
                t.extend(read_until_closing_quote(toks, q));
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
                match toks.peek().unwrap().kind {
                    '/' => read_until_newline(toks),
                    _ => t.push(next),
                };
                continue;
            }
            _ => t.push(toks.next().unwrap()),
        }
    }
    devour_whitespace(toks);
    t
}

pub(crate) fn read_until_closing_quote<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    q: char,
) -> Vec<Token> {
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
                t.push(toks.next().unwrap());
            }
            '#' => {
                t.push(tok);
                let next = toks.peek().unwrap();
                if next.kind == '{' {
                    t.push(toks.next().unwrap());
                    t.append(&mut read_until_closing_curly_brace(toks));
                }
            }
            _ => t.push(tok),
        }
    }
    t
}

pub(crate) fn read_until_semicolon_or_closing_curly_brace<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
) -> Vec<Token> {
    let mut t = Vec::new();
    let mut nesting = 0;
    while let Some(tok) = toks.peek() {
        match tok.kind {
            ';' => {
                break;
            }
            '\\' => {
                t.push(toks.next().unwrap());
                t.push(toks.next().unwrap());
            }
            '"' | '\'' => {
                let quote = toks.next().unwrap();
                t.push(quote.clone());
                t.extend(read_until_closing_quote(toks, quote.kind));
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
                match toks.peek().unwrap().kind {
                    '/' => read_until_newline(toks),
                    _ => t.push(next),
                };
                continue;
            }
            _ => t.push(toks.next().unwrap()),
        }
    }
    devour_whitespace(toks);
    t
}

pub(crate) fn read_until_semicolon_or_open_or_closing_curly_brace<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
) -> Vec<Token> {
    let mut t = Vec::new();
    let mut nesting = 0;
    while let Some(tok) = toks.peek() {
        match tok.kind {
            ';' => {
                break;
            }
            '\\' => {
                t.push(toks.next().unwrap());
                t.push(toks.next().unwrap());
            }
            '"' | '\'' => {
                let quote = toks.next().unwrap();
                t.push(quote.clone());
                t.extend(read_until_closing_quote(toks, quote.kind));
            }
            '#' => {
                t.push(toks.next().unwrap());
                match toks.peek().unwrap().kind {
                    '{' => nesting += 1,
                    ';' => break,
                    '}' => {
                        if nesting == 0 {
                            break;
                        } else {
                            nesting -= 1;
                        }
                    }
                    _ => {}
                }
                t.push(toks.next().unwrap());
            }
            '{' => break,
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
                match toks.peek().unwrap().kind {
                    '/' => read_until_newline(toks),
                    _ => t.push(next),
                };
                continue;
            }
            _ => t.push(toks.next().unwrap()),
        }
    }
    devour_whitespace(toks);
    t
}

pub(crate) fn eat_number<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
) -> SassResult<Spanned<String>> {
    let mut whole = String::new();
    let mut span = if let Some(tok) = toks.peek() {
        tok.pos()
    } else {
        todo!()
    };
    while let Some(c) = toks.peek() {
        if !c.kind.is_numeric() {
            break;
        }
        let tok = toks.next().unwrap();
        span = span.merge(tok.pos());
        whole.push(tok.kind);
    }

    if toks.peek().is_none() {
        return Ok(Spanned { node: whole, span });
    }

    let mut dec = String::new();

    let next_tok = toks.peek().unwrap().clone();

    if next_tok.kind == '.' {
        toks.next();
        dec.push('.');
        while let Some(c) = toks.peek() {
            if !c.kind.is_numeric() {
                break;
            }
            let tok = toks.next().unwrap();
            span = span.merge(tok.pos());
            dec.push(tok.kind);
        }
    }

    if dec.len() == 1 {
        return Err(("Expected digit.", next_tok.pos()).into());
    }

    whole.push_str(&dec);
    Ok(Spanned { node: whole, span })
}

pub(crate) fn read_until_closing_paren<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
) -> Vec<Token> {
    let mut v = Vec::new();
    let mut scope = 0;
    while let Some(tok) = toks.next() {
        match tok.kind {
            ')' => {
                if scope < 1 {
                    v.push(tok);
                    return v;
                } else {
                    scope -= 1;
                }
            }
            '(' => scope += 1,
            '"' | '\'' => {
                v.push(tok.clone());
                v.extend(read_until_closing_quote(toks, tok.kind));
                continue;
            }
            _ => {}
        }
        v.push(tok)
    }
    v
}

pub(crate) fn read_until_closing_square_brace<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
) -> Vec<Token> {
    let mut v = Vec::new();
    let mut scope = 0;
    while let Some(tok) = toks.next() {
        match tok.kind {
            ']' => {
                if scope < 1 {
                    v.push(tok);
                    return v;
                } else {
                    scope -= 1;
                }
            }
            '[' => scope += 1,
            '"' | '\'' => {
                v.push(tok.clone());
                v.extend(read_until_closing_quote(toks, tok.kind));
                continue;
            }
            _ => {}
        }
        v.push(tok)
    }
    v
}
