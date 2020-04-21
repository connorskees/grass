use std::iter::Iterator;

use peekmore::PeekMoreIterator;

use crate::Token;

use super::{devour_whitespace, read_until_newline};

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
            '(' => {
                t.push(toks.next().unwrap());
                t.extend(read_until_closing_paren(toks));
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
            '(' => {
                t.push(toks.next().unwrap());
                t.extend(read_until_closing_paren(toks));
            }
            _ => t.push(toks.next().unwrap()),
        }
    }
    devour_whitespace(toks);
    t
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
