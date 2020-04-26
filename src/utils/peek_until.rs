use std::iter::Iterator;

use codemap::{Span, Spanned};

use peekmore::PeekMoreIterator;

use crate::error::SassResult;
use crate::Token;

use super::{as_hex, hex_char_for, is_name, is_name_start, IsWhitespace};

pub(crate) fn peek_until_closing_curly_brace<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
) -> Vec<Token> {
    let mut t = Vec::new();
    let mut nesting = 0;
    while let Some(tok) = toks.peek() {
        match tok.kind {
            q @ '"' | q @ '\'' => {
                t.push(*toks.peek().unwrap());
                toks.move_forward(1);
                t.extend(peek_until_closing_quote(toks, q));
            }
            '{' => {
                nesting += 1;
                t.push(*toks.peek().unwrap());
                toks.move_forward(1);
            }
            '}' => {
                if nesting == 0 {
                    break;
                } else {
                    nesting -= 1;
                    t.push(*toks.peek().unwrap());
                    toks.move_forward(1);
                }
            }
            '/' => {
                let next = *toks.peek_forward(1).unwrap();
                match toks.peek().unwrap().kind {
                    '/' => peek_until_newline(toks),
                    _ => t.push(next),
                };
                continue;
            }
            _ => {
                t.push(*toks.peek().unwrap());
                toks.move_forward(1);
            }
        }
    }
    peek_whitespace(toks);
    t
}

fn peek_until_closing_quote<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    q: char,
) -> Vec<Token> {
    let mut t = Vec::new();
    while let Some(tok) = toks.peek() {
        match tok.kind {
            '"' if q == '"' => {
                t.push(*tok);
                toks.move_forward(1);
                break;
            }
            '\'' if q == '\'' => {
                t.push(*tok);
                toks.move_forward(1);
                break;
            }
            '\\' => {
                t.push(*tok);
                t.push(*toks.peek_forward(1).unwrap());
            }
            '#' => {
                t.push(*tok);
                let next = toks.peek().unwrap();
                if next.kind == '{' {
                    t.push(*toks.peek_forward(1).unwrap());
                    t.append(&mut peek_until_closing_curly_brace(toks));
                }
            }
            _ => t.push(*tok),
        }
        toks.move_forward(1);
    }
    t
}

fn peek_until_newline<I: Iterator<Item = Token>>(toks: &mut PeekMoreIterator<I>) {
    while let Some(tok) = toks.peek() {
        if tok.kind == '\n' {
            break;
        }
        toks.move_forward(1);
    }
}

fn peek_whitespace<I: Iterator<Item = W>, W: IsWhitespace>(s: &mut PeekMoreIterator<I>) -> bool {
    let mut found_whitespace = false;
    while let Some(w) = s.peek() {
        if !w.is_whitespace() {
            break;
        }
        found_whitespace = true;
        s.move_forward(1);
    }
    found_whitespace
}

pub(crate) fn peek_escape<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
) -> SassResult<String> {
    let mut value = 0;
    let first = match toks.peek() {
        Some(t) => t,
        None => return Ok(String::new()),
    };
    if first.kind == '\n' {
        return Err(("Expected escape sequence.", first.pos()).into());
    } else if first.kind.is_ascii_hexdigit() {
        for _ in 0..6 {
            let next = match toks.peek() {
                Some(t) => t,
                None => break,
            };
            if !next.kind.is_ascii_hexdigit() {
                break;
            }
            value *= 16;
            value += as_hex(next.kind);
            toks.peek_forward(1);
        }
        if toks.peek().is_some() && toks.peek().unwrap().kind.is_whitespace() {
            toks.peek_forward(1);
        }
    } else {
        value = toks.peek_forward(1).unwrap().kind as u32;
    }

    let c = std::char::from_u32(value).unwrap();
    if is_name(c) {
        Ok(c.to_string())
    } else if value <= 0x1F || value == 0x7F {
        let mut buf = String::with_capacity(4);
        buf.push('\\');
        if value > 0xF {
            buf.push(hex_char_for(value >> 4));
        }
        buf.push(hex_char_for(value & 0xF));
        buf.push(' ');
        Ok(buf)
    } else {
        Ok(format!("\\{}", c))
    }
}

pub(crate) fn peek_ident_no_interpolation<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    unit: bool,
) -> SassResult<Spanned<String>> {
    let mut span = toks.peek().unwrap().pos();
    let mut text = String::new();
    if toks.peek().unwrap().kind == '-' {
        toks.peek_forward(1);
        text.push('-');
        if toks.peek().unwrap().kind == '-' {
            toks.peek_forward(1);
            text.push('-');
            text.push_str(&peek_ident_body_no_interpolation(toks, unit, span)?.node);
            return Ok(Spanned { node: text, span });
        }
    }

    let first = match toks.peek() {
        Some(v) => v,
        None => return Err(("Expected identifier.", span).into()),
    };

    if is_name_start(first.kind) {
        text.push(first.kind);
        toks.peek_forward(1);
    } else if first.kind == '\\' {
        toks.peek_forward(1);
        text.push_str(&peek_escape(toks)?);
    } else {
        return Err(("Expected identifier.", first.pos()).into());
    }

    let body = peek_ident_body_no_interpolation(toks, unit, span)?;
    span = span.merge(body.span);
    text.push_str(&body.node);
    Ok(Spanned { node: text, span })
}

fn peek_ident_body_no_interpolation<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    unit: bool,
    mut span: Span,
) -> SassResult<Spanned<String>> {
    let mut text = String::new();
    while let Some(tok) = toks.peek() {
        span = span.merge(tok.pos());
        if unit && tok.kind == '-' {
            // Disallow `-` followed by a dot or a digit digit in units.
            let second = match toks.peek_forward(1) {
                Some(v) => *v,
                None => break,
            };

            toks.peek_backward(1).unwrap();

            if second.kind == '.' || second.kind.is_ascii_digit() {
                break;
            }
            toks.peek_forward(1);
            text.push('-');
            text.push(toks.peek_forward(1).unwrap().kind);
        } else if is_name(tok.kind) {
            text.push(tok.kind);
            toks.peek_forward(1);
        } else if tok.kind == '\\' {
            toks.peek_forward(1);
            text.push_str(&peek_escape(toks)?);
        } else {
            break;
        }
    }
    Ok(Spanned { node: text, span })
}
