use std::vec::IntoIter;

use codemap::{Span, Spanned};

use peekmore::PeekMoreIterator;

use crate::{error::SassResult, Token};

use super::{as_hex, hex_char_for, is_name, is_name_start, peek_whitespace};

pub(crate) fn peek_until_closing_curly_brace(
    toks: &mut PeekMoreIterator<IntoIter<Token>>,
) -> SassResult<Vec<Token>> {
    let mut t = Vec::new();
    let mut nesting = 0;
    while let Some(tok) = toks.peek().copied() {
        match tok.kind {
            q @ '"' | q @ '\'' => {
                t.push(tok);
                toks.advance_cursor();
                t.extend(peek_until_closing_quote(toks, q)?);
            }
            '{' => {
                nesting += 1;
                t.push(tok);
                toks.advance_cursor();
            }
            '}' => {
                if nesting == 0 {
                    break;
                }

                nesting -= 1;
                t.push(tok);
                toks.advance_cursor();
            }
            '/' => {
                let next = *toks
                    .peek_forward(1)
                    .ok_or(("Expected expression.", tok.pos))?;
                match toks.peek() {
                    Some(Token { kind: '/', .. }) => peek_until_newline(toks),
                    _ => t.push(next),
                };
                continue;
            }
            _ => {
                t.push(tok);
                toks.advance_cursor();
            }
        }
    }
    peek_whitespace(toks);
    Ok(t)
}

fn peek_until_closing_quote(
    toks: &mut PeekMoreIterator<IntoIter<Token>>,
    q: char,
) -> SassResult<Vec<Token>> {
    let mut t = Vec::new();
    while let Some(tok) = toks.peek().copied() {
        match tok.kind {
            '"' if q == '"' => {
                t.push(tok);
                toks.advance_cursor();
                break;
            }
            '\'' if q == '\'' => {
                t.push(tok);
                toks.advance_cursor();
                break;
            }
            '\\' => {
                t.push(tok);
                t.push(match toks.peek_forward(1) {
                    Some(tok) => *tok,
                    None => return Err((format!("Expected {}.", q), tok.pos).into()),
                });
            }
            '#' => {
                t.push(tok);
                let next = match toks.peek() {
                    Some(tok) => tok,
                    None => return Err((format!("Expected {}.", q), tok.pos).into()),
                };
                if next.kind == '{' {
                    t.push(*next);
                    toks.peek_forward(1);
                    t.append(&mut peek_until_closing_curly_brace(toks)?);
                }
            }
            _ => t.push(tok),
        }
        toks.advance_cursor();
    }
    Ok(t)
}

pub(crate) fn peek_until_newline(toks: &mut PeekMoreIterator<IntoIter<Token>>) {
    while let Some(tok) = toks.peek() {
        if tok.kind == '\n' {
            break;
        }
        toks.advance_cursor();
    }
}

pub(crate) fn peek_escape(toks: &mut PeekMoreIterator<IntoIter<Token>>) -> SassResult<String> {
    let mut value = 0;
    let first = match toks.peek() {
        Some(t) => *t,
        None => return Ok(String::new()),
    };
    let mut span = first.pos;
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
            span = span.merge(next.pos);
            toks.peek_forward(1);
        }
        if toks.peek().is_some() && toks.peek().unwrap().kind.is_whitespace() {
            toks.peek_forward(1);
        }
    } else {
        value = first.kind as u32;
        toks.advance_cursor();
    }

    let c = std::char::from_u32(value).ok_or(("Invalid escape sequence.", span))?;
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

pub(crate) fn peek_ident_no_interpolation(
    toks: &mut PeekMoreIterator<IntoIter<Token>>,
    unit: bool,
    span_before: Span,
) -> SassResult<Spanned<String>> {
    let mut span = toks
        .peek()
        .ok_or(("Expected identifier.", span_before))?
        .pos();
    let mut text = String::new();
    if let Some(Token { kind: '-', .. }) = toks.peek() {
        toks.peek_forward(1);
        text.push('-');
        if toks.peek().is_none() {
            return Ok(Spanned { node: text, span });
        }
        if let Some(Token { kind: '-', .. }) = toks.peek() {
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

fn peek_ident_body_no_interpolation(
    toks: &mut PeekMoreIterator<IntoIter<Token>>,
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
