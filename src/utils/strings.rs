use std::iter::Iterator;

use codemap::{Span, Spanned};

use peekmore::PeekMoreIterator;

use crate::common::QuoteKind;
use crate::error::SassResult;
use crate::selector::Selector;
use crate::value::Value;
use crate::{Scope, Token};

use super::{as_hex, hex_char_for, is_name, is_name_start, parse_interpolation};

pub(crate) fn is_ident(s: &str) -> bool {
    let mut chars = s.chars().peekable();
    match chars.next() {
        Some(c) if is_name_start(c) && !c.is_numeric() => {}
        Some(..) | None => return false,
    }
    while let Some(c) = chars.next() {
        if c == '\\' {
            for _ in 0..6 {
                let next = match chars.next() {
                    Some(t) => t,
                    None => return true,
                };
                if !next.is_ascii_hexdigit() {
                    break;
                }
            }
            match chars.peek() {
                Some(c) if c.is_whitespace() => {
                    chars.next();
                }
                _ => {}
            };
            continue;
        }
        if !is_name(c) {
            return false;
        }
    }
    true
}

fn ident_body_no_interpolation<I: Iterator<Item = Token>>(
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

            toks.next();
            text.push('-');
        } else if is_name(tok.kind) {
            text.push(toks.next().unwrap().kind);
        } else if tok.kind == '\\' {
            toks.next();
            text.push_str(&escape(toks, false)?);
        } else {
            break;
        }
    }
    Ok(Spanned { node: text, span })
}

fn interpolated_ident_body<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    scope: &Scope,
    super_selector: &Selector,
    mut span: Span,
    buf: &mut String,
) -> SassResult<Span> {
    while let Some(tok) = toks.peek() {
        match tok.kind {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-' | '\u{80}'..=std::char::MAX => {
                span = span.merge(tok.pos());
                buf.push(toks.next().unwrap().kind);
            }
            '\\' => {
                toks.next();
                buf.push_str(&escape(toks, false)?);
            }
            '#' => {
                if let Some(Token { kind: '{', pos }) = toks.peek_forward(1).cloned() {
                    toks.next();
                    toks.next();
                    // TODO: if ident, interpolate literally
                    let interpolation = parse_interpolation(toks, scope, super_selector, pos)?;
                    buf.push_str(&interpolation.node.to_css_string(interpolation.span)?);
                } else {
                    toks.reset_view();
                    break;
                }
            }
            _ => break,
        }
    }
    Ok(span)
}

fn escape<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    identifier_start: bool,
) -> SassResult<String> {
    let mut value = 0;
    let first = match toks.peek() {
        Some(t) => t,
        None => return Ok(String::new()),
    };
    let mut span = first.pos();
    if first.kind == '\n' {
        return Err(("Expected escape sequence.", span).into());
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
            span = span.merge(next.pos());
            value += as_hex(toks.next().unwrap().kind)
        }
        if toks.peek().is_some() && toks.peek().unwrap().kind.is_whitespace() {
            toks.next();
        }
    } else {
        let next = toks.next().unwrap();
        span = span.merge(next.pos());
        value = next.kind as u32;
    }

    let c = std::char::from_u32(value).ok_or(("Invalid escape sequence.", span))?;
    if (identifier_start && is_name_start(c) && !c.is_digit(10))
        || (!identifier_start && is_name(c))
    {
        Ok(c.to_string())
    } else if value <= 0x1F || value == 0x7F || (identifier_start && c.is_digit(10)) {
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

pub(crate) fn eat_ident<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    scope: &Scope,
    super_selector: &Selector,
    span_before: Span,
) -> SassResult<Spanned<String>> {
    let mut span = toks
        .peek()
        .ok_or(("Expected identifier.", span_before))?
        .pos();
    let mut text = String::new();
    if toks.peek().unwrap().kind == '-' {
        toks.next();
        text.push('-');
        if toks.peek().is_none() {
            return Ok(Spanned { node: text, span });
        }
        if toks.peek().unwrap().kind == '-' {
            toks.next();
            text.push('-');
            let body_span = interpolated_ident_body(toks, scope, super_selector, span, &mut text)?;
            span = span.merge(body_span);
            return Ok(Spanned { node: text, span });
        }
    }

    let Token { kind: first, pos } = match toks.peek() {
        Some(v) => *v,
        None => return Err(("Expected identifier.", span).into()),
    };

    if is_name_start(first) {
        text.push(toks.next().unwrap().kind);
    } else if first == '\\' {
        toks.next();
        text.push_str(&escape(toks, true)?);
    // TODO: peekmore
    // (first == '#' && scanner.peekChar(1) == $lbrace)
    } else if first == '#' {
        toks.next();
        let Token { kind, pos } = if let Some(tok) = toks.peek() {
            *tok
        } else {
            return Err(("Expected identifier.", pos).into());
        };
        if kind == '{' {
            toks.next();
            text.push_str(
                &match parse_interpolation(toks, scope, super_selector, pos)?.node {
                    Value::String(s, ..) => s,
                    v => v.to_css_string(span)?.into(),
                },
            );
        } else {
            return Err(("Expected identifier.", pos).into());
        }
    } else {
        return Err(("Expected identifier.", pos).into());
    }

    let body_span = interpolated_ident_body(toks, scope, super_selector, pos, &mut text)?;
    span = span.merge(body_span);
    Ok(Spanned { node: text, span })
}

pub(crate) fn eat_ident_no_interpolation<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    unit: bool,
    span_before: Span,
) -> SassResult<Spanned<String>> {
    let mut span = toks
        .peek()
        .ok_or(("Expected identifier.", span_before))?
        .pos();
    let mut text = String::new();
    if toks.peek().unwrap().kind == '-' {
        toks.next();
        text.push('-');
        if toks.peek().is_none() {
            return Ok(Spanned { node: text, span });
        }
        if toks.peek().unwrap().kind == '-' {
            toks.next();
            text.push('-');
            text.push_str(&ident_body_no_interpolation(toks, unit, span)?.node);
            return Ok(Spanned { node: text, span });
        }
    }

    let first = match toks.peek() {
        Some(v) => v,
        None => return Err(("Expected identifier.", span).into()),
    };

    if is_name_start(first.kind) {
        text.push(toks.next().unwrap().kind);
    } else if first.kind == '\\' {
        toks.next();
        text.push_str(&escape(toks, true)?);
    } else {
        return Err(("Expected identifier.", first.pos()).into());
    }

    let body = ident_body_no_interpolation(toks, unit, span)?;
    span = span.merge(body.span);
    text.push_str(&body.node);
    Ok(Spanned { node: text, span })
}

pub(crate) fn parse_quoted_string<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    scope: &Scope,
    q: char,
    super_selector: &Selector,
    span_before: Span,
) -> SassResult<Spanned<Value>> {
    let mut s = String::new();
    let mut span = toks
        .peek()
        .ok_or((format!("Expected {}.", q), span_before))?
        .pos();
    while let Some(tok) = toks.next() {
        span = span.merge(tok.pos());
        match tok.kind {
            '"' if q == '"' => break,
            '\'' if q == '\'' => break,
            '#' => {
                if let Some(Token { kind: '{', pos }) = toks.peek().cloned() {
                    toks.next();
                    let interpolation = parse_interpolation(toks, scope, super_selector, pos)?;
                    s.push_str(&match interpolation.node {
                        Value::String(s, ..) => s,
                        v => v.to_css_string(interpolation.span)?.into(),
                    });
                    continue;
                } else {
                    s.push('#');
                    continue;
                }
            }
            '\n' => return Err(("Expected \".", tok.pos()).into()),
            '\\' => {
                let first = match toks.peek() {
                    Some(c) => c,
                    None => {
                        s.push('\u{FFFD}');
                        continue;
                    }
                };

                if first.kind == '\n' {
                    toks.next();
                    continue;
                }

                if first.kind.is_ascii_hexdigit() {
                    let mut value = 0;
                    for _ in 0..6 {
                        let next = match toks.peek() {
                            Some(c) => c,
                            None => break,
                        };
                        if !next.kind.is_ascii_hexdigit() {
                            break;
                        }
                        value = (value << 4) + as_hex(toks.next().unwrap().kind);
                    }

                    if toks.peek().is_some() && toks.peek().unwrap().kind.is_ascii_whitespace() {
                        toks.next();
                    }

                    if value == 0 || (value >= 0xD800 && value <= 0xDFFF) || value >= 0x0010_FFFF {
                        s.push('\u{FFFD}');
                    } else {
                        s.push(std::char::from_u32(value).unwrap());
                    }
                } else {
                    s.push(toks.next().unwrap().kind);
                }
            }
            _ => s.push(tok.kind),
        }
    }
    Ok(Spanned {
        node: Value::String(s, QuoteKind::Quoted),
        span,
    })
}
