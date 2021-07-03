use std::{borrow::Borrow, iter::Iterator};

use codemap::Spanned;

use crate::{
    common::QuoteKind,
    error::SassResult,
    utils::{as_hex, hex_char_for, is_name, is_name_start},
    value::Value,
    Token,
};

use super::Parser;

impl<'a> Parser<'a> {
    fn ident_body_no_interpolation(&mut self, unit: bool) -> SassResult<Spanned<String>> {
        let mut text = String::new();
        while let Some(tok) = self.toks.peek() {
            self.span_before = self.span_before.merge(tok.pos());
            if unit && tok.kind == '-' {
                // Disallow `-` followed by a dot or a digit digit in units.
                let second = match self.toks.peek_forward(1) {
                    Some(v) => *v,
                    None => break,
                };

                self.toks.peek_backward(1).unwrap();

                if second.kind == '.' || second.kind.is_ascii_digit() {
                    break;
                }

                self.toks.next();
                text.push('-');
            } else if is_name(tok.kind) {
                text.push(self.toks.next().unwrap().kind);
            } else if tok.kind == '\\' {
                self.toks.next();
                text.push_str(&self.escape(false)?);
            } else {
                break;
            }
        }
        Ok(Spanned {
            node: text,
            span: self.span_before,
        })
    }

    fn interpolated_ident_body(&mut self, buf: &mut String) -> SassResult<()> {
        while let Some(tok) = self.toks.peek() {
            match tok.kind {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-' | '\u{80}'..=std::char::MAX => {
                    self.span_before = self.span_before.merge(tok.pos());
                    buf.push(self.toks.next().unwrap().kind);
                }
                '\\' => {
                    self.toks.next();
                    buf.push_str(&self.escape(false)?);
                }
                '#' => {
                    if let Some(Token { kind: '{', .. }) = self.toks.peek_forward(1).cloned() {
                        self.toks.next();
                        self.toks.next();
                        // TODO: if ident, interpolate literally
                        let interpolation = self.parse_interpolation()?;
                        buf.push_str(&interpolation.node.to_css_string(interpolation.span)?);
                    } else {
                        self.toks.reset_cursor();
                        break;
                    }
                }
                _ => break,
            }
        }
        Ok(())
    }

    fn escape(&mut self, identifier_start: bool) -> SassResult<String> {
        let mut value = 0;
        let first = match self.toks.peek() {
            Some(t) => t,
            None => return Err(("Expected expression.", self.span_before).into()),
        };
        let mut span = first.pos();
        if first.kind == '\n' {
            return Err(("Expected escape sequence.", span).into());
        } else if first.kind.is_ascii_hexdigit() {
            for _ in 0..6 {
                let next = match self.toks.peek() {
                    Some(t) => t,
                    None => break,
                };
                if !next.kind.is_ascii_hexdigit() {
                    break;
                }
                value *= 16;
                span = span.merge(next.pos);
                value += as_hex(next.kind);
                self.toks.next();
            }
            if matches!(
                self.toks.peek(),
                Some(Token { kind: ' ', .. })
                    | Some(Token { kind: '\n', .. })
                    | Some(Token { kind: '\t', .. })
            ) {
                self.toks.next();
            }
        } else {
            span = span.merge(first.pos);
            value = first.kind as u32;
            self.toks.next();
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

    pub(crate) fn parse_identifier(&mut self) -> SassResult<Spanned<String>> {
        let Token { kind, pos } = self
            .toks
            .peek()
            .cloned()
            .ok_or(("Expected identifier.", self.span_before))?;
        let mut text = String::new();
        if kind == '-' {
            self.toks.next();
            text.push('-');
            match self.toks.peek() {
                Some(Token { kind: '-', .. }) => {
                    self.toks.next();
                    text.push('-');
                    self.interpolated_ident_body(&mut text)?;
                    return Ok(Spanned {
                        node: text,
                        span: pos,
                    });
                }
                Some(..) => {}
                None => {
                    return Ok(Spanned {
                        node: text,
                        span: self.span_before,
                    })
                }
            }
        }

        let Token { kind: first, pos } = match self.toks.peek() {
            Some(v) => *v,
            None => return Err(("Expected identifier.", self.span_before).into()),
        };

        match first {
            c if is_name_start(c) => {
                text.push(self.toks.next().unwrap().kind);
            }
            '\\' => {
                self.toks.next();
                text.push_str(&self.escape(true)?);
            }
            '#' if matches!(self.toks.peek_forward(1), Some(Token { kind: '{', .. })) => {
                self.toks.next();
                self.toks.next();
                match self.parse_interpolation()?.node {
                    Value::String(ref s, ..) => text.push_str(s),
                    v => text.push_str(v.to_css_string(self.span_before)?.borrow()),
                }
            }
            _ => return Err(("Expected identifier.", pos).into()),
        }

        self.interpolated_ident_body(&mut text)?;
        Ok(Spanned {
            node: text,
            span: self.span_before,
        })
    }

    pub(crate) fn parse_identifier_no_interpolation(
        &mut self,
        unit: bool,
    ) -> SassResult<Spanned<String>> {
        let Token {
            kind,
            pos: mut span,
        } = self
            .toks
            .peek()
            .ok_or(("Expected identifier.", self.span_before))?;
        let mut text = String::new();
        if kind == &'-' {
            self.toks.next();
            text.push('-');

            match self.toks.peek() {
                Some(Token { kind: '-', .. }) => {
                    self.toks.next();
                    text.push('-');
                    text.push_str(&self.ident_body_no_interpolation(unit)?.node);
                    return Ok(Spanned { node: text, span });
                }
                Some(..) => {}
                None => return Ok(Spanned { node: text, span }),
            }
        }

        let first = match self.toks.next() {
            Some(v) => v,
            None => return Err(("Expected identifier.", span).into()),
        };

        if is_name_start(first.kind) {
            text.push(first.kind);
        } else if first.kind == '\\' {
            text.push_str(&self.escape(true)?);
        } else {
            return Err(("Expected identifier.", first.pos).into());
        }

        let body = self.ident_body_no_interpolation(unit)?;
        span = span.merge(body.span);
        text.push_str(&body.node);
        Ok(Spanned { node: text, span })
    }

    pub(crate) fn parse_quoted_string(&mut self, q: char) -> SassResult<Spanned<Value>> {
        let mut s = String::new();
        let mut span = self
            .toks
            .peek()
            .ok_or((format!("Expected {}.", q), self.span_before))?
            .pos();
        while let Some(tok) = self.toks.next() {
            span = span.merge(tok.pos());
            match tok.kind {
                '"' if q == '"' => {
                    return Ok(Spanned {
                        node: Value::String(s, QuoteKind::Quoted),
                        span,
                    });
                }
                '\'' if q == '\'' => {
                    return Ok(Spanned {
                        node: Value::String(s, QuoteKind::Quoted),
                        span,
                    })
                }
                '#' => {
                    if let Some(Token { kind: '{', pos }) = self.toks.peek() {
                        self.span_before = self.span_before.merge(*pos);
                        self.toks.next();
                        let interpolation = self.parse_interpolation()?;
                        match interpolation.node {
                            Value::String(ref v, ..) => s.push_str(v),
                            v => s.push_str(v.to_css_string(interpolation.span)?.borrow()),
                        };
                        continue;
                    } else {
                        s.push('#');
                        continue;
                    }
                }
                '\n' => return Err(("Expected \".", tok.pos()).into()),
                '\\' => {
                    let first = match self.toks.peek() {
                        Some(c) => c,
                        None => {
                            s.push('\u{FFFD}');
                            continue;
                        }
                    };

                    if first.kind == '\n' {
                        self.toks.next();
                        continue;
                    }

                    if first.kind.is_ascii_hexdigit() {
                        let mut value = 0;
                        for _ in 0..6 {
                            let next = match self.toks.peek() {
                                Some(c) => c,
                                None => break,
                            };
                            if !next.kind.is_ascii_hexdigit() {
                                break;
                            }
                            value = (value << 4) + as_hex(self.toks.next().unwrap().kind);
                        }

                        if self.toks.peek().is_some()
                            && self.toks.peek().unwrap().kind.is_ascii_whitespace()
                        {
                            self.toks.next();
                        }

                        if value == 0 || (0xD800..=0xDFFF).contains(&value) || value >= 0x0010_FFFF
                        {
                            s.push('\u{FFFD}');
                        } else {
                            s.push(std::char::from_u32(value).unwrap());
                        }
                    } else {
                        s.push(self.toks.next().unwrap().kind);
                    }
                }
                _ => s.push(tok.kind),
            }
        }
        Err((format!("Expected {}.", q), span).into())
    }
}
