use std::iter::Iterator;

use crate::{
    error::SassResult,
    utils::{as_hex, hex_char_for, is_name, is_name_start},
    Token,
};

use super::Parser;

impl<'a, 'b> Parser<'a, 'b> {
    pub(crate) fn parse_escape(&mut self, identifier_start: bool) -> SassResult<String> {
        self.expect_char('\\')?;
        let mut value = 0;
        let first = match self.toks.peek() {
            Some(t) => t,
            None => return Err(("Expected expression.", self.toks.current_span()).into()),
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

        let c = std::char::from_u32(value).ok_or(("Invalid Unicode code point.", span))?;
        if (identifier_start && is_name_start(c) && !c.is_ascii_digit())
            || (!identifier_start && is_name(c))
        {
            Ok(c.to_string())
        } else if value <= 0x1F || value == 0x7F || (identifier_start && c.is_ascii_digit()) {
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

    // pub(crate) fn parse_quoted_string(&mut self, q: char) -> SassResult<Spanned<Value>> {
    // let mut s = String::new();
    // let mut span = self
    //     .toks
    //     .peek()
    //     .ok_or((format!("Expected {}.", q), self.span_before))?
    //     .pos();
    // while let Some(tok) = self.toks.next() {
    //     span = span.merge(tok.pos());
    //     match tok.kind {
    //         '"' if q == '"' => {
    //             return Ok(Spanned {
    //                 node: Value::String(s, QuoteKind::Quoted),
    //                 span,
    //             });
    //         }
    //         '\'' if q == '\'' => {
    //             return Ok(Spanned {
    //                 node: Value::String(s, QuoteKind::Quoted),
    //                 span,
    //             })
    //         }
    //         '#' => {
    //             if let Some(Token { kind: '{', pos }) = self.toks.peek() {
    //                 self.span_before = self.span_before.merge(pos);
    //                 self.toks.next();
    //                 let interpolation = self.parse_interpolation()?;
    //                 match interpolation.node {
    //                     Value::String(ref v, ..) => s.push_str(v),
    //                     v => s.push_str(
    //                         v.to_css_string(interpolation.span, self.options.is_compressed())?
    //                             .borrow(),
    //                     ),
    //                 };
    //                 continue;
    //             }

    //             s.push('#');
    //             continue;
    //         }
    //         '\n' => return Err(("Expected \".", tok.pos()).into()),
    //         '\\' => {
    //             let first = match self.toks.peek() {
    //                 Some(c) => c,
    //                 None => {
    //                     s.push('\u{FFFD}');
    //                     continue;
    //                 }
    //             };

    //             if first.kind == '\n' {
    //                 self.toks.next();
    //                 continue;
    //             }

    //             if first.kind.is_ascii_hexdigit() {
    //                 let mut value = 0;
    //                 for _ in 0..6 {
    //                     let next = match self.toks.peek() {
    //                         Some(c) => c,
    //                         None => break,
    //                     };
    //                     if !next.kind.is_ascii_hexdigit() {
    //                         break;
    //                     }
    //                     value = (value << 4) + as_hex(self.toks.next().unwrap().kind);
    //                 }

    //                 if self.toks.peek().is_some()
    //                     && self.toks.peek().unwrap().kind.is_ascii_whitespace()
    //                 {
    //                     self.toks.next();
    //                 }

    //                 if value == 0 || (0xD800..=0xDFFF).contains(&value) || value >= 0x0010_FFFF
    //                 {
    //                     s.push('\u{FFFD}');
    //                 } else {
    //                     s.push(std::char::from_u32(value).unwrap());
    //                 }
    //             } else {
    //                 s.push(self.toks.next().unwrap().kind);
    //             }
    //         }
    //         _ => s.push(tok.kind),
    //     }
    // }
    // Err((format!("Expected {}.", q), span).into())
    // }

    /// Returns whether the scanner is immediately before a plain CSS identifier.
    ///
    // todo: foward arg
    /// If `forward` is passed, this looks that many characters forward instead.
    ///
    /// This is based on [the CSS algorithm][], but it assumes all backslashes
    /// start escapes.
    ///
    /// [the CSS algorithm]: https://drafts.csswg.org/css-syntax-3/#would-start-an-identifier
    pub fn looking_at_identifier(&mut self) -> bool {
        match self.toks.peek() {
            Some(Token { kind, .. }) if is_name_start(kind) || kind == '\\' => return true,
            Some(Token { kind: '-', .. }) => {}
            Some(..) | None => return false,
        }

        match self.toks.peek_n(1) {
            Some(Token { kind, .. }) if is_name_start(kind) || kind == '-' || kind == '\\' => true,
            Some(..) | None => false,
        }
    }
}
