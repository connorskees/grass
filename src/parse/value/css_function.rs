use std::{borrow::Borrow, iter::Iterator};

use codemap::Spanned;

use crate::{
    error::SassResult,
    utils::{
        as_hex, hex_char_for, is_name, peek_ident_no_interpolation, peek_until_closing_curly_brace,
        peek_whitespace,
    },
    value::Value,
    Token,
};

use super::super::Parser;

impl<'a> Parser<'a> {
    pub(super) fn parse_calc_args(&mut self, buf: &mut String) -> SassResult<()> {
        buf.reserve(2);
        buf.push('(');
        let mut nesting = 0;
        while let Some(tok) = self.toks.next() {
            match tok.kind {
                ' ' | '\t' | '\n' => {
                    self.whitespace();
                    buf.push(' ');
                }
                '#' => {
                    if let Some(Token { kind: '{', pos }) = self.toks.peek() {
                        self.span_before = *pos;
                        self.toks.next();
                        let interpolation = self.parse_interpolation()?;
                        buf.push_str(&interpolation.node.to_css_string(interpolation.span)?);
                    } else {
                        buf.push('#');
                    }
                }
                '(' => {
                    nesting += 1;
                    buf.push('(');
                }
                ')' => {
                    if nesting == 0 {
                        break;
                    } else {
                        nesting -= 1;
                        buf.push(')');
                    }
                }
                q @ '\'' | q @ '"' => {
                    buf.push('"');
                    match self.parse_quoted_string(q)?.node {
                        Value::String(ref s, ..) => buf.push_str(s),
                        _ => unreachable!(),
                    }
                    buf.push('"');
                }
                c => buf.push(c),
            }
        }
        buf.push(')');
        Ok(())
    }

    pub(super) fn parse_progid(&mut self) -> SassResult<String> {
        let mut string = String::new();
        let mut span = match self.toks.peek() {
            Some(token) => token.pos(),
            None => {
                return Err(("expected \"(\".", self.span_before).into());
            }
        };
        while let Some(tok) = self.toks.next() {
            span = span.merge(tok.pos());
            match tok.kind {
                'a'..='z' | 'A'..='Z' | '.' => {
                    string.push(tok.kind);
                }
                '(' => {
                    self.parse_calc_args(&mut string)?;
                    break;
                }
                _ => return Err(("expected \"(\".", span).into()),
            }
        }
        Ok(string)
    }

    pub(super) fn try_parse_url(&mut self) -> SassResult<Option<String>> {
        let mut buf = String::from("url(");
        peek_whitespace(self.toks);
        while let Some(tok) = self.toks.peek() {
            let kind = tok.kind;
            self.toks.advance_cursor();
            if kind == '!'
                || kind == '%'
                || kind == '&'
                || ('*'..='~').contains(&kind)
                || kind as u32 >= 0x0080
            {
                buf.push(kind);
            } else if kind == '\\' {
                buf.push_str(&self.peek_escape()?);
            } else if kind == '#' {
                if let Some(Token { kind: '{', .. }) = self.toks.peek() {
                    self.toks.advance_cursor();
                    let interpolation = self.peek_interpolation()?;
                    match interpolation.node {
                        Value::String(ref s, ..) => buf.push_str(s),
                        v => buf.push_str(v.to_css_string(interpolation.span)?.borrow()),
                    };
                } else {
                    buf.push('#');
                }
            } else if kind == ')' {
                buf.push(')');
                self.toks.truncate_iterator_to_cursor();
                return Ok(Some(buf));
            } else if kind.is_whitespace() {
                peek_whitespace(self.toks);
                if let Some(Token { kind: ')', .. }) = self.toks.peek() {
                    self.toks.advance_cursor();
                    buf.push(')');
                    self.toks.truncate_iterator_to_cursor();
                    return Ok(Some(buf));
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        self.toks.reset_cursor();
        Ok(None)
    }

    pub(super) fn try_parse_min_max(
        &mut self,
        fn_name: &str,
        allow_comma: bool,
    ) -> SassResult<Option<String>> {
        let mut buf = if allow_comma {
            format!("{}(", fn_name)
        } else {
            String::new()
        };
        peek_whitespace(self.toks);
        while let Some(tok) = self.toks.peek() {
            let kind = tok.kind;
            match kind {
                '+' | '-' | '0'..='9' => {
                    self.toks.advance_cursor();
                    if let Some(number) = self.peek_number()? {
                        buf.push(kind);
                        buf.push_str(&number);
                    } else {
                        return Ok(None);
                    }
                }
                '#' => {
                    self.toks.advance_cursor();
                    if let Some(Token { kind: '{', .. }) = self.toks.peek() {
                        self.toks.advance_cursor();
                        let interpolation = self.peek_interpolation()?;
                        match interpolation.node {
                            Value::String(ref s, ..) => buf.push_str(s),
                            v => buf.push_str(v.to_css_string(interpolation.span)?.borrow()),
                        };
                    } else {
                        return Ok(None);
                    }
                }
                'c' | 'C' => {
                    if let Some(name) = self.try_parse_min_max_function("calc")? {
                        buf.push_str(&name);
                    } else {
                        return Ok(None);
                    }
                }
                'e' | 'E' => {
                    if let Some(name) = self.try_parse_min_max_function("env")? {
                        buf.push_str(&name);
                    } else {
                        return Ok(None);
                    }
                }
                'v' | 'V' => {
                    if let Some(name) = self.try_parse_min_max_function("var")? {
                        buf.push_str(&name);
                    } else {
                        return Ok(None);
                    }
                }
                '(' => {
                    self.toks.advance_cursor();
                    buf.push('(');
                    if let Some(val) = self.try_parse_min_max(fn_name, false)? {
                        buf.push_str(&val);
                    } else {
                        return Ok(None);
                    }
                }
                'm' | 'M' => {
                    self.toks.advance_cursor();
                    match self.toks.peek() {
                        Some(Token { kind: 'i', .. }) | Some(Token { kind: 'I', .. }) => {
                            self.toks.advance_cursor();
                            if !matches!(
                                self.toks.peek(),
                                Some(Token { kind: 'n', .. }) | Some(Token { kind: 'N', .. })
                            ) {
                                return Ok(None);
                            }
                            buf.push_str("min(")
                        }
                        Some(Token { kind: 'a', .. }) | Some(Token { kind: 'A', .. }) => {
                            self.toks.advance_cursor();
                            if !matches!(
                                self.toks.peek(),
                                Some(Token { kind: 'x', .. }) | Some(Token { kind: 'X', .. })
                            ) {
                                return Ok(None);
                            }
                            buf.push_str("max(")
                        }
                        _ => return Ok(None),
                    }

                    self.toks.advance_cursor();

                    if !matches!(self.toks.peek(), Some(Token { kind: '(', .. })) {
                        return Ok(None);
                    }

                    if let Some(val) = self.try_parse_min_max(fn_name, false)? {
                        buf.push_str(&val);
                    } else {
                        return Ok(None);
                    }
                }
                _ => return Ok(None),
            }

            peek_whitespace(self.toks);

            let next = match self.toks.peek() {
                Some(tok) => tok,
                None => return Ok(None),
            };

            match next.kind {
                ')' => {
                    self.toks.advance_cursor();
                    buf.push(')');
                    return Ok(Some(buf));
                }
                '+' | '-' | '*' | '/' => {
                    buf.push(' ');
                    buf.push(next.kind);
                    buf.push(' ');
                    self.toks.advance_cursor();
                }
                ',' => {
                    if !allow_comma {
                        return Ok(None);
                    }
                    self.toks.advance_cursor();
                    buf.push(',');
                    buf.push(' ');
                }
                _ => return Ok(None),
            }

            peek_whitespace(self.toks);
        }

        Ok(Some(buf))
    }

    #[allow(dead_code, unused_mut, unused_variables, unused_assignments)]
    fn try_parse_min_max_function(&mut self, fn_name: &'static str) -> SassResult<Option<String>> {
        let mut ident = peek_ident_no_interpolation(self.toks, false, self.span_before)?.node;
        ident.make_ascii_lowercase();
        if ident != fn_name {
            return Ok(None);
        }
        if !matches!(self.toks.peek(), Some(Token { kind: '(', .. })) {
            return Ok(None);
        }
        self.toks.advance_cursor();
        ident.push('(');
        todo!("special functions inside `min()` or `max()`")
    }
}

/// Methods required to do arbitrary lookahead
impl<'a> Parser<'a> {
    fn peek_number(&mut self) -> SassResult<Option<String>> {
        let mut buf = String::new();

        let num = self.peek_whole_number();
        buf.push_str(&num);

        self.toks.advance_cursor();

        if let Some(Token { kind: '.', .. }) = self.toks.peek() {
            self.toks.advance_cursor();
            let num = self.peek_whole_number();
            if num.is_empty() {
                return Ok(None);
            }
            buf.push_str(&num);
        } else {
            self.toks.move_cursor_back().unwrap();
        }

        let next = match self.toks.peek() {
            Some(tok) => tok,
            None => return Ok(Some(buf)),
        };

        match next.kind {
            'a'..='z' | 'A'..='Z' | '-' | '_' | '\\' => {
                let unit = peek_ident_no_interpolation(self.toks, true, self.span_before)?.node;

                buf.push_str(&unit);
            }
            '%' => {
                self.toks.advance_cursor();
                buf.push('%');
            }
            _ => {}
        }

        Ok(Some(buf))
    }

    fn peek_whole_number(&mut self) -> String {
        let mut buf = String::new();
        while let Some(tok) = self.toks.peek() {
            if tok.kind.is_ascii_digit() {
                buf.push(tok.kind);
                self.toks.advance_cursor();
            } else {
                return buf;
            }
        }
        buf
    }

    fn peek_interpolation(&mut self) -> SassResult<Spanned<Value>> {
        let vec = peek_until_closing_curly_brace(self.toks)?;
        self.toks.advance_cursor();
        let val = self.parse_value_from_vec(vec, false)?;
        Ok(Spanned {
            node: val.node.unquote(),
            span: val.span,
        })
    }

    fn peek_escape(&mut self) -> SassResult<String> {
        let mut value = 0;
        let first = match self.toks.peek() {
            Some(t) => *t,
            None => return Ok(String::new()),
        };
        let mut span = first.pos;
        if first.kind == '\n' {
            return Err(("Expected escape sequence.", first.pos()).into());
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
                value += as_hex(next.kind);
                span = span.merge(next.pos);
                self.toks.peek_forward(1);
            }
            if matches!(
                self.toks.peek(),
                Some(Token { kind: ' ', .. })
                    | Some(Token { kind: '\n', .. })
                    | Some(Token { kind: '\t', .. })
            ) {
                self.toks.peek_forward(1);
            }
        } else {
            value = self.toks.peek_forward(1).unwrap().kind as u32;
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
}
