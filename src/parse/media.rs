use crate::{
    error::SassResult,
    utils::{is_name_start, peek_ident_no_interpolation, read_until_closing_paren},
    {Cow, Token},
};

use super::Parser;

impl<'a> Parser<'a> {
    pub fn scan_identifier(&mut self, ident: &str) -> SassResult<bool> {
        let peeked_identifier =
            match peek_ident_no_interpolation(self.toks, false, self.span_before) {
                Ok(v) => v.node,
                Err(..) => return Ok(false),
            };
        if peeked_identifier == ident {
            self.toks.take(ident.chars().count()).for_each(drop);
            self.toks.reset_cursor();
            return Ok(true);
        }
        self.toks.reset_cursor();
        Ok(false)
    }

    pub fn expect_char(&mut self, c: char) -> SassResult<()> {
        if let Some(Token { kind, .. }) = self.toks.peek() {
            if *kind == c {
                self.toks.next();
                return Ok(());
            }
        }
        Err((format!("expected \"{}\".", c), self.span_before).into())
    }

    pub fn scan_char(&mut self, c: char) -> bool {
        if let Some(Token { kind, .. }) = self.toks.peek() {
            if *kind == c {
                self.toks.next();
                return true;
            }
        }
        false
    }

    pub fn expression_until_comparison(&mut self) -> SassResult<Cow<'static, str>> {
        let mut toks = Vec::new();
        while let Some(tok) = self.toks.peek().cloned() {
            match tok.kind {
                '=' => {
                    self.toks.advance_cursor();
                    if matches!(self.toks.peek(), Some(Token { kind: '=', .. })) {
                        self.toks.reset_cursor();
                        break;
                    }
                    self.toks.reset_cursor();
                    toks.push(tok);
                    toks.push(tok);
                    self.toks.next();
                    self.toks.next();
                }
                '>' | '<' | ':' => {
                    break;
                }
                _ => {
                    toks.push(tok);
                    self.toks.next();
                }
            }
        }
        self.parse_value_as_string_from_vec(toks)
    }

    pub(super) fn parse_media_query_list(&mut self) -> SassResult<String> {
        let mut buf = String::new();
        loop {
            self.whitespace();
            buf.push_str(&self.parse_single_media_query()?);
            if !self.scan_char(',') {
                break;
            }
            buf.push(',');
            buf.push(' ');
        }
        Ok(buf)
    }

    fn parse_media_feature(&mut self) -> SassResult<String> {
        if let Some(Token { kind: '#', .. }) = self.toks.peek() {
            if let Some(Token { kind: '{', .. }) = self.toks.peek_forward(1) {
                self.toks.next();
                self.toks.next();
                return Ok(self.parse_interpolation_as_string()?.into_owned());
            }
            todo!()
        }
        let mut buf = String::with_capacity(2);
        self.expect_char('(')?;
        buf.push('(');
        self.whitespace();

        buf.push_str(&self.expression_until_comparison()?);

        if let Some(Token { kind: ':', .. }) = self.toks.peek() {
            self.toks.next();
            self.whitespace();

            buf.push(':');
            buf.push(' ');
            let mut toks = read_until_closing_paren(self.toks)?;
            if let Some(tok) = toks.pop() {
                if tok.kind != ')' {
                    todo!()
                }
            }
            buf.push_str(&self.parse_value_as_string_from_vec(toks)?);

            self.whitespace();
            buf.push(')');
            return Ok(buf);
        } else {
            let next_tok = self.toks.peek().cloned();
            let is_angle = next_tok.map_or(false, |t| t.kind == '<' || t.kind == '>');
            if is_angle || matches!(next_tok, Some(Token { kind: '=', .. })) {
                buf.push(' ');
                // todo: remove this unwrap
                buf.push(self.toks.next().unwrap().kind);
                if is_angle && self.scan_char('=') {
                    buf.push('=');
                }
                buf.push(' ');

                self.whitespace();

                buf.push_str(&self.expression_until_comparison()?);
            }
        }

        self.expect_char(')')?;
        self.whitespace();
        buf.push(')');
        Ok(buf)
    }

    fn parse_single_media_query(&mut self) -> SassResult<String> {
        let mut buf = String::new();

        if !matches!(self.toks.peek(), Some(Token { kind: '(', .. })) {
            buf.push_str(&self.parse_identifier()?);

            self.whitespace();

            if let Some(tok) = self.toks.peek() {
                if !is_name_start(tok.kind) {
                    return Ok(buf);
                }
            }

            let ident = self.parse_identifier()?;

            self.whitespace();

            if ident.to_ascii_lowercase() == "and" {
                buf.push_str(" and ");
            } else {
                buf.push_str(&ident);

                if self.scan_identifier("and")? {
                    self.whitespace();
                    buf.push_str(" and ");
                } else {
                    return Ok(buf);
                }
            }
        }

        loop {
            self.whitespace();
            buf.push_str(&self.parse_media_feature()?);
            self.whitespace();
            if !self.scan_identifier("and")? {
                break;
            }
            buf.push_str(" and ");
        }
        Ok(buf)
    }
}
