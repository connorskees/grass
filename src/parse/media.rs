use crate::{
    error::SassResult,
    utils::{is_name_start, peek_ident_no_interpolation},
    {Cow, Token},
};

use super::Parser;

impl<'a> Parser<'a> {
    /// Peeks to see if the `ident` is at the current position. If it is,
    /// consume the identifier
    ///
    /// This method is case insensitive
    pub fn scan_identifier(&mut self, ident: &'static str) -> SassResult<bool> {
        let mut peeked_identifier =
            match peek_ident_no_interpolation(self.toks, false, self.span_before) {
                Ok(v) => v.node,
                Err(..) => return Ok(false),
            };
        peeked_identifier.make_ascii_lowercase();
        if peeked_identifier == ident {
            self.toks.truncate_iterator_to_cursor();
            self.toks.next();
            return Ok(true);
        }
        self.toks.reset_cursor();
        Ok(false)
    }

    pub fn expression_until_comparison(&mut self) -> SassResult<Cow<'static, str>> {
        let value = self.parse_value(false, &|toks| match toks.peek() {
            Some(Token { kind: '>', .. })
            | Some(Token { kind: '<', .. })
            | Some(Token { kind: ':', .. })
            | Some(Token { kind: ')', .. }) => true,
            Some(Token { kind: '=', .. }) => {
                if matches!(toks.peek_next(), Some(Token { kind: '=', .. })) {
                    toks.reset_cursor();
                    true
                } else {
                    toks.reset_cursor();
                    false
                }
            }
            _ => false,
        })?;

        value.node.unquote().to_css_string(value.span)
    }

    pub(super) fn parse_media_query_list(&mut self) -> SassResult<String> {
        let mut buf = String::new();
        loop {
            self.whitespace_or_comment();
            buf.push_str(&self.parse_single_media_query()?);
            if !self.consume_char_if_exists(',') {
                break;
            }
            buf.push(',');
            buf.push(' ');
        }
        Ok(buf)
    }

    fn parse_media_feature(&mut self) -> SassResult<String> {
        if let Some(Token { kind: '#', .. }) = self.toks.peek() {
            self.toks.next();
            self.expect_char('{')?;
            return Ok(self.parse_interpolation_as_string()?.into_owned());
        }
        let mut buf = String::with_capacity(2);
        self.expect_char('(')?;
        buf.push('(');
        self.whitespace_or_comment();

        buf.push_str(&self.expression_until_comparison()?);

        if let Some(Token { kind: ':', .. }) = self.toks.peek() {
            self.toks.next();
            self.whitespace_or_comment();

            buf.push(':');
            buf.push(' ');

            let value = self.parse_value(false, &|toks| match toks.peek() {
                Some(Token { kind: ')', .. }) => true,
                _ => false,
            })?;
            self.expect_char(')')?;

            buf.push_str(&value.node.to_css_string(value.span)?);

            self.whitespace_or_comment();
            buf.push(')');
            return Ok(buf);
        } else {
            let next_tok = self.toks.peek().cloned();
            let is_angle = next_tok.map_or(false, |t| t.kind == '<' || t.kind == '>');
            if is_angle || matches!(next_tok, Some(Token { kind: '=', .. })) {
                buf.push(' ');
                // todo: remove this unwrap
                buf.push(self.toks.next().unwrap().kind);
                if is_angle && self.consume_char_if_exists('=') {
                    buf.push('=');
                }
                buf.push(' ');

                self.whitespace_or_comment();

                buf.push_str(&self.expression_until_comparison()?);
            }
        }

        self.expect_char(')')?;
        self.whitespace_or_comment();
        buf.push(')');
        Ok(buf)
    }

    fn parse_single_media_query(&mut self) -> SassResult<String> {
        let mut buf = String::new();

        if !matches!(self.toks.peek(), Some(Token { kind: '(', .. })) {
            buf.push_str(&self.parse_identifier()?);

            self.whitespace_or_comment();

            if let Some(tok) = self.toks.peek() {
                if !is_name_start(tok.kind) {
                    return Ok(buf);
                }
            }

            buf.push(' ');
            let ident = self.parse_identifier()?;

            self.whitespace_or_comment();

            if ident.to_ascii_lowercase() == "and" {
                buf.push_str("and ");
            } else {
                buf.push_str(&ident);

                if self.scan_identifier("and")? {
                    self.whitespace_or_comment();
                    buf.push_str(" and ");
                } else {
                    return Ok(buf);
                }
            }
        }

        loop {
            self.whitespace_or_comment();
            buf.push_str(&self.parse_media_feature()?);
            self.whitespace_or_comment();
            if !self.scan_identifier("and")? {
                break;
            }
            buf.push_str(" and ");
        }
        Ok(buf)
    }
}
