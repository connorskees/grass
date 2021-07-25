use crate::{
    error::SassResult,
    utils::is_name_start,
    {Cow, Token},
};

use super::Parser;

impl<'a, 'b> Parser<'a, 'b> {
    /// Peeks to see if the `ident` is at the current position. If it is,
    /// consume the identifier
    pub fn scan_identifier(&mut self, ident: &'static str, case_insensitive: bool) -> bool {
        let start = self.toks.cursor();

        let mut peeked_identifier = match self.parse_identifier_no_interpolation(false) {
            Ok(v) => v.node,
            Err(..) => {
                self.toks.set_cursor(start);
                return false;
            }
        };

        if case_insensitive {
            peeked_identifier.make_ascii_lowercase();
        }

        if peeked_identifier == ident {
            return true;
        }

        self.toks.set_cursor(start);

        false
    }

    pub fn expression_until_comparison(&mut self) -> SassResult<Cow<'static, str>> {
        let value = self.parse_value(false, &|parser| match parser.toks.peek() {
            Some(Token { kind: '>', .. })
            | Some(Token { kind: '<', .. })
            | Some(Token { kind: ':', .. })
            | Some(Token { kind: ')', .. }) => true,
            Some(Token { kind: '=', .. }) => {
                let is_double_eq = matches!(parser.toks.peek_next(), Some(Token { kind: '=', .. }));
                parser.toks.reset_cursor();
                // if it is a double eq, then parse as normal
                //
                // otherwise, it is a single eq and we should
                // treat it as a comparison
                !is_double_eq
            }
            _ => false,
        })?;

        value
            .node
            .unquote()
            .to_css_string(value.span, self.options.is_compressed())
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
        if self.consume_char_if_exists('#') {
            self.expect_char('{')?;
            return Ok(self.parse_interpolation_as_string()?.into_owned());
        }

        let mut buf = String::with_capacity(2);
        self.expect_char('(')?;
        buf.push('(');
        self.whitespace_or_comment();

        buf.push_str(&self.expression_until_comparison()?);

        if self.consume_char_if_exists(':') {
            self.whitespace_or_comment();

            buf.push(':');
            buf.push(' ');

            let value = self.parse_value(false, &|parser| {
                matches!(parser.toks.peek(), Some(Token { kind: ')', .. }))
            })?;
            self.expect_char(')')?;

            buf.push_str(
                &value
                    .node
                    .to_css_string(value.span, self.options.is_compressed())?,
            );

            self.whitespace_or_comment();
            buf.push(')');
            return Ok(buf);
        }

        let next_tok = self.toks.peek();
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

                if self.scan_identifier("and", true) {
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
            if !self.scan_identifier("and", true) {
                break;
            }
            buf.push_str(" and ");
        }
        Ok(buf)
    }
}
