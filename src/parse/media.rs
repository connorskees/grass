use codemap::Spanned;

use crate::{
    ast::{AstExpr, Interpolation},
    error::SassResult,
    utils::is_name,
    Token,
};

use super::Parser;

impl<'a, 'b> Parser<'a, 'b> {
    fn consume_identifier(&mut self, ident: &str, case_sensitive: bool) -> SassResult<bool> {
        for c in ident.chars() {
            if !self.scan_ident_char(c, case_sensitive)? {
                return Ok(false);
            }
        }

        Ok(true)
    }

    pub(crate) fn scan_ident_char(&mut self, c: char, case_sensitive: bool) -> SassResult<bool> {
        let matches = |actual: char| {
            if case_sensitive {
                actual == c
            } else {
                actual.to_ascii_lowercase() == c.to_ascii_lowercase()
            }
        };

        Ok(match self.toks.peek() {
            Some(Token { kind, .. }) if matches(kind) => {
                self.toks.next();
                true
            }
            Some(Token { kind: '\\', .. }) => {
                let start = self.toks.cursor();
                if matches(self.consume_escaped_char()?) {
                    return Ok(true);
                }
                self.toks.set_cursor(start);
                false
            }
            Some(..) | None => false,
        })
    }

    pub(crate) fn expect_ident_char(&mut self, c: char, case_sensitive: bool) -> SassResult<()> {
        if self.scan_ident_char(c, case_sensitive)? {
            return Ok(());
        }

        Err((format!("Expected \"{}\".", c), self.toks.current_span()).into())
    }

    pub(crate) fn looking_at_identifier_body(&mut self) -> bool {
        matches!(self.toks.peek(), Some(t) if is_name(t.kind) || t.kind == '\\')
    }

    /// Peeks to see if the `ident` is at the current position. If it is,
    /// consume the identifier
    pub fn scan_identifier(
        &mut self,
        ident: &'static str,
        // default=false
        case_sensitive: bool,
    ) -> SassResult<bool> {
        if !self.looking_at_identifier() {
            return Ok(false);
        }

        let start = self.toks.cursor();

        if self.consume_identifier(ident, case_sensitive)? && !self.looking_at_identifier_body() {
            Ok(true)
        } else {
            self.toks.set_cursor(start);
            Ok(false)
        }
    }

    pub fn expression_until_comparison(&mut self) -> SassResult<Spanned<AstExpr>> {
        let value = self.parse_expression(
            Some(&|parser| {
                Ok(match parser.toks.peek() {
                    Some(Token { kind: '>', .. }) | Some(Token { kind: '<', .. }) => true,
                    Some(Token { kind: '=', .. }) => {
                        !matches!(parser.toks.peek_n(1), Some(Token { kind: '=', .. }))
                    }
                    _ => false,
                })
            }),
            None,
            None,
        )?;
        Ok(value)
    }

    pub(super) fn parse_media_query_list(&mut self) -> SassResult<Interpolation> {
        let mut buf = Interpolation::new();
        loop {
            self.whitespace_or_comment();
            self.parse_media_query(&mut buf)?;
            self.whitespace_or_comment();
            if !self.consume_char_if_exists(',') {
                break;
            }
            buf.add_char(',');
            buf.add_char(' ');
        }
        Ok(buf)
    }

    pub(crate) fn expect_whitespace(&mut self) -> SassResult<()> {
        if !matches!(
            self.toks.peek(),
            Some(Token {
                kind: ' ' | '\t' | '\n' | '\r',
                ..
            })
        ) && !self.scan_comment()?
        {
            return Err(("Expected whitespace.", self.toks.current_span()).into());
        }

        self.whitespace_or_comment();

        Ok(())
    }

    fn parse_media_in_parens(&mut self, buf: &mut Interpolation) -> SassResult<()> {
        self.expect_char('(')?;
        buf.add_char('(');
        self.whitespace_or_comment();

        if matches!(self.toks.peek(), Some(Token { kind: '(', .. })) {
            self.parse_media_in_parens(buf)?;
            self.whitespace_or_comment();

            if self.scan_identifier("and", false)? {
                buf.add_string(" and ".to_owned());
                self.expect_whitespace()?;
                self.parse_media_logic_sequence(buf, "and")?;
            } else if self.scan_identifier("or", false)? {
                buf.add_string(" or ".to_owned());
                self.expect_whitespace()?;
                self.parse_media_logic_sequence(buf, "or")?;
            }
        } else if self.scan_identifier("not", false)? {
            buf.add_string("not ".to_owned());
            self.expect_whitespace()?;
            self.parse_media_or_interpolation(buf)?;
        } else {
            buf.add_expr(self.expression_until_comparison()?);

            if self.consume_char_if_exists(':') {
                self.whitespace_or_comment();
                buf.add_char(':');
                buf.add_char(' ');
                buf.add_expr(self.parse_expression(None, None, None)?);
            } else {
                let next = self.toks.peek();
                if matches!(
                    next,
                    Some(Token {
                        kind: '<' | '>' | '=',
                        ..
                    })
                ) {
                    let next = next.unwrap().kind;
                    buf.add_char(' ');
                    buf.add_token(self.toks.next().unwrap());

                    if (next == '<' || next == '>') && self.consume_char_if_exists('=') {
                        buf.add_char('=');
                    }

                    buf.add_char(' ');

                    self.whitespace_or_comment();

                    buf.add_expr(self.expression_until_comparison()?);

                    if (next == '<' || next == '>') && self.consume_char_if_exists(next) {
                        buf.add_char(' ');
                        buf.add_char(next);

                        if self.consume_char_if_exists('=') {
                            buf.add_char('=');
                        }

                        buf.add_char(' ');

                        self.whitespace_or_comment();
                        buf.add_expr(self.expression_until_comparison()?);
                    }
                }
            }
        }

        self.expect_char(')')?;
        self.whitespace_or_comment();
        buf.add_char(')');

        Ok(())
    }

    fn parse_media_logic_sequence(
        &mut self,
        buf: &mut Interpolation,
        operator: &'static str,
    ) -> SassResult<()> {
        loop {
            self.parse_media_or_interpolation(buf)?;
            self.whitespace_or_comment();

            if !self.scan_identifier(operator, false)? {
                return Ok(());
            }

            self.expect_whitespace()?;

            buf.add_char(' ');
            buf.add_string(operator.to_owned());
            buf.add_char(' ');
        }
    }

    fn parse_media_or_interpolation(&mut self, buf: &mut Interpolation) -> SassResult<()> {
        if self.toks.next_char_is('#') {
            buf.add_interpolation(self.parse_single_interpolation()?);
        } else {
            self.parse_media_in_parens(buf)?;
        }

        Ok(())
    }

    fn parse_media_query(&mut self, buf: &mut Interpolation) -> SassResult<()> {
        if matches!(self.toks.peek(), Some(Token { kind: '(', .. })) {
            self.parse_media_in_parens(buf)?;
            self.whitespace_or_comment();

            if self.scan_identifier("and", false)? {
                buf.add_string(" and ".to_owned());
                self.expect_whitespace()?;
                self.parse_media_logic_sequence(buf, "and")?;
            } else if self.scan_identifier("or", false)? {
                buf.add_string(" or ".to_owned());
                self.expect_whitespace()?;
                self.parse_media_logic_sequence(buf, "or")?;
            }

            return Ok(());
        }

        let ident1 = self.parse_interpolated_identifier()?;

        if ident1.as_plain().unwrap_or("").to_ascii_lowercase() == "not" {
            // For example, "@media not (...) {"
            self.expect_whitespace()?;
            if !self.looking_at_interpolated_identifier() {
                buf.add_string("not ".to_owned());
                self.parse_media_or_interpolation(buf)?;
                return Ok(());
            }
        }

        self.whitespace_or_comment();
        buf.add_interpolation(ident1);
        if !self.looking_at_interpolated_identifier() {
            // For example, "@media screen {".
            return Ok(());
        }

        buf.add_char(' ');

        let ident2 = self.parse_interpolated_identifier()?;

        if ident2.as_plain().unwrap_or("").to_ascii_lowercase() == "and" {
            self.expect_whitespace()?;
            // For example, "@media screen and ..."
            buf.add_string(" and ".to_owned());
        } else {
            self.whitespace_or_comment();
            buf.add_interpolation(ident2);

            if self.scan_identifier("and", false)? {
                // For example, "@media only screen and ..."
                self.expect_whitespace()?;
                buf.add_string(" and ".to_owned());
            } else {
                // For example, "@media only screen {"
                return Ok(());
            }
        }

        // We've consumed either `IDENTIFIER "and"` or
        // `IDENTIFIER IDENTIFIER "and"`.

        if self.scan_identifier("not", false)? {
            // For example, "@media screen and not (...) {"
            self.expect_whitespace()?;
            buf.add_string("not ".to_owned());
            self.parse_media_or_interpolation(buf)?;
            return Ok(());
        }

        self.parse_media_logic_sequence(buf, "and")?;

        Ok(())
    }
}
