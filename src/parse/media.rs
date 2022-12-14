use codemap::Spanned;

use crate::{
    error::SassResult,
    utils::{is_name, is_name_start},
    Token,
};

use super::{value_new::AstExpr, Interpolation, Parser};

impl<'a, 'b> Parser<'a, 'b> {
    fn consume_identifier(&mut self, ident: &str, case_insensitive: bool) -> bool {
        let start = self.toks.cursor();
        for c in ident.chars() {
            if self.consume_char_if_exists(c) {
                continue;
            }

            // todo: can be optimized
            if case_insensitive
                && (self.consume_char_if_exists(c.to_ascii_lowercase())
                    || self.consume_char_if_exists(c.to_ascii_uppercase()))
            {
                continue;
            }

            self.toks.set_cursor(start);
            return false;
        }

        true
    }

    // todo: duplicated in selector code
    fn looking_at_identifier_body(&mut self) -> bool {
        matches!(self.toks.peek(), Some(t) if is_name(t.kind) || t.kind == '\\')
    }

    /// Peeks to see if the `ident` is at the current position. If it is,
    /// consume the identifier
    pub fn scan_identifier(&mut self, ident: &'static str, case_insensitive: bool) -> bool {
        if !self.looking_at_identifier() {
            return false;
        }

        let start = self.toks.cursor();

        if self.consume_identifier(ident, case_insensitive) && !self.looking_at_identifier_body() {
            return true;
        } else {
            self.toks.set_cursor(start);
            return false;
        }
    }

    pub fn expression_until_comparison(&mut self) -> SassResult<Spanned<AstExpr>> {
        let value = self.parse_expression(
            Some(&|parser| {
                match parser.toks.peek() {
                    Some(Token { kind: '>', .. })
                    | Some(Token { kind: '<', .. })
                    | Some(Token { kind: ':', .. })
                    | Some(Token { kind: ')', .. }) => true,
                    Some(Token { kind: '=', .. }) => {
                        let is_double_eq =
                            matches!(parser.toks.peek_next(), Some(Token { kind: '=', .. }));
                        parser.toks.reset_cursor();
                        // if it is a double eq, then parse as normal
                        //
                        // otherwise, it is a single eq and we should
                        // treat it as a comparison
                        !is_double_eq
                    }
                    _ => false,
                }
            }),
            None,
            None,
        )?;
        Ok(value)
    }

    pub(super) fn parse_media_query_list(&mut self) -> SassResult<Interpolation> {
        let mut buf = Interpolation::new(self.span_before);
        loop {
            self.whitespace_or_comment();
            buf.add_interpolation(self.parse_single_media_query()?);
            if !self.consume_char_if_exists(',') {
                break;
            }
            buf.add_token(Token {
                kind: ',',
                pos: self.span_before,
            });
            buf.add_token(Token {
                kind: ' ',
                pos: self.span_before,
            });
        }
        Ok(buf)
    }

    fn parse_media_feature(&mut self) -> SassResult<Interpolation> {
        let mut buf = Interpolation::new(self.span_before);

        if self.consume_char_if_exists('#') {
            self.expect_char('{')?;
            todo!()
            // buf.add_expr(self.parse_interpolated_string()?);
            // return Ok(buf);
        };
        buf.add_token(self.expect_char('(')?);
        self.whitespace_or_comment();

        buf.add_expr(self.expression_until_comparison()?);

        if self.consume_char_if_exists(':') {
            self.whitespace_or_comment();

            buf.add_token(Token {
                kind: ':',
                pos: self.span_before,
            });
            buf.add_token(Token {
                kind: ' ',
                pos: self.span_before,
            });

            let value = self.parse_expression(
                Some(&|parser| matches!(parser.toks.peek(), Some(Token { kind: ')', .. }))),
                None,
                None,
            )?;
            self.expect_char(')')?;

            buf.add_expr(value);

            self.whitespace_or_comment();
            buf.add_char(')');
            return Ok(buf);
        }

        let next_tok = self.toks.peek();
        let is_angle = next_tok.map_or(false, |t| t.kind == '<' || t.kind == '>');
        if is_angle || matches!(next_tok, Some(Token { kind: '=', .. })) {
            buf.add_char(' ');
            // todo: remove this unwrap
            buf.add_token(self.toks.next().unwrap());
            if is_angle && self.consume_char_if_exists('=') {
                buf.add_char('=');
            }
            buf.add_char(' ');

            self.whitespace_or_comment();

            buf.add_expr(self.expression_until_comparison()?);
        }

        self.expect_char(')')?;
        self.whitespace_or_comment();
        buf.add_char(')');
        Ok(buf)
    }

    fn parse_single_media_query(&mut self) -> SassResult<Interpolation> {
        let mut buf = Interpolation::new(self.span_before);

        if !matches!(self.toks.peek(), Some(Token { kind: '(', .. })) {
            buf.add_string(Spanned {
                node: self.__parse_identifier(false, false)?,
                span: self.span_before,
            });

            self.whitespace_or_comment();

            if let Some(tok) = self.toks.peek() {
                if !is_name_start(tok.kind) {
                    return Ok(buf);
                }
            }

            buf.add_token(Token {
                kind: ' ',
                pos: self.span_before,
            });
            let ident = self.__parse_identifier(false, false)?;

            self.whitespace_or_comment();

            if ident.to_ascii_lowercase() == "and" {
                buf.add_string(Spanned {
                    node: "and ".to_owned(),
                    span: self.span_before,
                });
            } else {
                buf.add_string(Spanned {
                    node: ident,
                    span: self.span_before,
                });

                if self.scan_identifier("and", true) {
                    self.whitespace_or_comment();
                    buf.add_string(Spanned {
                        node: " and ".to_owned(),
                        span: self.span_before,
                    });
                } else {
                    return Ok(buf);
                }
            }
        }

        loop {
            self.whitespace_or_comment();
            buf.add_interpolation(self.parse_media_feature()?);
            self.whitespace_or_comment();
            if !self.scan_identifier("and", true) {
                break;
            }
            buf.add_string(Spanned {
                node: " and ".to_owned(),
                span: self.span_before,
            });
        }
        Ok(buf)
    }
}
