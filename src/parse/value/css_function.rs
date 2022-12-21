use std::iter::Iterator;

use crate::{error::SassResult, utils::opposite_bracket, Token};

use super::super::Parser;

impl<'a, 'b> Parser<'a, 'b> {
    pub(crate) fn declaration_value(&mut self, allow_empty: bool) -> SassResult<String> {
        let mut buffer = String::new();

        let mut brackets = Vec::new();
        let mut wrote_newline = false;

        while let Some(tok) = self.toks.peek() {
            match tok.kind {
                '\\' => {
                    self.toks.next();
                    buffer.push_str(&self.parse_escape(true)?);
                    wrote_newline = false;
                }
                '"' | '\'' => {
                    buffer.push_str(&self.fallible_raw_text(Self::parse_string)?);
                    wrote_newline = false;
                }
                '/' => {
                    if matches!(self.toks.peek_n(1), Some(Token { kind: '*', .. })) {
                        buffer.push_str(&self.fallible_raw_text(Self::skip_loud_comment)?);
                    } else {
                        buffer.push('/');
                        self.toks.next();
                    }

                    wrote_newline = false;
                }
                '#' => {
                    if matches!(self.toks.peek_n(1), Some(Token { kind: '{', .. })) {
                        let s = self.parse_identifier(false, false)?;
                        buffer.push_str(&s);
                    } else {
                        buffer.push('#');
                        self.toks.next();
                    }

                    wrote_newline = false;
                }
                c @ (' ' | '\t') => {
                    if wrote_newline
                        || !self
                            .toks
                            .peek_n(1)
                            .map_or(false, |tok| tok.kind.is_ascii_whitespace())
                    {
                        buffer.push(c);
                    }

                    self.toks.next();
                }
                '\n' | '\r' => {
                    if !wrote_newline {
                        buffer.push('\n');
                    }

                    wrote_newline = true;

                    self.toks.next();
                }

                '[' | '(' | '{' => {
                    buffer.push(tok.kind);

                    self.toks.next();

                    brackets.push(opposite_bracket(tok.kind));
                    wrote_newline = false;
                }
                ']' | ')' | '}' => {
                    if let Some(end) = brackets.pop() {
                        buffer.push(tok.kind);
                        self.expect_char(end)?;
                    } else {
                        break;
                    }

                    wrote_newline = false;
                }
                ';' => {
                    if brackets.is_empty() {
                        break;
                    }

                    self.toks.next();
                    buffer.push(';');
                    wrote_newline = false;
                }
                'u' | 'U' => {
                    // let before_url = self.toks.cursor();

                    if !self.scan_identifier("url", false)? {
                        buffer.push(tok.kind);
                        self.toks.next();
                        wrote_newline = false;
                        continue;
                    }

                    todo!()
                    // if let Some(contents) = self.try_parse_url()? {
                    //     buffer.push_str(&contents);
                    // } else {
                    //     self.toks.set_cursor(before_url);
                    //     buffer.push(tok.kind);
                    //     self.toks.next();
                    // }

                    // wrote_newline = false;
                }
                c => {
                    if self.looking_at_identifier() {
                        buffer.push_str(&self.parse_identifier(false, false)?);
                    } else {
                        self.toks.next();
                        buffer.push(c);
                    }

                    wrote_newline = false;
                }
            }
        }

        if let Some(last) = brackets.pop() {
            self.expect_char(last)?;
        }

        if !allow_empty && buffer.is_empty() {
            return Err(("Expected token.", self.span_before).into());
        }

        Ok(buffer)
    }
}
