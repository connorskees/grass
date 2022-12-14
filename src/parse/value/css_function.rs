use std::iter::Iterator;

use crate::{error::SassResult, parse::value_new::opposite_bracket, Token};

use super::super::Parser;

impl<'a, 'b> Parser<'a, 'b> {
    // pub(super) fn parse_calc_args(&mut self, buf: &mut String) -> SassResult<()> {
    //     buf.reserve(2);
    //     buf.push('(');
    //     let mut nesting = 0;
    //     while let Some(tok) = self.toks.next() {
    //         match tok.kind {
    //             ' ' | '\t' | '\n' => {
    //                 self.whitespace();
    //                 buf.push(' ');
    //             }
    //             '#' => {
    //                 if let Some(Token { kind: '{', pos }) = self.toks.peek() {
    //                     self.span_before = pos;
    //                     self.toks.next();
    //                     let interpolation = self.parse_interpolation()?;
    //                     buf.push_str(
    //                         &interpolation
    //                             .node
    //                             .to_css_string(interpolation.span, self.options.is_compressed())?,
    //                     );
    //                 } else {
    //                     buf.push('#');
    //                 }
    //             }
    //             '(' => {
    //                 nesting += 1;
    //                 buf.push('(');
    //             }
    //             ')' => {
    //                 if nesting == 0 {
    //                     break;
    //                 }

    //                 nesting -= 1;
    //                 buf.push(')');
    //             }
    //             q @ '\'' | q @ '"' => {
    //                 buf.push('"');
    //                 match self.parse_quoted_string(q)?.node {
    //                     Value::String(ref s, ..) => buf.push_str(s),
    //                     _ => unreachable!(),
    //                 }
    //                 buf.push('"');
    //             }
    //             c => buf.push(c),
    //         }
    //     }
    //     buf.push(')');
    //     Ok(())
    // }

    // pub(super) fn parse_progid(&mut self) -> SassResult<String> {
    //     let mut string = String::new();
    //     let mut span = match self.toks.peek() {
    //         Some(token) => token.pos(),
    //         None => {
    //             return Err(("expected \"(\".", self.span_before).into());
    //         }
    //     };

    //     while let Some(tok) = self.toks.next() {
    //         span = span.merge(tok.pos());
    //         match tok.kind {
    //             'a'..='z' | 'A'..='Z' | '.' => {
    //                 string.push(tok.kind);
    //             }
    //             '(' => {
    //                 self.parse_calc_args(&mut string)?;
    //                 break;
    //             }
    //             _ => return Err(("expected \"(\".", span).into()),
    //         }
    //     }

    //     Ok(string)
    // }

    // pub(super) fn try_parse_url(&mut self) -> SassResult<Option<String>> {
    //     let mut buf = String::from("url(");

    //     let start = self.toks.cursor();

    //     self.whitespace();

    //     while let Some(tok) = self.toks.next() {
    //         match tok.kind {
    //             '!' | '%' | '&' | '*'..='~' | '\u{80}'..=char::MAX => buf.push(tok.kind),
    //             '#' => {
    //                 if self.consume_char_if_exists('{') {
    //                     let interpolation = self.parse_interpolation()?;
    //                     match interpolation.node {
    //                         Value::String(ref s, ..) => buf.push_str(s),
    //                         v => buf.push_str(
    //                             v.to_css_string(interpolation.span, self.options.is_compressed())?
    //                                 .borrow(),
    //                         ),
    //                     };
    //                 } else {
    //                     buf.push('#');
    //                 }
    //             }
    //             ')' => {
    //                 buf.push(')');

    //                 return Ok(Some(buf));
    //             }
    //             ' ' | '\t' | '\n' | '\r' => {
    //                 self.whitespace();

    //                 if self.consume_char_if_exists(')') {
    //                     buf.push(')');

    //                     return Ok(Some(buf));
    //                 }

    //                 break;
    //             }
    //             _ => break,
    //         }
    //     }

    //     self.toks.set_cursor(start);

    //     Ok(None)
    // }

    // pub(super) fn try_parse_min_max(
    //     &mut self,
    //     fn_name: &str,
    //     allow_comma: bool,
    // ) -> SassResult<Option<String>> {
    //     let mut buf = if allow_comma {
    //         format!("{}(", fn_name)
    //     } else {
    //         String::new()
    //     };

    //     self.whitespace_or_comment();

    //     while let Some(tok) = self.toks.peek() {
    //         let kind = tok.kind;
    //         match kind {
    //             '+' | '-' | '0'..='9' => {
    //                 let number = self.parse_dimension(&|_| false)?;
    //                 buf.push_str(
    //                     &number
    //                         .node
    //                         .to_css_string(number.span, self.options.is_compressed())?,
    //                 );
    //             }
    //             '#' => {
    //                 self.toks.next();
    //                 if self.consume_char_if_exists('{') {
    //                     let interpolation = self.parse_interpolation_as_string()?;

    //                     buf.push_str(&interpolation);
    //                 } else {
    //                     return Ok(None);
    //                 }
    //             }
    //             'c' | 'C' => {
    //                 if let Some(name) = self.try_parse_min_max_function("calc")? {
    //                     buf.push_str(&name);
    //                 } else {
    //                     return Ok(None);
    //                 }
    //             }
    //             'e' | 'E' => {
    //                 if let Some(name) = self.try_parse_min_max_function("env")? {
    //                     buf.push_str(&name);
    //                 } else {
    //                     return Ok(None);
    //                 }
    //             }
    //             'v' | 'V' => {
    //                 if let Some(name) = self.try_parse_min_max_function("var")? {
    //                     buf.push_str(&name);
    //                 } else {
    //                     return Ok(None);
    //                 }
    //             }
    //             '(' => {
    //                 self.toks.next();
    //                 buf.push('(');
    //                 if let Some(val) = self.try_parse_min_max(fn_name, false)? {
    //                     buf.push_str(&val);
    //                 } else {
    //                     return Ok(None);
    //                 }
    //             }
    //             'm' | 'M' => {
    //                 self.toks.next();
    //                 let inner_fn_name = match self.toks.peek() {
    //                     Some(Token { kind: 'i', .. }) | Some(Token { kind: 'I', .. }) => {
    //                         self.toks.next();
    //                         if !matches!(
    //                             self.toks.peek(),
    //                             Some(Token { kind: 'n', .. }) | Some(Token { kind: 'N', .. })
    //                         ) {
    //                             return Ok(None);
    //                         }

    //                         "min"
    //                     }
    //                     Some(Token { kind: 'a', .. }) | Some(Token { kind: 'A', .. }) => {
    //                         self.toks.next();
    //                         if !matches!(
    //                             self.toks.peek(),
    //                             Some(Token { kind: 'x', .. }) | Some(Token { kind: 'X', .. })
    //                         ) {
    //                             return Ok(None);
    //                         }

    //                         "max"
    //                     }
    //                     _ => return Ok(None),
    //                 };

    //                 self.toks.next();

    //                 if !matches!(self.toks.peek(), Some(Token { kind: '(', .. })) {
    //                     return Ok(None);
    //                 }

    //                 self.toks.next();

    //                 if let Some(val) = self.try_parse_min_max(inner_fn_name, true)? {
    //                     buf.push_str(&val);
    //                 } else {
    //                     return Ok(None);
    //                 }
    //             }
    //             _ => return Ok(None),
    //         }

    //         self.whitespace_or_comment();

    //         let next = match self.toks.peek() {
    //             Some(tok) => tok,
    //             None => return Ok(None),
    //         };

    //         match next.kind {
    //             ')' => {
    //                 self.toks.next();
    //                 buf.push(')');
    //                 return Ok(Some(buf));
    //             }
    //             '+' | '-' | '*' | '/' => {
    //                 self.toks.next();
    //                 buf.push(' ');
    //                 buf.push(next.kind);
    //                 buf.push(' ');
    //             }
    //             ',' => {
    //                 if !allow_comma {
    //                     return Ok(None);
    //                 }
    //                 self.toks.next();
    //                 buf.push(',');
    //                 buf.push(' ');
    //             }
    //             _ => return Ok(None),
    //         }

    //         self.whitespace_or_comment();
    //     }

    //     Ok(Some(buf))
    // }

    // fn try_parse_min_max_function(&mut self, fn_name: &'static str) -> SassResult<Option<String>> {
    //     let mut ident = self.parse_identifier_no_interpolation(false)?.node;
    //     ident.make_ascii_lowercase();

    //     if ident != fn_name {
    //         return Ok(None);
    //     }

    //     if !matches!(self.toks.peek(), Some(Token { kind: '(', .. })) {
    //         return Ok(None);
    //     }

    //     self.toks.next();
    //     ident.push('(');

    //     let value = self.declaration_value(true, false, true)?;

    //     if !matches!(self.toks.peek(), Some(Token { kind: ')', .. })) {
    //         return Ok(None);
    //     }

    //     self.toks.next();

    //     ident.push_str(&value);

    //     ident.push(')');

    //     Ok(Some(ident))
    // }

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
                        let s = self.parse_identifier()?;
                        buffer.push_str(&s.node);
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
                    let before_url = self.toks.cursor();

                    if !self.scan_identifier("url", true) {
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
                        buffer.push_str(&self.__parse_identifier(false, false)?);
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
