//! Consume tokens without allocating

use crate::{error::SassResult, Token};

use super::Parser;

impl<'a> Parser<'a> {
    pub(super) fn throw_away_until_newline(&mut self) {
        while let Some(tok) = self.toks.next() {
            if tok.kind == '\n' {
                break;
            }
        }
    }

    pub(super) fn throw_away_quoted_string(&mut self, q: char) -> SassResult<()> {
        while let Some(tok) = self.toks.next() {
            match tok.kind {
                '"' if q == '"' => {
                    return Ok(());
                }
                '\'' if q == '\'' => {
                    return Ok(());
                }
                '\\' => {
                    if self.toks.next().is_none() {
                        return Err((format!("Expected {}.", q), tok.pos).into());
                    }
                }
                '#' => {
                    self.toks.next();
                    self.throw_away_until_closing_curly_brace()?;
                }
                _ => {}
            }
        }
        return Err((format!("Expected {}.", q), self.span_before).into());
    }

    pub(super) fn throw_away_until_open_curly_brace(&mut self) -> SassResult<()> {
        while let Some(tok) = self.toks.next() {
            match tok.kind {
                '{' => return Ok(()),
                '/' => {
                    match self.toks.peek() {
                        Some(Token { kind: '/', .. }) => self.throw_away_until_newline(),
                        _ => {}
                    };
                    continue;
                }
                '\\' | '#' => {
                    self.toks.next();
                }
                q @ '"' | q @ '\'' => {
                    self.throw_away_quoted_string(q)?;
                    continue;
                }
                _ => {}
            }
        }
        Err(("expected \"{\".", self.span_before).into())
    }

    pub(super) fn throw_away_until_closing_curly_brace(&mut self) -> SassResult<()> {
        let mut nesting = 0;
        while let Some(tok) = self.toks.next() {
            match tok.kind {
                q @ '"' | q @ '\'' => {
                    self.throw_away_quoted_string(q)?;
                }
                '{' => {
                    nesting += 1;
                }
                '}' => {
                    if nesting == 0 {
                        return Ok(());
                    } else {
                        nesting -= 1;
                    }
                }
                '/' => match self.toks.peek() {
                    Some(Token { kind: '/', .. }) => {
                        self.throw_away_until_newline();
                    }
                    Some(..) | None => continue,
                },
                '(' => {
                    self.throw_away_until_closing_paren()?;
                }
                '\\' => {
                    self.toks.next();
                }
                _ => {}
            }
        }
        Err(("expected \"}\".", self.span_before).into())
    }

    pub(super) fn throw_away_until_closing_paren(&mut self) -> SassResult<()> {
        let mut scope = 0;
        while let Some(tok) = self.toks.next() {
            match tok.kind {
                ')' => {
                    if scope < 1 {
                        return Ok(());
                    } else {
                        scope -= 1;
                    }
                }
                '(' => scope += 1,
                '"' | '\'' => {
                    self.throw_away_quoted_string(tok.kind)?;
                }
                '\\' => {
                    match self.toks.next() {
                        Some(tok) => tok,
                        None => continue,
                    };
                }
                _ => {}
            }
        }
        Err(("expected \")\".", self.span_before).into())
    }
}
