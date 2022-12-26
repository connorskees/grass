use std::fmt;

use crate::{ast::KeyframesSelector, error::SassResult, lexer::Lexer, token::Token};

use super::BaseParser;

impl fmt::Display for KeyframesSelector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            KeyframesSelector::To => f.write_str("to"),
            KeyframesSelector::From => f.write_str("from"),
            KeyframesSelector::Percent(p) => write!(f, "{}%", p),
        }
    }
}

pub(crate) struct KeyframesSelectorParser<'a, 'b> {
    pub toks: &'a mut Lexer<'b>,
}

impl<'a, 'b: 'a> BaseParser<'a, 'b> for KeyframesSelectorParser<'a, 'b> {
    fn toks(&self) -> &Lexer<'b> {
        &self.toks
    }

    fn toks_mut(&mut self) -> &mut Lexer<'b> {
        &mut self.toks
    }
}

impl<'a, 'b> KeyframesSelectorParser<'a, 'b> {
    pub fn new(toks: &'a mut Lexer<'b>) -> KeyframesSelectorParser<'a, 'b> {
        KeyframesSelectorParser { toks }
    }

    pub fn parse_keyframes_selector(&mut self) -> SassResult<Vec<KeyframesSelector>> {
        let mut selectors = Vec::new();
        loop {
            self.whitespace()?;
            if self.looking_at_identifier() {
                if self.scan_identifier("to", false)? {
                    selectors.push(KeyframesSelector::To);
                } else if self.scan_identifier("from", false)? {
                    selectors.push(KeyframesSelector::From);
                } else {
                    return Err(("Expected \"to\" or \"from\".", self.toks.current_span()).into());
                }
            } else {
                selectors.push(self.parse_percentage_selector()?);
            }

            self.whitespace()?;

            if !self.scan_char(',') {
                break;
            }
        }

        Ok(selectors)
    }

    fn parse_percentage_selector(&mut self) -> SassResult<KeyframesSelector> {
        let mut buffer = String::new();

        if self.scan_char('+') {
            buffer.push('+');
        }

        if !matches!(
            self.toks.peek(),
            Some(Token {
                kind: '0'..='9' | '.',
                ..
            })
        ) {
            return Err(("Expected number.", self.toks.current_span()).into());
        }

        while matches!(
            self.toks.peek(),
            Some(Token {
                kind: '0'..='9',
                ..
            })
        ) {
            buffer.push(self.toks.next().unwrap().kind);
        }

        if self.scan_char('.') {
            buffer.push('.');

            while matches!(
                self.toks.peek(),
                Some(Token {
                    kind: '0'..='9',
                    ..
                })
            ) {
                buffer.push(self.toks.next().unwrap().kind);
            }
        }

        if self.scan_ident_char('e', false)? {
            buffer.push('e');

            if matches!(
                self.toks.peek(),
                Some(Token {
                    kind: '+' | '-',
                    ..
                })
            ) {
                buffer.push(self.toks.next().unwrap().kind);
            }

            if !matches!(
                self.toks.peek(),
                Some(Token {
                    kind: '0'..='9',
                    ..
                })
            ) {
                return Err(("Expected digit.", self.toks.current_span()).into());
            }

            while matches!(
                self.toks.peek(),
                Some(Token {
                    kind: '0'..='9',
                    ..
                })
            ) {
                buffer.push(self.toks.next().unwrap().kind);
            }
        }

        self.expect_char('%')?;

        Ok(KeyframesSelector::Percent(buffer.into_boxed_str()))
    }
}
