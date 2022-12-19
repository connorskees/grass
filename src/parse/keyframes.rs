use std::fmt;

use crate::{
    atrule::keyframes::KeyframesSelector,
    error::SassResult,
    token::Token,
    // lexer::Lexer,
    // parse::Stmt,
    // Token,
};

use super::Parser;

impl fmt::Display for KeyframesSelector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            KeyframesSelector::To => f.write_str("to"),
            KeyframesSelector::From => f.write_str("from"),
            KeyframesSelector::Percent(p) => write!(f, "{}%", p),
        }
    }
}

pub(crate) struct KeyframesSelectorParser<'a, 'b, 'c> {
    parser: &'a mut Parser<'b, 'c>,
}

impl<'a, 'b, 'c> KeyframesSelectorParser<'a, 'b, 'c> {
    pub fn new(parser: &'a mut Parser<'b, 'c>) -> Self {
        Self { parser }
    }

    pub fn parse_keyframes_selector(&mut self) -> SassResult<Vec<KeyframesSelector>> {
        let mut selectors = Vec::new();
        loop {
            self.parser.whitespace_or_comment();
            if self.parser.looking_at_identifier() {
                if self.parser.scan_identifier("to", false)? {
                    selectors.push(KeyframesSelector::To);
                } else if self.parser.scan_identifier("from", false)? {
                    selectors.push(KeyframesSelector::From);
                } else {
                    return Err((
                        "Expected \"to\" or \"from\".",
                        self.parser.toks.current_span(),
                    )
                        .into());
                }
            } else {
                selectors.push(self.parse_percentage_selector()?);
            }

            self.parser.whitespace_or_comment();

            if !self.parser.consume_char_if_exists(',') {
                break;
            }
        }

        Ok(selectors)
    }

    fn parse_percentage_selector(&mut self) -> SassResult<KeyframesSelector> {
        let mut buffer = String::new();

        if self.parser.consume_char_if_exists('+') {
            buffer.push('+');
        }

        if !matches!(
            self.parser.toks.peek(),
            Some(Token {
                kind: '0'..='9' | '.',
                ..
            })
        ) {
            return Err(("Expected number.", self.parser.toks.current_span()).into());
        }

        while matches!(
            self.parser.toks.peek(),
            Some(Token {
                kind: '0'..='9',
                ..
            })
        ) {
            buffer.push(self.parser.toks.next().unwrap().kind);
        }

        if self.parser.consume_char_if_exists('.') {
            buffer.push('.');

            while matches!(
                self.parser.toks.peek(),
                Some(Token {
                    kind: '0'..='9',
                    ..
                })
            ) {
                buffer.push(self.parser.toks.next().unwrap().kind);
            }
        }

        if self.parser.scan_ident_char('e', false)? {
            buffer.push('e');

            if matches!(
                self.parser.toks.peek(),
                Some(Token {
                    kind: '+' | '-',
                    ..
                })
            ) {
                buffer.push(self.parser.toks.next().unwrap().kind);
            }

            if !matches!(
                self.parser.toks.peek(),
                Some(Token {
                    kind: '0'..='9',
                    ..
                })
            ) {
                return Err(("Expected digit.", self.parser.toks.current_span()).into());
            }

            while matches!(
                self.parser.toks.peek(),
                Some(Token {
                    kind: '0'..='9',
                    ..
                })
            ) {
                buffer.push(self.parser.toks.next().unwrap().kind);
            }
        }

        self.parser.expect_char('%')?;

        Ok(KeyframesSelector::Percent(buffer.into_boxed_str()))
    }
}
