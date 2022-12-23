use crate::{ast::MediaQuery, error::SassResult};

use super::Parser;

pub(crate) struct MediaQueryParser<'a> {
    parser: &'a mut Parser<'a, 'a>,
}

impl<'a> MediaQueryParser<'a> {
    pub fn new(parser: &'a mut Parser<'a, 'a>) -> MediaQueryParser<'a> {
        MediaQueryParser { parser }
    }

    pub fn parse(&mut self) -> SassResult<Vec<MediaQuery>> {
        let mut queries = Vec::new();
        loop {
            self.parser.whitespace()?;
            queries.push(self.parse_media_query()?);
            self.parser.whitespace()?;

            if !self.parser.scan_char(',') {
                break;
            }
        }

        debug_assert!(self.parser.toks.next().is_none());

        Ok(queries)
    }

    fn parse_media_query(&mut self) -> SassResult<MediaQuery> {
        if self.parser.toks.next_char_is('(') {
            let mut conditions = vec![self.parse_media_in_parens()?];
            self.parser.whitespace()?;

            let mut conjunction = true;

            if self.parser.scan_identifier("and", false)? {
                self.parser.expect_whitespace()?;
                conditions.append(&mut self.parse_media_logic_sequence("and")?);
            } else if self.parser.scan_identifier("or", false)? {
                self.parser.expect_whitespace()?;
                conjunction = false;
                conditions.append(&mut self.parse_media_logic_sequence("or")?);
            }

            return Ok(MediaQuery::condition(conditions, conjunction));
        }

        let mut modifier: Option<String> = None;
        let media_type: Option<String>;
        let identifier1 = self.parser.parse_identifier(false, false)?;

        if identifier1.to_ascii_lowercase() == "not" {
            self.parser.expect_whitespace()?;
            if !self.parser.looking_at_identifier() {
                return Ok(MediaQuery::condition(
                    vec![format!("(not {})", self.parse_media_in_parens()?)],
                    true,
                ));
            }
        }

        self.parser.whitespace()?;

        if !self.parser.looking_at_identifier() {
            return Ok(MediaQuery::media_type(Some(identifier1), None, None));
        }

        let identifier2 = self.parser.parse_identifier(false, false)?;

        if identifier2.to_ascii_lowercase() == "and" {
            self.parser.expect_whitespace()?;
            media_type = Some(identifier1);
        } else {
            self.parser.whitespace()?;
            modifier = Some(identifier1);
            media_type = Some(identifier2);
            if self.parser.scan_identifier("and", false)? {
                // For example, "@media only screen and ..."
                self.parser.expect_whitespace()?;
            } else {
                // For example, "@media only screen {"
                return Ok(MediaQuery::media_type(media_type, modifier, None));
            }
        }

        // We've consumed either `IDENTIFIER "and"` or
        // `IDENTIFIER IDENTIFIER "and"`.

        if self.parser.scan_identifier("not", false)? {
            // For example, "@media screen and not (...) {"
            self.parser.expect_whitespace()?;
            return Ok(MediaQuery::media_type(
                media_type,
                modifier,
                Some(vec![format!("(not {})", self.parse_media_in_parens()?)]),
            ));
        }

        Ok(MediaQuery::media_type(
            media_type,
            modifier,
            Some(self.parse_media_logic_sequence("and")?),
        ))
    }

    fn parse_media_in_parens(&mut self) -> SassResult<String> {
        self.parser.expect_char('(')?;
        let result = format!("({})", self.parser.declaration_value(false)?);
        self.parser.expect_char(')')?;
        Ok(result)
    }

    fn parse_media_logic_sequence(&mut self, operator: &'static str) -> SassResult<Vec<String>> {
        let mut result = Vec::new();
        loop {
            result.push(self.parse_media_in_parens()?);
            self.parser.whitespace()?;
            if !self.parser.scan_identifier(operator, false)? {
                return Ok(result);
            }
            self.parser.expect_whitespace()?;
        }
    }
}
