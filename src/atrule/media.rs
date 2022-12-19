#![allow(dead_code)]
use std::fmt::{self, Write};

use crate::{
    error::SassResult,
    lexer::Lexer,
    parse::{Parser, Stmt},
    token::Token,
};

#[derive(Debug, Clone)]
pub(crate) struct MediaRule {
    pub query: String,
    pub body: Vec<Stmt>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) struct MediaQuery {
    pub modifier: Option<String>,
    pub media_type: Option<String>,
    pub conditions: Vec<String>,
    pub conjunction: bool,
}

struct MediaQueryParser<'a> {
    parser: &'a mut Parser<'a, 'a>,
}

// todo: move to separate file
impl<'a> MediaQueryParser<'a> {
    pub fn new(parser: &'a mut Parser<'a, 'a>) -> MediaQueryParser<'a> {
        MediaQueryParser { parser }
    }

    pub fn parse(&mut self) -> SassResult<Vec<MediaQuery>> {
        let mut queries = Vec::new();
        loop {
            self.parser.whitespace_or_comment();
            queries.push(self.parse_media_query()?);
            self.parser.whitespace_or_comment();

            if !self.parser.consume_char_if_exists(',') {
                break;
            }
        }

        debug_assert!(self.parser.toks.next().is_none());

        Ok(queries)
    }

    fn parse_media_query(&mut self) -> SassResult<MediaQuery> {
        if self.parser.toks.next_char_is('(') {
            let mut conditions = vec![self.parse_media_in_parens()?];
            self.parser.whitespace_or_comment();

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
        let mut media_type: Option<String> = None;
        let identifier1 = self.parser.__parse_identifier(false, false)?;

        if identifier1.to_ascii_lowercase() == "not" {
            self.parser.expect_whitespace()?;
            if !self.parser.looking_at_identifier() {
                return Ok(MediaQuery::condition(
                    vec![format!("(not {})", self.parse_media_in_parens()?)],
                    true,
                ));
            }
        }

        self.parser.whitespace_or_comment();

        if !self.parser.looking_at_identifier() {
            return Ok(MediaQuery::media_type(Some(identifier1), None, None));
        }

        let identifier2 = self.parser.__parse_identifier(false, false)?;

        if identifier2.to_ascii_lowercase() == "and" {
            self.parser.expect_whitespace()?;
            media_type = Some(identifier1);
        } else {
            self.parser.whitespace_or_comment();
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
            self.parser.whitespace_or_comment();
            if !self.parser.scan_identifier(operator, false)? {
                return Ok(result);
            }
            self.parser.expect_whitespace()?;
        }
    }
}

impl MediaQuery {
    pub fn is_condition(&self) -> bool {
        self.modifier.is_none() && self.media_type.is_none()
    }

    pub fn matches_all_types(&self) -> bool {
        self.media_type.is_none()
            || self
                .media_type
                .as_ref()
                .map_or(false, |v| v.to_ascii_lowercase() == "all")
    }

    pub fn condition(
        conditions: Vec<String>,
        // default=true
        conjunction: bool,
    ) -> Self {
        Self {
            modifier: None,
            media_type: None,
            conditions,
            conjunction,
        }
    }

    pub fn media_type(
        media_type: Option<String>,
        modifier: Option<String>,
        conditions: Option<Vec<String>>,
    ) -> Self {
        Self {
            modifier,
            conjunction: true,
            media_type,
            conditions: conditions.unwrap_or_default(),
        }
    }

    pub fn parse_list(list: String, parser: &mut Parser) -> SassResult<Vec<Self>> {
        let mut toks = Lexer::new(
            list.chars()
                .map(|x| Token::new(parser.span_before, x))
                .collect(),
        );

        let mut parser = Parser {
            toks: &mut toks,
            map: parser.map,
            path: parser.path,
            is_plain_css: false,
            span_before: parser.span_before,
            flags: parser.flags,
            options: parser.options,
        };

        MediaQueryParser::new(&mut parser).parse()
    }

    #[allow(clippy::if_not_else)]
    pub fn merge(&self, other: &Self) -> MediaQueryMergeResult {
        if !self.conjunction || !other.conjunction {
            return MediaQueryMergeResult::Unrepresentable;
        }

        let this_modifier = self.modifier.as_ref().map(|m| m.to_ascii_lowercase());
        let this_type = self.media_type.as_ref().map(|m| m.to_ascii_lowercase());
        let other_modifier = other.modifier.as_ref().map(|m| m.to_ascii_lowercase());
        let other_type = other.media_type.as_ref().map(|m| m.to_ascii_lowercase());

        if this_type.is_none() && other_type.is_none() {
            return MediaQueryMergeResult::Success(Self::condition(
                self.conditions
                    .iter()
                    .chain(&other.conditions)
                    .cloned()
                    .collect(),
                true,
            ));
        }

        let modifier;
        let media_type;
        let conditions;

        if (this_modifier.as_deref() == Some("not")) != (other_modifier.as_deref() == Some("not")) {
            if this_modifier == other_modifier {
                let negative_conditions = if this_modifier.as_deref() == Some("not") {
                    &self.conditions
                } else {
                    &other.conditions
                };

                let positive_conditions = if this_modifier.as_deref() == Some("not") {
                    &other.conditions
                } else {
                    &self.conditions
                };

                // If the negative conditions are a subset of the positive conditions, the
                // query is empty. For example, `not screen and (color)` has no
                // intersection with `screen and (color) and (grid)`.
                //
                // However, `not screen and (color)` *does* intersect with `screen and
                // (grid)`, because it means `not (screen and (color))` and so it allows
                // a screen with no color but with a grid.

                if negative_conditions
                    .iter()
                    .all(|feat| positive_conditions.contains(feat))
                {
                    return MediaQueryMergeResult::Empty;
                }

                return MediaQueryMergeResult::Unrepresentable;
            } else if self.matches_all_types() || other.matches_all_types() {
                return MediaQueryMergeResult::Unrepresentable;
            }

            if this_modifier.as_deref() == Some("not") {
                modifier = &other_modifier;
                media_type = &other_type;
                conditions = other.conditions.clone();
            } else {
                modifier = &this_modifier;
                media_type = &this_type;
                conditions = self.conditions.clone();
            }
        } else if this_modifier.as_deref() == Some("not") {
            debug_assert_eq!(other_modifier.as_deref(), Some("not"));

            // CSS has no way of representing "neither screen nor print".
            if this_type != other_type {
                return MediaQueryMergeResult::Unrepresentable;
            }

            let more_conditions = if self.conditions.len() > other.conditions.len() {
                &self.conditions
            } else {
                &other.conditions
            };

            let fewer_conditions = if self.conditions.len() > other.conditions.len() {
                &other.conditions
            } else {
                &self.conditions
            };

            // If one set of conditions is a superset of the other, use those conditions
            // because they're strictly narrower.
            if fewer_conditions
                .iter()
                .all(|feat| more_conditions.contains(feat))
            {
                modifier = &this_modifier; // "not"
                media_type = &this_type;
                conditions = more_conditions.clone();
            } else {
                // Otherwise, there's no way to represent the intersection.
                return MediaQueryMergeResult::Unrepresentable;
            }
        } else if self.matches_all_types() {
            modifier = &other_modifier;

            // Omit the type if either input query did, since that indicates that they
            // aren't targeting a browser that requires "all and".
            media_type = if other.matches_all_types() && this_type.is_none() {
                &None
            } else {
                &other_type
            };

            conditions = self
                .conditions
                .iter()
                .chain(&other.conditions)
                .cloned()
                .collect();
        } else if other.matches_all_types() {
            modifier = &this_modifier;
            media_type = &this_type;
            conditions = self
                .conditions
                .iter()
                .chain(&other.conditions)
                .cloned()
                .collect();
        } else if this_type != other_type {
            return MediaQueryMergeResult::Empty;
        } else {
            if this_modifier.is_some() {
                modifier = &this_modifier;
            } else {
                modifier = &other_modifier;
            }

            media_type = &this_type;
            conditions = self
                .conditions
                .iter()
                .chain(&other.conditions)
                .cloned()
                .collect();
        }

        MediaQueryMergeResult::Success(MediaQuery {
            media_type: if media_type == &this_type {
                self.media_type.clone()
            } else {
                other.media_type.clone()
            },
            modifier: if modifier == &this_modifier {
                self.modifier.clone()
            } else {
                other.modifier.clone()
            },
            conditions,
            conjunction: true,
        })
    }
}

impl fmt::Display for MediaQuery {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(modifier) = &self.modifier {
            f.write_str(modifier)?;
            f.write_char(' ')?;
        }

        if let Some(media_type) = &self.media_type {
            f.write_str(media_type)?;
            if !&self.conditions.is_empty() {
                f.write_str(" and ")?;
            }
        }

        f.write_str(&self.conditions.join(" and "))
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum MediaQueryMergeResult {
    Empty,
    Unrepresentable,
    Success(MediaQuery),
}
