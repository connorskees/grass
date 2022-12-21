use std::collections::HashSet;

use crate::{ast::AtRootQuery, error::SassResult};

use super::Parser;

pub(crate) struct AtRootQueryParser<'a> {
    parser: &'a mut Parser<'a, 'a>,
}

impl<'a> AtRootQueryParser<'a> {
    pub fn new(parser: &'a mut Parser<'a, 'a>) -> AtRootQueryParser<'a> {
        AtRootQueryParser { parser }
    }

    pub fn parse(&mut self) -> SassResult<AtRootQuery> {
        self.parser.expect_char('(')?;
        self.parser.whitespace()?;
        let include = self.parser.scan_identifier("with", false)?;

        if !include {
            self.parser.expect_identifier("without", false)?;
        }

        self.parser.whitespace()?;
        self.parser.expect_char(':')?;
        self.parser.whitespace()?;

        let mut names = HashSet::new();

        loop {
            names.insert(self.parser.parse_identifier(false, false)?);

            if !self.parser.looking_at_identifier() {
                break;
            }
        }

        self.parser.expect_char(')')?;
        self.parser.expect_done()?;

        Ok(AtRootQuery::new(include, names))
    }
}
