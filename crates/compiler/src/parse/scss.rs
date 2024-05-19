use std::path::Path;

use codemap::Span;

use crate::{lexer::Lexer, ContextFlags, Options};

use super::{BaseParser, StylesheetParser};

pub(crate) struct ScssParser<'a> {
    pub toks: Lexer<'a>,
    pub path: &'a Path,
    pub empty_span: Span,
    pub flags: ContextFlags,
    pub options: &'a Options<'a>,
}

impl<'a> ScssParser<'a> {
    pub fn new(
        toks: Lexer<'a>,
        options: &'a Options<'a>,
        empty_span: Span,
        file_name: &'a Path,
    ) -> Self {
        let mut flags = ContextFlags::empty();

        flags.set(ContextFlags::IS_USE_ALLOWED, true);

        ScssParser {
            toks,
            path: file_name,
            empty_span,
            flags,
            options,
        }
    }
}

impl<'a> BaseParser<'a> for ScssParser<'a> {
    fn toks(&self) -> &Lexer<'a> {
        &self.toks
    }

    fn toks_mut(&mut self) -> &mut Lexer<'a> {
        &mut self.toks
    }
}

impl<'a> StylesheetParser<'a> for ScssParser<'a> {
    fn is_plain_css(&self) -> bool {
        false
    }

    fn is_indented(&self) -> bool {
        false
    }

    fn path(&self) -> &'a Path {
        self.path
    }

    fn options(&self) -> &Options {
        self.options
    }

    fn current_indentation(&self) -> usize {
        0
    }

    fn flags(&self) -> &ContextFlags {
        &self.flags
    }

    fn flags_mut(&mut self) -> &mut ContextFlags {
        &mut self.flags
    }

    fn empty_span(&self) -> Span {
        self.empty_span
    }
}
