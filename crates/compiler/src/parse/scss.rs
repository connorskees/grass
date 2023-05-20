use std::path::Path;

use codemap::{CodeMap, Span};

use crate::{lexer::Lexer, ContextFlags, Options};

use super::{BaseParser, StylesheetParser};

pub(crate) struct ScssParser<'a> {
    pub toks: Lexer<'a>,
    // todo: likely superfluous
    pub map: &'a mut CodeMap,
    pub path: &'a Path,
    pub span_before: Span,
    pub flags: ContextFlags,
    pub options: &'a Options<'a>,
}

impl<'a> ScssParser<'a> {
    pub fn new(
        toks: Lexer<'a>,
        map: &'a mut CodeMap,
        options: &'a Options<'a>,
        span_before: Span,
        file_name: &'a Path,
    ) -> Self {
        let mut flags = ContextFlags::empty();

        flags.set(ContextFlags::IS_USE_ALLOWED, true);

        ScssParser {
            toks,
            map,
            path: file_name,
            span_before,
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

    fn map(&mut self) -> &mut CodeMap {
        self.map
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

    fn span_before(&self) -> Span {
        self.span_before
    }
}
