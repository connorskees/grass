use std::path::Path;

use codemap::{CodeMap, Span};

use crate::{lexer::Lexer, ContextFlags, Options};

use super::{BaseParser, StylesheetParser};

pub(crate) struct ScssParser<'a, 'b> {
    pub toks: &'a mut Lexer<'b>,
    // todo: likely superfluous
    pub map: &'a mut CodeMap,
    pub path: &'a Path,
    pub span_before: Span,
    pub flags: ContextFlags,
    pub options: &'a Options<'a>,
}

impl<'a, 'b> ScssParser<'a, 'b> {
    pub fn new(
        toks: &'a mut Lexer<'b>,
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

impl<'a, 'b: 'a> BaseParser<'a, 'b> for ScssParser<'a, 'b> {
    fn toks(&self) -> &Lexer<'b> {
        self.toks
    }

    fn toks_mut(&mut self) -> &mut Lexer<'b> {
        self.toks
    }
}

impl<'a, 'b: 'a> StylesheetParser<'a, 'b> for ScssParser<'a, 'b> {
    fn is_plain_css(&mut self) -> bool {
        false
    }

    fn is_indented(&mut self) -> bool {
        false
    }

    fn path(&mut self) -> &'a Path {
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

    fn flags(&mut self) -> &ContextFlags {
        &self.flags
    }

    fn flags_mut(&mut self) -> &mut ContextFlags {
        &mut self.flags
    }

    fn span_before(&self) -> Span {
        self.span_before
    }
}
