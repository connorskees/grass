use codemap::Span;

use crate::args::FuncArgs;
use crate::scope::Scope;
use crate::Token;

#[derive(Debug, Clone)]
pub(crate) struct Function {
    pub scope: Scope,
    pub args: FuncArgs,
    pub body: Vec<Token>,
    pos: Span,
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.pos == other.pos
    }
}

impl Eq for Function {}

impl Function {
    pub fn new(scope: Scope, args: FuncArgs, body: Vec<Token>, pos: Span) -> Self {
        Function {
            scope,
            args,
            body,
            pos,
        }
    }
}
