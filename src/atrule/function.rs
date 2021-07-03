use std::hash::{Hash, Hasher};

use codemap::Span;

use crate::{args::FuncArgs, Token};

#[derive(Debug, Clone)]
pub(crate) struct Function {
    pub args: FuncArgs,
    pub body: Vec<Token>,
    pub declared_at_root: bool,
    pos: Span,
}

impl Hash for Function {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.pos.hash(state);
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.pos == other.pos
    }
}

impl Eq for Function {}

impl Function {
    pub fn new(args: FuncArgs, body: Vec<Token>, declared_at_root: bool, pos: Span) -> Self {
        Function {
            args,
            body,
            declared_at_root,
            pos,
        }
    }
}
