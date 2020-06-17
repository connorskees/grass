use std::vec::IntoIter;

use peekmore::{PeekMore, PeekMoreIterator};

use crate::{args::FuncArgs, scope::Scope, Token};

#[derive(Debug, Clone)]
pub(crate) struct Mixin {
    pub scope: Scope,
    pub args: FuncArgs,
    pub body: PeekMoreIterator<IntoIter<Token>>,
    pub accepts_content_block: bool,
}

impl Mixin {
    pub fn new(
        scope: Scope,
        args: FuncArgs,
        body: Vec<Token>,
        accepts_content_block: bool,
    ) -> Self {
        let body = body.into_iter().peekmore();
        Mixin {
            scope,
            args,
            body,
            accepts_content_block,
        }
    }
}
