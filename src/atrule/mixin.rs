use crate::{args::FuncArgs, scope::Scope, Token};

#[derive(Debug, Clone)]
pub(crate) struct Mixin {
    pub scope: Scope,
    pub args: FuncArgs,
    pub body: Vec<Token>,
    pub accepts_content_block: bool,
}

impl Mixin {
    pub fn new(
        scope: Scope,
        args: FuncArgs,
        body: Vec<Token>,
        accepts_content_block: bool,
    ) -> Self {
        Mixin {
            scope,
            args,
            body,
            accepts_content_block,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Content {
    pub content: Option<Vec<Token>>,
    pub content_args: Option<FuncArgs>,
    pub scope: Scope,
}

impl Content {
    pub fn new() -> Self {
        Self {
            content: None,
            content_args: None,
            scope: Scope::new(),
        }
    }
}
