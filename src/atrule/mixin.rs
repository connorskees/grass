use crate::{args::FuncArgs, scope::Scopes, Token};

#[derive(Debug, Clone)]
pub(crate) struct Mixin {
    pub args: FuncArgs,
    pub body: Vec<Token>,
    pub accepts_content_block: bool,
    pub declared_at_root: bool,
}

impl Mixin {
    pub fn new(
        args: FuncArgs,
        body: Vec<Token>,
        accepts_content_block: bool,
        declared_at_root: bool,
    ) -> Self {
        Mixin {
            args,
            body,
            accepts_content_block,
            declared_at_root,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Content {
    pub content: Option<Vec<Token>>,
    pub content_args: Option<FuncArgs>,
    pub scopes: Scopes,
}

impl Content {
    pub const fn new() -> Self {
        Self {
            content: None,
            content_args: None,
            scopes: Scopes::new(),
        }
    }
}
