use crate::{args::FuncArgs, Token};

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
    /// The literal block, serialized as a list of tokens
    pub content: Option<Vec<Token>>,

    /// Optional args, e.g. `@content(a, b, c);`
    pub content_args: Option<FuncArgs>,

    /// The number of scopes at the use of `@include`
    ///
    /// This is used to "reset" back to the state of the `@include`
    /// without actually cloning the scope or putting it in an `Rc`
    pub scope_len: usize,

    /// Whether or not the mixin this `@content` block is inside of was
    /// declared in the global scope
    pub declared_at_root: bool,
}
