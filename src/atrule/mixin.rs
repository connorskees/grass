use std::fmt;

use crate::{
    args::{CallArgs, FuncArgs},
    error::SassResult,
    parse::{AstNode, Parser},
    Token,
};

pub(crate) type BuiltinMixin = fn(CallArgs, &mut Parser<'_>) -> SassResult<Vec<AstNode>>;

#[derive(Clone)]
pub(crate) enum Mixin {
    UserDefined(UserDefinedMixin),
    Builtin(BuiltinMixin),
}

impl fmt::Debug for Mixin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UserDefined(u) => f
                .debug_struct("UserDefinedMixin")
                .field("args", &u.args)
                .field("body", &u.body)
                .field("accepts_content_block", &u.accepts_content_block)
                .field("declared_at_root", &u.declared_at_root)
                .finish(),
            Self::Builtin(..) => f.debug_struct("BuiltinMixin").finish(),
        }
    }
}

impl Mixin {
    pub fn new_user_defined(
        args: FuncArgs,
        body: Vec<Token>,
        accepts_content_block: bool,
        declared_at_root: bool,
    ) -> Self {
        Mixin::UserDefined(UserDefinedMixin::new(
            args,
            body,
            accepts_content_block,
            declared_at_root,
        ))
    }
}

#[derive(Debug, Clone)]
pub(crate) struct UserDefinedMixin {
    pub args: FuncArgs,
    pub body: Vec<Token>,
    pub accepts_content_block: bool,
    pub declared_at_root: bool,
}

impl UserDefinedMixin {
    pub fn new(
        args: FuncArgs,
        body: Vec<Token>,
        accepts_content_block: bool,
        declared_at_root: bool,
    ) -> Self {
        Self {
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
