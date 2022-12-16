use std::fmt;

use crate::{
    ast::ArgumentResult,
    error::SassResult,
    evaluate::{Environment, Visitor},
    parse::Stmt,
};

pub(crate) type BuiltinMixin = fn(ArgumentResult, &mut Visitor) -> SassResult<Vec<Stmt>>;

pub(crate) use crate::ast::AstMixin as UserDefinedMixin;

#[derive(Clone)]
pub(crate) enum Mixin {
    // todo: env is superfluous?
    UserDefined(UserDefinedMixin, Environment),
    Builtin(BuiltinMixin),
}

impl fmt::Debug for Mixin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UserDefined(u, ..) => f
                .debug_struct("AstMixin")
                .field("name", &u.name)
                .field("args", &u.args)
                .field("body", &u.body)
                .field("has_content", &u.has_content)
                .finish(),
            Self::Builtin(..) => f.debug_struct("BuiltinMixin").finish(),
        }
    }
}

// impl Mixin {
//     pub fn new_user_defined(
//         args: FuncArgs,
//         body: Vec<Token>,
//         accepts_content_block: bool,
//         declared_at_root: bool,
//     ) -> Self {
//         Mixin::UserDefined(UserDefinedMixin::new(
//             args,
//             body,
//             accepts_content_block,
//             declared_at_root,
//         ))
//     }
// }

// #[derive(Debug, Clone)]
// pub(crate) struct UserDefinedMixin {
//     pub args: FuncArgs,
//     pub body: Vec<Token>,
//     pub accepts_content_block: bool,
//     pub declared_at_root: bool,
// }

// impl UserDefinedMixin {
//     pub fn new(
//         args: FuncArgs,
//         body: Vec<Token>,
//         accepts_content_block: bool,
//         declared_at_root: bool,
//     ) -> Self {
//         Self {
//             args,
//             body,
//             accepts_content_block,
//             declared_at_root,
//         }
//     }
// }

// #[derive(Debug, Clone)]
// pub(crate) struct Content {
//     /// The literal block, serialized as a list of tokens
//     pub content: Option<Vec<Token>>,

//     /// Optional args, e.g. `@content(a, b, c);`
//     pub content_args: Option<ArgumentInvocation>,

//     /// The number of scopes at the use of `@include`
//     ///
//     /// This is used to "reset" back to the state of the `@include`
//     /// without actually cloning the scope or putting it in an `Rc`
//     pub scope_len: usize,

//     /// Whether or not the mixin this `@content` block is inside of was
//     /// declared in the global scope
//     pub declared_at_root: bool,
// }
