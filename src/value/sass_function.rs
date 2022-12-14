//! Sass functions are those that are evaluated and return a value
//!
//! Sass functions can be either user-defined or builtin.
//!
//! User-defined functions are those that have been implemented in Sass
//! using the @function rule. See the documentation of `crate::atrule::Function`
//! for more information.
//!
//! Builtin functions are those that have been implemented in rust and are
//! in the global scope.

use std::fmt;

// use codemap::Spanned;

use crate::{
    builtin::Builtin,
    common::Identifier,
    // error::SassResult,
    parse::AstFunctionDecl,
    // value::Value,
};

/// A Sass function
///
/// See toplevel documentation for more information
///
/// The function name is stored in addition to the body
/// for use in the builtin function `inspect()`
#[derive(Clone, Eq, PartialEq)]
pub(crate) enum SassFunction {
    // todo: Cow<'static>?
    Builtin(Builtin, Identifier),
    UserDefined(UserDefinedFunction),
    Plain { name: Identifier },
}

#[derive(Debug, Clone)]
pub(crate) struct UserDefinedFunction {
    pub function: Box<AstFunctionDecl>,
    pub name: Identifier,
    // pub env: Environment,
    pub scope_idx: usize,
}

impl PartialEq for UserDefinedFunction {
    fn eq(&self, other: &Self) -> bool {
        self.function == other.function && self.name == other.name
    }
}

impl Eq for UserDefinedFunction {}

impl SassFunction {
    /// Get the name of the function referenced
    ///
    /// Used mainly in debugging and `inspect()`
    pub fn name(&self) -> &Identifier {
        match self {
            Self::Builtin(_, name)
            | Self::UserDefined(UserDefinedFunction { name, .. })
            | Self::Plain { name } => name,
        }
    }

    /// Whether the function is builtin or user-defined
    ///
    /// Used only in `std::fmt::Debug` for `SassFunction`
    fn kind(&self) -> &'static str {
        match &self {
            Self::Plain { .. } => "Plain",
            Self::Builtin(..) => "Builtin",
            Self::UserDefined { .. } => "UserDefined",
        }
    }

    // pub fn call(
    //     self,
    //     args: CallArgs,
    //     module: Option<Spanned<Identifier>>,
    //     parser: &mut Visitor,
    // ) -> SassResult<Value> {
    //     match self {
    //         Self::Builtin(f, ..) => todo!(), //f.0(args, parser),
    //         Self::UserDefined { function, .. } => todo!(),
    //         // parser.eval_function(*function, args, module),
    //         Self::Plain { .. } => todo!(),
    //     }
    // }
}

impl fmt::Debug for SassFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SassFunction")
            .field("name", &self.name())
            .field("kind", &self.kind())
            .finish()
    }
}
