//! SASS functions are those that are evaluated and return a value
//!
//! SASS functions can be either user-defined or builtin.
//!
//! User-defined functions are those that have been implemented in SASS
//! using the @function rule. See the documentation of `crate::atrule::Function`
//! for more information.
//!
//! Builtin functions are those that have been implemented in rust and are
//! in the global scope.

use std::fmt;

use crate::{
    args::CallArgs, atrule::Function, builtin::Builtin, common::Identifier, error::SassResult,
    parse::Parser, value::Value,
};

/// A SASS function
///
/// See toplevel documentation for more information
///
/// The function name is stored in addition to the body
/// for use in the builtin function `inspect()`
#[derive(Clone)]
pub(crate) enum SassFunction {
    Builtin(Builtin, Identifier),
    UserDefined(Box<Function>, Identifier),
}

impl SassFunction {
    /// Get the name of the function referenced
    ///
    /// Used mainly in debugging and `inspect()`
    pub fn name(&self) -> &Identifier {
        match self {
            Self::Builtin(_, name) | Self::UserDefined(_, name) => name,
        }
    }

    /// Whether the function is builtin or user-defined
    ///
    /// Used only in `std::fmt::Debug` for `SassFunction`
    fn kind(&self) -> &'static str {
        match &self {
            Self::Builtin(..) => "Builtin",
            Self::UserDefined(..) => "UserDefined",
        }
    }

    pub fn call(self, args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
        match self {
            Self::Builtin(f, ..) => f.0(args, parser),
            Self::UserDefined(f, ..) => parser.eval_function(*f, args),
        }
    }
}

impl fmt::Debug for SassFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SassFunction")
            .field("name", &self.name())
            .field("kind", &self.kind())
            .finish()
    }
}

impl PartialEq for SassFunction {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::UserDefined(f, ..) => match other {
                Self::UserDefined(f2, ..) => f == f2,
                Self::Builtin(..) => false,
            },
            Self::Builtin(f, ..) => match other {
                Self::UserDefined(..) => false,
                Self::Builtin(f2, ..) => f == f2,
            },
        }
    }
}

impl Eq for SassFunction {}
