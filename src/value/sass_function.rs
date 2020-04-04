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

use crate::args::CallArgs;
use crate::atrule::Function;
use crate::builtin::Builtin;
use crate::error::SassResult;
use crate::scope::Scope;
use crate::selector::Selector;
use crate::value::Value;

/// A SASS function
/// See toplevel documentation for more information
/// 
/// The function name is stored in addition to the body
/// for use in the builtin function `inspect()`
#[derive(Clone)]
pub(crate) enum SassFunction {
    Builtin(Builtin, String),
    UserDefined(Box<Function>, String),
}

impl SassFunction {
    /// Get the name of the function referenced
    /// 
    /// Used mainly in debugging and `inspect()`
    pub fn name(&self) -> &str {
        match self {
            Self::Builtin(_, name) => name,
            Self::UserDefined(_, name) => name,
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

    pub fn call(
        self,
        args: CallArgs,
        scope: &Scope,
        super_selector: &Selector,
    ) -> SassResult<Value> {
        match self {
            Self::Builtin(f, ..) => f.0(args, scope, super_selector),
            // todo: superselector
            Self::UserDefined(f, ..) => f
                .clone()
                .args(args, scope, super_selector)?
                .call(&Selector::new(), f.body()),
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
