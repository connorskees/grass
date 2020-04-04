use std::fmt;

use crate::args::CallArgs;
use crate::atrule::Function;
use crate::builtin::Builtin;
use crate::error::SassResult;
use crate::scope::Scope;
use crate::selector::Selector;
use crate::value::Value;

#[derive(Clone)]
pub(crate) enum SassFunction {
    Builtin(Builtin, String),
    UserDefined(Box<Function>, String),
}

impl SassFunction {
    pub fn name(&self) -> &str {
        match self {
            Self::Builtin(_, name) => name,
            Self::UserDefined(_, name) => name,
        }
    }

    pub fn call(self, args: CallArgs, scope: &Scope) -> SassResult<Value> {
        match self {
            Self::Builtin(f, ..) => f.0(args, scope),
            // todo: superselector
            Self::UserDefined(f, ..) => f.clone().args(args)?.call(&Selector::new(), f.body()),
        }
    }
}

impl fmt::Debug for SassFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
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
