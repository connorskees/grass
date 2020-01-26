use lazy_static::lazy_static;
use std::collections::BTreeMap;

use crate::common::Scope;
use crate::function::Function;
use crate::value::Value;

pub(crate) type Builtin = dyn Fn(&Scope) -> Value + Send + Sync;

lazy_static! {
    pub(crate) static ref GLOBAL_FUNCTIONS: BTreeMap<String, Function> = {
        let m = BTreeMap::new();
        m
    };
}
