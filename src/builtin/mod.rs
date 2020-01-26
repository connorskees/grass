use lazy_static::lazy_static;
use std::collections::BTreeMap;

use crate::args::CallArgs;
use crate::common::Scope;
use crate::function::Function;
use crate::value::Value;

mod color;

pub(crate) type Builtin = Box<dyn Fn(&CallArgs) -> Value + Send + Sync>;

lazy_static! {
    pub(crate) static ref GLOBAL_FUNCTIONS: BTreeMap<String, Builtin> = {
        let mut m = BTreeMap::new();
        color::register(&mut m);
        m
    };
}

