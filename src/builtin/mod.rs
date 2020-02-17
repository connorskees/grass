use lazy_static::lazy_static;
use std::collections::BTreeMap;

use crate::args::CallArgs;
use crate::common::Scope;
use crate::error::SassResult;
use crate::value::Value;

#[macro_use]
mod macros;

mod color;
mod list;
mod map;
mod math;
mod meta;
mod selector;
mod string;

pub(crate) type Builtin = Box<dyn Fn(&mut CallArgs, &Scope) -> SassResult<Value> + Send + Sync>;

lazy_static! {
    pub(crate) static ref GLOBAL_FUNCTIONS: BTreeMap<String, Builtin> = {
        let mut m = BTreeMap::new();
        color::register(&mut m);
        list::register(&mut m);
        math::register(&mut m);
        meta::register(&mut m);
        string::register(&mut m);
        m
    };
}
