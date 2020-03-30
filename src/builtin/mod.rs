use once_cell::sync::Lazy;
use std::collections::HashMap;

use crate::args::CallArgs;
use crate::error::SassResult;
use crate::scope::Scope;
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

pub(crate) static GLOBAL_FUNCTIONS: Lazy<HashMap<String, Builtin>> = Lazy::new(|| {
    let mut m = HashMap::new();
    color::register(&mut m);
    list::register(&mut m);
    map::register(&mut m);
    math::register(&mut m);
    meta::register(&mut m);
    string::register(&mut m);
    m
});
