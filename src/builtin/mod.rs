use lazy_static::lazy_static;
use std::collections::BTreeMap;

use crate::args::CallArgs;
use crate::value::Value;

macro_rules! arg {
    ($args:ident, $idx:literal, $name:literal) => {
        match $args.get(stringify!($idx)) {
            Some(v) => v,
            None => match $args.get($name) {
                Some(v) => v,
                None => panic!("missing variable"),
            },
        };
    };
    ($args:ident, $idx:literal, $name:literal, $default:literal) => {
        match $args.get(stringify!($idx)) {
            Some(v) => v,
            None => match $args.get($name) {
                Some(v) => v,
                None => $default,
            },
        };
    };
}


mod color;
mod list;
mod map;
mod math;
mod meta;
mod selector;
mod string;

pub(crate) type Builtin = Box<dyn Fn(&CallArgs) -> Option<Value> + Send + Sync>;

lazy_static! {
    pub(crate) static ref GLOBAL_FUNCTIONS: BTreeMap<String, Builtin> = {
        let mut m = BTreeMap::new();
        color::register(&mut m);
        meta::register(&mut m);
        m
    };
}
