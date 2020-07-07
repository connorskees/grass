use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
    sync::atomic::{AtomicUsize, Ordering},
};

use once_cell::sync::Lazy;

use crate::{args::CallArgs, error::SassResult, parse::Parser, value::Value};

#[macro_use]
mod macros;

mod color;
mod list;
mod map;
mod math;
mod meta;
mod selector;
mod string;

pub(crate) type GlobalFunctionMap = HashMap<&'static str, Builtin>;

static FUNCTION_COUNT: AtomicUsize = AtomicUsize::new(0);

// TODO: impl Fn
#[derive(Clone)]
pub(crate) struct Builtin(
    pub fn(CallArgs, &mut Parser<'_>) -> SassResult<Value>,
    usize,
);

impl Hash for Builtin {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.1.hash(state)
    }
}

impl Builtin {
    pub fn new(body: fn(CallArgs, &mut Parser<'_>) -> SassResult<Value>) -> Builtin {
        let count = FUNCTION_COUNT.fetch_add(1, Ordering::Relaxed);
        Self(body, count)
    }
}

impl PartialEq for Builtin {
    fn eq(&self, other: &Self) -> bool {
        self.1 == other.1
    }
}

pub(crate) static GLOBAL_FUNCTIONS: Lazy<GlobalFunctionMap> = Lazy::new(|| {
    let mut m = HashMap::new();
    color::declare(&mut m);
    list::declare(&mut m);
    map::declare(&mut m);
    math::declare(&mut m);
    meta::declare(&mut m);
    selector::declare(&mut m);
    string::declare(&mut m);
    m
});
