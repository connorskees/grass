// A reference to the parser is only necessary for some functions
#![allow(unused_variables)]

use std::{
    collections::{BTreeSet, HashMap},
    sync::atomic::{AtomicUsize, Ordering},
};

use once_cell::sync::Lazy;

use crate::{ast::ArgumentResult, error::SassResult, evaluate::Visitor, value::Value};

pub mod color;
pub mod list;
pub mod map;
pub mod math;
pub mod meta;
pub mod selector;
pub mod string;

// todo: maybe Identifier instead of str?
pub(crate) type GlobalFunctionMap = HashMap<&'static str, Builtin>;

static FUNCTION_COUNT: AtomicUsize = AtomicUsize::new(0);

#[derive(Clone)]
pub(crate) struct Builtin(
    pub fn(ArgumentResult, &mut Visitor) -> SassResult<Value>,
    usize,
);

impl Builtin {
    pub fn new(body: fn(ArgumentResult, &mut Visitor) -> SassResult<Value>) -> Builtin {
        let count = FUNCTION_COUNT.fetch_add(1, Ordering::Relaxed);
        Self(body, count)
    }
}

impl PartialEq for Builtin {
    fn eq(&self, other: &Self) -> bool {
        self.1 == other.1
    }
}

impl Eq for Builtin {}

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

pub(crate) static DISALLOWED_PLAIN_CSS_FUNCTION_NAMES: Lazy<BTreeSet<&str>> = Lazy::new(|| {
    GLOBAL_FUNCTIONS
        .keys()
        .copied()
        .filter(|&name| {
            !matches!(
                name,
                "rgb"
                    | "rgba"
                    | "hsl"
                    | "hsla"
                    | "grayscale"
                    | "invert"
                    | "alpha"
                    | "opacity"
                    | "saturate"
            )
        })
        .collect()
});
