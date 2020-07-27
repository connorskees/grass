#![allow(unused_imports, unused_variables, dead_code, unused_mut)]

use std::collections::BTreeMap;

use codemap::Spanned;

use crate::{
    args::CallArgs,
    atrule::Mixin,
    builtin::Builtin,
    common::{Identifier, QuoteKind},
    error::SassResult,
    parse::Parser,
    value::{SassFunction, SassMap, Value},
};

mod color;
mod list;
mod map;
mod math;
mod meta;
mod selector;
mod string;

#[derive(Debug, Default)]
pub(crate) struct Module {
    vars: BTreeMap<Identifier, Value>,
    mixins: BTreeMap<Identifier, Mixin>,
    functions: BTreeMap<Identifier, SassFunction>,
}

impl Module {
    pub fn get_var(&self, name: Spanned<Identifier>) -> SassResult<&Value> {
        match self.vars.get(&name.node) {
            Some(v) => Ok(v),
            None => Err(("Undefined variable.", name.span).into()),
        }
    }

    pub fn insert_builtin_var(&mut self, name: &'static str, value: Value) {
        self.vars.insert(name.into(), value);
    }

    pub fn get_fn(&self, name: Identifier) -> Option<SassFunction> {
        self.functions.get(&name).cloned()
    }

    pub fn insert_builtin(
        &mut self,
        name: &'static str,
        function: fn(CallArgs, &mut Parser<'_>) -> SassResult<Value>,
    ) {
        let ident = name.into();
        self.functions
            .insert(ident, SassFunction::Builtin(Builtin::new(function), ident));
    }

    pub fn functions(&self) -> SassMap {
        SassMap::new_with(
            self.functions
                .iter()
                .map(|(key, value)| {
                    (
                        Value::String(key.to_string(), QuoteKind::Quoted),
                        Value::FunctionRef(value.clone()),
                    )
                })
                .collect::<Vec<(Value, Value)>>(),
        )
    }

    pub fn variables(&self) -> SassMap {
        SassMap::new_with(
            self.vars
                .iter()
                .map(|(key, value)| {
                    (
                        Value::String(key.to_string(), QuoteKind::Quoted),
                        value.clone(),
                    )
                })
                .collect::<Vec<(Value, Value)>>(),
        )
    }
}

pub(crate) fn declare_module_color() -> Module {
    let mut module = Module::default();
    color::declare(&mut module);
    module
}

pub(crate) fn declare_module_list() -> Module {
    let mut module = Module::default();
    list::declare(&mut module);
    module
}

pub(crate) fn declare_module_map() -> Module {
    let mut module = Module::default();
    map::declare(&mut module);
    module
}

pub(crate) fn declare_module_math() -> Module {
    let mut module = Module::default();
    math::declare(&mut module);
    module
}

pub(crate) fn declare_module_meta() -> Module {
    let mut module = Module::default();
    meta::declare(&mut module);
    module
}

pub(crate) fn declare_module_selector() -> Module {
    let mut module = Module::default();
    selector::declare(&mut module);
    module
}

pub(crate) fn declare_module_string() -> Module {
    let mut module = Module::default();
    string::declare(&mut module);
    module
}
