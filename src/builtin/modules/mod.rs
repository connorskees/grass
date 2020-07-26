use std::collections::BTreeMap;

use crate::{
    atrule::Mixin,
    common::Identifier,
    value::{SassFunction, Value},
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
