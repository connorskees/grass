use std::collections::BTreeMap;

use codemap::{Span, Spanned};

use crate::{
    atrule::mixin::{BuiltinMixin, Mixin},
    builtin::Builtin,
    common::{Identifier, QuoteKind},
    error::SassResult,
    parse::{visitor::Visitor, ArgumentResult, Parser},
    scope::Scope,
    value::{SassFunction, SassMap, Value},
};

mod color;
mod list;
mod map;
mod math;
mod meta;
mod selector;
mod string;

#[derive(Debug, Default, Clone)]
pub(crate) struct Module {
    pub scope: Scope,

    /// A module can itself import other modules
    pub modules: Modules,

    /// Whether or not this module is builtin
    /// e.g. `"sass:math"`
    is_builtin: bool,
}

#[derive(Debug, Default, Clone)]
pub(crate) struct Modules(BTreeMap<Identifier, Module>);

#[derive(Debug, Default)]
pub(crate) struct ModuleConfig(BTreeMap<Identifier, Value>);

impl ModuleConfig {
    /// Removes and returns element with name
    pub fn get(&mut self, name: Identifier) -> Option<Value> {
        self.0.remove(&name)
    }

    /// If this structure is not empty at the end of
    /// an `@use`, we must throw an error
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn insert(&mut self, name: Spanned<Identifier>, value: Spanned<Value>) -> SassResult<()> {
        if self.0.insert(name.node, value.node).is_some() {
            Err((
                "The same variable may only be configured once.",
                name.span.merge(value.span),
            )
                .into())
        } else {
            Ok(())
        }
    }
}

impl Modules {
    pub fn insert(&mut self, name: Identifier, module: Module, span: Span) -> SassResult<()> {
        if self.0.contains_key(&name) {
            return Err((
                format!("There's already a module with namespace \"{}\".", name),
                span,
            )
                .into());
        }

        self.0.insert(name, module);

        Ok(())
    }

    pub fn get(&self, name: Identifier, span: Span) -> SassResult<&Module> {
        match self.0.get(&name) {
            Some(v) => Ok(v),
            None => Err((
                format!(
                    "There is no module with the namespace \"{}\".",
                    name.as_str()
                ),
                span,
            )
                .into()),
        }
    }

    pub fn get_mut(&mut self, name: Identifier, span: Span) -> SassResult<&mut Module> {
        match self.0.get_mut(&name) {
            Some(v) => Ok(v),
            None => Err((
                format!(
                    "There is no module with the namespace \"{}\".",
                    name.as_str()
                ),
                span,
            )
                .into()),
        }
    }

    pub fn merge(&mut self, other: Self) {
        self.0.extend(other.0);
    }
}

impl Module {
    pub fn new_builtin() -> Self {
        Module {
            scope: Scope::default(),
            modules: Modules::default(),
            is_builtin: true,
        }
    }

    pub fn get_var(&self, name: Spanned<Identifier>) -> SassResult<&Value> {
        if name.node.as_str().starts_with('-') {
            return Err((
                "Private members can't be accessed from outside their modules.",
                name.span,
            )
                .into());
        }

        match self.scope.vars.get(&name.node) {
            Some(v) => Ok(v),
            None => Err(("Undefined variable.", name.span).into()),
        }
    }

    pub fn update_var(&mut self, name: Spanned<Identifier>, value: Value) -> SassResult<()> {
        if self.is_builtin {
            return Err(("Cannot modify built-in variable.", name.span).into());
        }

        if name.node.as_str().starts_with('-') {
            return Err((
                "Private members can't be accessed from outside their modules.",
                name.span,
            )
                .into());
        }

        if self.scope.insert_var(name.node, value).is_some() {
            Ok(())
        } else {
            Err(("Undefined variable.", name.span).into())
        }
    }

    pub fn get_mixin(&self, name: Spanned<Identifier>) -> SassResult<Mixin> {
        if name.node.as_str().starts_with('-') {
            return Err((
                "Private members can't be accessed from outside their modules.",
                name.span,
            )
                .into());
        }

        match self.scope.mixins.get(&name.node) {
            Some(v) => Ok(v.clone()),
            None => Err(("Undefined mixin.", name.span).into()),
        }
    }

    pub fn insert_builtin_mixin(&mut self, name: &'static str, mixin: BuiltinMixin) {
        // self.scope.mixins.insert(name.into(), Mixin::Builtin(mixin));
        todo!()
    }

    pub fn insert_builtin_var(&mut self, name: &'static str, value: Value) {
        self.scope.vars.insert(name.into(), value);
    }

    pub fn get_fn(&self, name: Spanned<Identifier>) -> SassResult<Option<SassFunction>> {
        if name.node.as_str().starts_with('-') {
            return Err((
                "Private members can't be accessed from outside their modules.",
                name.span,
            )
                .into());
        }

        Ok(self.scope.functions.get(&name.node).cloned())
    }

    pub fn var_exists(&self, name: Identifier) -> bool {
        !name.as_str().starts_with('-') && self.scope.var_exists(name)
    }

    pub fn mixin_exists(&self, name: Identifier) -> bool {
        !name.as_str().starts_with('-') && self.scope.mixin_exists(name)
    }

    pub fn fn_exists(&self, name: Identifier) -> bool {
        !name.as_str().starts_with('-') && self.scope.fn_exists(name)
    }

    pub fn insert_builtin(
        &mut self,
        name: &'static str,
        function: fn(ArgumentResult, &mut Visitor) -> SassResult<Value>,
    ) {
        let ident = name.into();
        self.scope
            .functions
            .insert(ident, SassFunction::Builtin(Builtin::new(function), ident));
    }

    pub fn functions(&self) -> SassMap {
        SassMap::new_with(
            self.scope
                .functions
                .iter()
                .filter(|(key, _)| !key.as_str().starts_with('-'))
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
            self.scope
                .vars
                .iter()
                .filter(|(key, _)| !key.as_str().starts_with('-'))
                .map(|(key, value)| {
                    (
                        Value::String(key.to_string(), QuoteKind::Quoted),
                        value.clone(),
                    )
                })
                .collect::<Vec<(Value, Value)>>(),
        )
    }

    pub const fn new_from_scope(scope: Scope, modules: Modules, is_builtin: bool) -> Self {
        Module {
            scope,
            modules,
            is_builtin,
        }
    }
}

pub(crate) fn declare_module_color() -> Module {
    let mut module = Module::new_builtin();
    color::declare(&mut module);
    module
}

pub(crate) fn declare_module_list() -> Module {
    let mut module = Module::new_builtin();
    list::declare(&mut module);
    module
}

pub(crate) fn declare_module_map() -> Module {
    let mut module = Module::new_builtin();
    map::declare(&mut module);
    module
}

pub(crate) fn declare_module_math() -> Module {
    let mut module = Module::new_builtin();
    math::declare(&mut module);
    module
}

pub(crate) fn declare_module_meta() -> Module {
    let mut module = Module::new_builtin();
    meta::declare(&mut module);
    module
}

pub(crate) fn declare_module_selector() -> Module {
    let mut module = Module::new_builtin();
    selector::declare(&mut module);
    module
}

pub(crate) fn declare_module_string() -> Module {
    let mut module = Module::new_builtin();
    string::declare(&mut module);
    module
}
