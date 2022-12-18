use std::{cell::RefCell, collections::BTreeMap, sync::Arc};

use codemap::{Span, Spanned};

use crate::{
    ast::ArgumentResult,
    atrule::mixin::{BuiltinMixin, Mixin},
    builtin::Builtin,
    common::Identifier,
    error::SassResult,
    evaluate::{Environment, Visitor},
    scope::Scope,
    selector::ExtensionStore,
    value::{SassFunction, SassMap, Value},
};

mod color;
mod list;
mod map;
mod math;
mod meta;
mod selector;
mod string;

#[derive(Debug, Clone)]
pub(crate) enum Module {
    Environment {
        scope: PublicMemberFilter,
        upstream: Vec<Module>,
        extension_store: ExtensionStore,
        env: Environment,
    },
    Builtin {
        scope: PublicMemberFilter,
    },
}
// /// Whether or not this module is builtin
// /// e.g. `"sass:math"`
// is_builtin: bool,

#[derive(Debug, Clone)]
pub(crate) struct Modules(BTreeMap<Identifier, Module>);

// #[derive(Debug, Default)]
// pub(crate) struct ModuleConfig(BTreeMap<Identifier, Value>);

// impl ModuleConfig {
//     /// Removes and returns element with name
//     pub fn get(&mut self, name: Identifier) -> Option<Value> {
//         self.0.remove(&name)
//     }

//     /// If this structure is not empty at the end of
//     /// an `@use`, we must throw an error
//     pub fn is_empty(&self) -> bool {
//         self.0.is_empty()
//     }

//     pub fn insert(&mut self, name: Spanned<Identifier>, value: Spanned<Value>) -> SassResult<()> {
//         if self.0.insert(name.node, value.node).is_some() {
//             Err((
//                 "The same variable may only be configured once.",
//                 name.span.merge(value.span),
//             )
//                 .into())
//         } else {
//             Ok(())
//         }
//     }
// }

impl Modules {
    pub fn new() -> Self {
        Self(BTreeMap::new())
    }

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

#[derive(Debug, Clone)]
pub(crate) struct PublicMemberFilter(Arc<RefCell<Scope>>);

impl PublicMemberFilter {
    pub fn new(scope: Arc<RefCell<Scope>>) -> Self {
        Self(scope)
    }

    pub fn var_exists(&self, name: Identifier) -> bool {
        if name.as_str().starts_with('-') {
            return false;
        }

        (*self.0).borrow().var_exists(name)
    }

    pub fn fn_exists(&self, name: Identifier) -> bool {
        if name.as_str().starts_with('-') {
            return false;
        }

        (*self.0).borrow().fn_exists(name)
    }

    pub fn get_mixin(&self, name: Identifier) -> Option<Mixin> {
        if name.as_str().starts_with('-') {
            return None;
        }

        (*self.0).borrow().get_mixin(name)
    }

    pub fn get_var(&self, name: Identifier) -> Option<Value> {
        if name.as_str().starts_with('-') {
            return None;
        }

        (*self.0).borrow().get_var_no_err(name).cloned()
    }

    pub fn get_fn(&self, name: Identifier) -> Option<SassFunction> {
        if name.as_str().starts_with('-') {
            return None;
        }

        (*self.0).borrow().get_fn(name)
    }
}

impl Module {
    pub fn new_env(env: Environment, extension_store: ExtensionStore) -> Self {
        Module::Environment {
            scope: PublicMemberFilter::new(env.scopes.global_scope_arc()),
            upstream: Vec::new(),
            extension_store,
            env,
        }
    }

    pub fn new_builtin() -> Self {
        Module::Builtin {
            scope: PublicMemberFilter::new(Arc::new(RefCell::new(Scope::new()))),
        }
    }

    pub fn get_var(&self, name: Spanned<Identifier>) -> SassResult<Value> {
        let scope = match self {
            Self::Builtin { scope } | Self::Environment { scope, .. } => scope,
        };

        match scope.get_var(name.node) {
            Some(v) => Ok(v.clone()),
            None => Err(("Undefined variable.", name.span).into()),
        }
    }

    pub fn get_var_no_err(&self, name: Identifier) -> Option<Value> {
        let scope = match self {
            Self::Builtin { scope } | Self::Environment { scope, .. } => scope,
        };

        scope.get_var(name)
    }

    pub fn update_var(&mut self, name: Spanned<Identifier>, value: Value) -> SassResult<()> {
        let scope = match self {
            Self::Builtin { .. } => {
                return Err(("Cannot modify built-in variable.", name.span).into())
            }
            Self::Environment { scope, .. } => scope,
        };
        // if self.is_builtin {
        //     return Err(("Cannot modify built-in variable.", name.span).into());
        // }

        if (*scope.0)
            .borrow_mut()
            .insert_var(name.node, value)
            .is_none()
        {
            return Err(("Undefined variable.", name.span).into());
        }

        // if self.scope.insert_var(name.node, value).is_some() {
        Ok(())
        // } else {
        //
        // }
        // todo!()
    }

    pub fn get_mixin(&self, name: Spanned<Identifier>) -> SassResult<Mixin> {
        // match self.scope.mixins.get(&name.node) {
        //     Some(v) => Ok(v.clone()),
        //     None => Err(("Undefined mixin.", name.span).into()),
        // }
        // todo!()
        let scope = match self {
            Self::Builtin { scope } | Self::Environment { scope, .. } => scope,
        };

        match scope.get_mixin(name.node) {
            Some(v) => Ok(v.clone()),
            None => Err(("Undefined mixin.", name.span).into()),
        }
    }

    pub fn insert_builtin_mixin(&mut self, name: &'static str, mixin: BuiltinMixin) {
        let scope = match self {
            Self::Builtin { scope } => scope,
            _ => unreachable!(),
        };

        (*scope.0)
            .borrow_mut()
            .insert_mixin(name, Mixin::Builtin(mixin));

        // self.scope.mixins.insert(name.into(), Mixin::Builtin(mixin));
        // todo!()
    }

    pub fn insert_builtin_var(&mut self, name: &'static str, value: Value) {
        let ident = name.into();

        let scope = match self {
            Self::Builtin { scope } => scope,
            _ => unreachable!(),
        };

        (*scope.0).borrow_mut().insert_var(ident, value);

        // self.scope.vars.insert(name.into(), value);
        // todo!()
    }

    pub fn get_fn(&self, name: Identifier) -> Option<SassFunction> {
        let scope = match self {
            Self::Builtin { scope } | Self::Environment { scope, .. } => scope,
        };

        scope.get_fn(name)
    }

    pub fn var_exists(&self, name: Identifier) -> bool {
        let scope = match self {
            Self::Builtin { scope } | Self::Environment { scope, .. } => scope,
        };

        scope.var_exists(name)
    }

    pub fn mixin_exists(&self, name: Identifier) -> bool {
        // !name.as_str().starts_with('-') && self.scope.mixin_exists(name)
        todo!()
    }

    pub fn fn_exists(&self, name: Identifier) -> bool {
        // !name.as_str().starts_with('-') && self.scope.fn_exists(name)
        let scope = match self {
            Self::Builtin { scope } | Self::Environment { scope, .. } => scope,
        };

        scope.fn_exists(name)
    }

    pub fn insert_builtin(
        &mut self,
        name: &'static str,
        function: fn(ArgumentResult, &mut Visitor) -> SassResult<Value>,
    ) {
        let ident = name.into();

        let scope = match self {
            Self::Builtin { scope } => scope,
            _ => unreachable!(),
        };

        (*scope.0)
            .borrow_mut()
            .insert_fn(ident, SassFunction::Builtin(Builtin::new(function), ident));
    }

    pub fn functions(&self) -> SassMap {
        // SassMap::new_with(
        //     self.scope
        //         .functions
        //         .iter()
        //         .filter(|(key, _)| !key.as_str().starts_with('-'))
        //         .map(|(key, value)| {
        //             (
        //                 Value::String(key.to_string(), QuoteKind::Quoted),
        //                 Value::FunctionRef(value.clone()),
        //             )
        //         })
        //         .collect::<Vec<(Value, Value)>>(),
        // )
        todo!()
    }

    pub fn variables(&self) -> SassMap {
        // SassMap::new_with(
        //     self.scope
        //         .vars
        //         .iter()
        //         .filter(|(key, _)| !key.as_str().starts_with('-'))
        //         .map(|(key, value)| {
        //             (
        //                 Value::String(key.to_string(), QuoteKind::Quoted),
        //                 value.clone(),
        //             )
        //         })
        //         .collect::<Vec<(Value, Value)>>(),
        // )
        todo!()
    }

    pub const fn new_from_scope(scope: Scope, modules: Modules, is_builtin: bool) -> Self {
        todo!()
        // Module {
        //     scope,
        //     modules,
        //     is_builtin,
        // }
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
