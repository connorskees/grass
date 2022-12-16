use codemap::Spanned;

use crate::{
    atrule::mixin::Mixin,
    builtin::modules::Modules,
    common::Identifier,
    error::SassResult,
    scope::{Scope, Scopes},
    value::{SassFunction, Value},
};
use std::{
    cell::{Ref, RefCell},
    sync::Arc,
};

use super::visitor::CallableContentBlock;

#[derive(Debug, Clone)]
pub(crate) struct Environment {
    pub scopes: Scopes,
    pub modules: Arc<RefCell<Modules>>,
    pub content: Option<Arc<CallableContentBlock>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            scopes: Scopes::new(),
            modules: Arc::new(RefCell::new(Modules::default())),
            content: None,
        }
    }

    pub fn new_closure(&self) -> Self {
        Self {
            scopes: self.scopes.new_closure(),
            modules: Arc::clone(&self.modules),
            content: self.content.as_ref().map(Arc::clone),
        }
    }

    pub fn insert_mixin(&mut self, name: Identifier, mixin: Mixin) {
        self.scopes.insert_mixin(name, mixin);
    }

    pub fn mixin_exists(&self, name: Identifier) -> bool {
        self.scopes.mixin_exists(name)
    }

    pub fn get_mixin(&self, name: Spanned<Identifier>) -> SassResult<Mixin> {
        self.scopes.get_mixin(name)
    }

    pub fn insert_fn(&mut self, func: SassFunction) {
        self.scopes.insert_fn(func);
    }

    pub fn fn_exists(&self, name: Identifier) -> bool {
        self.scopes.fn_exists(name)
    }

    pub fn get_fn(&self, name: Identifier) -> Option<SassFunction> {
        self.scopes.get_fn(name)
    }

    pub fn var_exists(&self, name: Identifier) -> bool {
        self.scopes.var_exists(name)
    }

    pub fn get_var(&self, name: Spanned<Identifier>) -> SassResult<Value> {
        self.scopes.get_var(name)
    }

    pub fn insert_var(
        &mut self,
        name: Identifier,
        value: Value,
        is_global: bool,
        in_semi_global_scope: bool,
    ) {
        if is_global || self.at_root() {
            //         // Don't set the index if there's already a variable with the given name,
            //   // since local accesses should still return the local variable.
            //   _variableIndices.putIfAbsent(name, () {
            //     _lastVariableName = name;
            //     _lastVariableIndex = 0;
            //     return 0;
            //   });

            //   // If this module doesn't already contain a variable named [name], try
            //   // setting it in a global module.
            //   if (!_variables.first.containsKey(name)) {
            //     var moduleWithName = _fromOneModule(name, "variable",
            //         (module) => module.variables.containsKey(name) ? module : null);
            //     if (moduleWithName != null) {
            //       moduleWithName.setVariable(name, value, nodeWithSpan);
            //       return;
            //     }
            //   }

            self.scopes.insert_var(0, name, value);
            return;
        }

        let mut index = self.scopes.find_var(name).unwrap_or(self.scopes.len() - 1);

        if !in_semi_global_scope && index == 0 {
            index = self.scopes.len() - 1;
        }

        self.scopes.insert_var(index, name, value);
    }

    pub fn at_root(&self) -> bool {
        self.scopes.len() == 1
    }

    pub fn scopes_mut(&mut self) -> &mut Scopes {
        &mut self.scopes
    }

    pub fn global_scope(&self) -> Ref<Scope> {
        self.scopes.global_scope()
    }
}
