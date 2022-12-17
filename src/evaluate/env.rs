use codemap::{Span, Spanned};

use crate::{
    atrule::mixin::Mixin,
    builtin::modules::{Module, Modules},
    common::Identifier,
    error::SassResult,
    scope::{Scope, Scopes},
    selector::ExtensionStore,
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
    pub global_modules: Vec<Arc<Module>>,
    pub content: Option<Arc<CallableContentBlock>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            scopes: Scopes::new(),
            modules: Arc::new(RefCell::new(Modules::new())),
            global_modules: Vec::new(),
            content: None,
        }
    }

    pub fn new_closure(&self) -> Self {
        Self {
            scopes: self.scopes.new_closure(),
            modules: Arc::clone(&self.modules),
            global_modules: self.global_modules.iter().map(Arc::clone).collect(),
            content: self.content.as_ref().map(Arc::clone),
        }
    }

    pub fn insert_mixin(&mut self, name: Identifier, mixin: Mixin) {
        self.scopes.insert_mixin(name, mixin);
    }

    pub fn mixin_exists(&self, name: Identifier) -> bool {
        self.scopes.mixin_exists(name)
    }

    pub fn get_mixin(
        &self,
        name: Spanned<Identifier>,
        namespace: Option<Spanned<Identifier>>,
    ) -> SassResult<Mixin> {
        if let Some(namespace) = namespace {
            let modules = (*self.modules).borrow();
            let module = modules.get(namespace.node, namespace.span)?;
            return module.get_mixin(name);
        }

        self.scopes.get_mixin(name)
    }

    pub fn insert_fn(&mut self, func: SassFunction) {
        self.scopes.insert_fn(func);
    }

    pub fn fn_exists(&self, name: Identifier) -> bool {
        self.scopes.fn_exists(name)
    }

    pub fn get_fn(
        &self,
        name: Identifier,
        namespace: Option<Spanned<Identifier>>,
    ) -> SassResult<Option<SassFunction>> {
        if let Some(namespace) = namespace {
            let modules = (*self.modules).borrow();
            let module = modules.get(namespace.node, namespace.span)?;
            return Ok(module.get_fn(name));
        }

        Ok(self.scopes.get_fn(name))
    }

    pub fn var_exists(
        &self,
        name: Identifier,
        namespace: Option<Spanned<Identifier>>,
    ) -> SassResult<bool> {
        if let Some(namespace) = namespace {
            let modules = (*self.modules).borrow();
            let module = modules.get(namespace.node, namespace.span)?;
            return Ok(module.var_exists(name));
        }

        Ok(self.scopes.var_exists(name))
    }

    pub fn get_var(
        &self,
        name: Spanned<Identifier>,
        namespace: Option<Spanned<Identifier>>,
    ) -> SassResult<Value> {
        if let Some(namespace) = namespace {
            let modules = (*self.modules).borrow();
            let module = modules.get(namespace.node, namespace.span)?;
            return module.get_var(name);
        }

        self.scopes.get_var(name)
    }

    pub fn insert_var(
        &mut self,
        name: Spanned<Identifier>,
        namespace: Option<Spanned<Identifier>>,
        value: Value,
        is_global: bool,
        in_semi_global_scope: bool,
    ) -> SassResult<()> {
        if let Some(namespace) = namespace {
            let mut modules = (*self.modules).borrow_mut();
            let module = modules.get_mut(namespace.node, namespace.span)?;
            module.update_var(name, value)?;
            return Ok(());
        }

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

            self.scopes.insert_var(0, name.node, value);
            return Ok(());
        }

        let mut index = self
            .scopes
            .find_var(name.node)
            .unwrap_or(self.scopes.len() - 1);

        if !in_semi_global_scope && index == 0 {
            index = self.scopes.len() - 1;
        }

        self.scopes.insert_var(index, name.node, value);

        Ok(())
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

    pub fn add_module(
        &mut self,
        namespace: Option<Identifier>,
        module: Module,
        span: Span,
    ) -> SassResult<()> {
        match namespace {
            Some(namespace) => {
                (*self.modules)
                    .borrow_mut()
                    .insert(namespace, module, span)?;
            }
            None => {
                for name in self.scopes.global_scope().var_names() {
                    if module.var_exists(name) {
                        todo!(
                            "This module and the new module both define a variable named \"{name}\"."
                        );
                    }
                }

                self.global_modules.push(Arc::new(module));
            }
        }

        Ok(())
    }

    pub fn to_module(self, extension_store: ExtensionStore) -> Module {
        debug_assert!(self.at_root());

        Module::new_env(self, extension_store)
        // Module {
        //     scope: todo!(),
        //     upstream: todo!(),
        //     extension_store: todo!(),
        //     is_builtin: todo!(),
        // }
    }
}
