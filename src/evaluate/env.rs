use codemap::{Span, Spanned};

use crate::{
    ast::AstForwardRule,
    atrule::mixin::Mixin,
    builtin::modules::{ForwardedModule, Module, Modules},
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
    pub global_modules: Vec<Arc<RefCell<Module>>>,
    pub content: Option<Arc<CallableContentBlock>>,
    pub forwarded_modules: Arc<RefCell<Vec<Arc<RefCell<Module>>>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            scopes: Scopes::new(),
            modules: Arc::new(RefCell::new(Modules::new())),
            global_modules: Vec::new(),
            content: None,
            forwarded_modules: Arc::new(RefCell::new(Vec::new())),
        }
    }

    pub fn new_closure(&self) -> Self {
        Self {
            scopes: self.scopes.new_closure(),
            modules: Arc::clone(&self.modules),
            global_modules: self.global_modules.iter().map(Arc::clone).collect(),
            content: self.content.as_ref().map(Arc::clone),
            forwarded_modules: Arc::clone(&self.forwarded_modules),
        }
    }

    pub fn forward_module(
        &mut self,
        module: Arc<RefCell<Module>>,
        rule: AstForwardRule,
    ) -> SassResult<()> {
        let view = ForwardedModule::if_necessary(module, rule);
        (*self.forwarded_modules).borrow_mut().push(view);
        //     var forwardedModules = (_forwardedModules ??= {});

        // var view = ForwardedModuleView.ifNecessary(module, rule);
        // for (var other in forwardedModules.keys) {
        //   _assertNoConflicts(
        //       view.variables, other.variables, view, other, "variable");
        //   _assertNoConflicts(
        //       view.functions, other.functions, view, other, "function");
        //   _assertNoConflicts(view.mixins, other.mixins, view, other, "mixin");
        // }

        // // Add the original module to [_allModules] (rather than the
        // // [ForwardedModuleView]) so that we can de-duplicate upstream modules using
        // // `==`. This is safe because upstream modules are only used for collating
        // // CSS, not for the members they expose.
        // _allModules.add(module);
        // forwardedModules[view] = rule;
        // todo!()
        Ok(())
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
            return (*module).borrow().get_mixin(name);
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
            return Ok((*module).borrow().get_fn(name));
        }

        Ok(self
            .scopes
            .get_fn(name)
            .or_else(|| self.get_function_from_global_modules(name)))
    }

    pub fn var_exists(
        &self,
        name: Identifier,
        namespace: Option<Spanned<Identifier>>,
    ) -> SassResult<bool> {
        if let Some(namespace) = namespace {
            let modules = (*self.modules).borrow();
            let module = modules.get(namespace.node, namespace.span)?;
            return Ok((*module).borrow().var_exists(name));
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
            return (*module).borrow().get_var(name);
        }

        match self.scopes.get_var(name) {
            Ok(v) => Ok(v),
            Err(e) => {
                if let Some(v) = self.get_variable_from_global_modules(name.node) {
                    return Ok(v);
                }

                Err(e)
            }
        }
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
            (*module).borrow_mut().update_var(name, value)?;
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

    fn get_variable_from_global_modules(&self, name: Identifier) -> Option<Value> {
        for module in &self.global_modules {
            if (**module).borrow().var_exists(name) {
                return (**module).borrow().get_var_no_err(name);
            }
        }

        None
    }

    fn get_function_from_global_modules(&self, name: Identifier) -> Option<SassFunction> {
        for module in &self.global_modules {
            if (**module).borrow().fn_exists(name) {
                return (**module).borrow().get_fn(name);
            }
        }

        None
    }

    pub fn add_module(
        &mut self,
        namespace: Option<Identifier>,
        module: Arc<RefCell<Module>>,
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
                    if (*module).borrow().var_exists(name) {
                        todo!(
                            "This module and the new module both define a variable named \"{name}\"."
                        );
                    }
                }

                self.global_modules.push(module);
            }
        }

        Ok(())
    }

    pub fn to_module(self, extension_store: ExtensionStore) -> Arc<RefCell<Module>> {
        debug_assert!(self.at_root());

        Arc::new(RefCell::new(Module::new_env(self, extension_store)))
    }
}
