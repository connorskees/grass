use codemap::{Span, Spanned};

use crate::{
    ast::{AstForwardRule, Mixin},
    builtin::modules::{ForwardedModule, Module, Modules},
    common::Identifier,
    error::SassResult,
    selector::ExtensionStore,
    value::{SassFunction, Value},
};
use std::{cell::RefCell, collections::BTreeMap, sync::Arc};

use super::{scope::Scopes, visitor::CallableContentBlock};

#[derive(Debug, Clone)]
pub(crate) struct Environment {
    pub scopes: Scopes,
    pub modules: std::rc::Rc<RefCell<Modules>>,
    pub global_modules: Vec<std::rc::Rc<RefCell<Module>>>,
    pub content: Option<std::rc::Rc<CallableContentBlock>>,
    pub forwarded_modules: std::rc::Rc<RefCell<Vec<std::rc::Rc<RefCell<Module>>>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            scopes: Scopes::new(),
            modules: std::rc::Rc::new(RefCell::new(Modules::new())),
            global_modules: Vec::new(),
            content: None,
            forwarded_modules: std::rc::Rc::new(RefCell::new(Vec::new())),
        }
    }

    pub fn new_closure(&self) -> Self {
        Self {
            scopes: self.scopes.new_closure(),
            modules: std::rc::Rc::clone(&self.modules),
            global_modules: self.global_modules.iter().map(std::rc::Rc::clone).collect(),
            content: self.content.as_ref().map(std::rc::Rc::clone),
            forwarded_modules: std::rc::Rc::clone(&self.forwarded_modules),
        }
    }

    pub fn forward_module(&mut self, module: std::rc::Rc<RefCell<Module>>, rule: AstForwardRule) {
        let view = ForwardedModule::if_necessary(module, rule);
        (*self.forwarded_modules).borrow_mut().push(view);

        // todo: assertnoconflicts
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

        match self.scopes.get_mixin(name) {
            Ok(v) => Ok(v),
            Err(e) => {
                if let Some(v) = self.get_mixin_from_global_modules(name.node) {
                    return Ok(v);
                }

                Err(e)
            }
        }
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
        &mut self,
        name: Spanned<Identifier>,
        namespace: Option<Spanned<Identifier>>,
    ) -> SassResult<std::rc::Rc<Value>> {
        if let Some(namespace) = namespace {
            let modules = (*self.modules).borrow();
            let module = modules.get(namespace.node, namespace.span)?;
            return (*module).borrow().get_var(name);
        }

        match self.scopes.get_var(name) {
            Ok(v) => Ok(v),
            Err(e) => {
                if let Some(v) = self.get_variable_from_global_modules(name.node) {
                    Ok(v)
                } else {
                    Err(e)
                }
            }
        }
    }

    pub fn insert_var(
        &mut self,
        name: Spanned<Identifier>,
        namespace: Option<Spanned<Identifier>>,
        value: std::rc::Rc<Value>,
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

        self.scopes.last_variable_index = Some((name.node, index));

        self.scopes.insert_var(index, name.node, value);

        Ok(())
    }

    pub fn at_root(&self) -> bool {
        self.scopes.len() == 1
    }

    pub fn scopes_mut(&mut self) -> &mut Scopes {
        &mut self.scopes
    }

    pub fn global_vars(&self) -> std::rc::Rc<RefCell<BTreeMap<Identifier, std::rc::Rc<Value>>>> {
        self.scopes.global_variables()
    }

    pub fn global_mixins(&self) -> std::rc::Rc<RefCell<BTreeMap<Identifier, Mixin>>> {
        self.scopes.global_mixins()
    }

    pub fn global_functions(&self) -> std::rc::Rc<RefCell<BTreeMap<Identifier, SassFunction>>> {
        self.scopes.global_functions()
    }

    fn get_variable_from_global_modules(&self, name: Identifier) -> Option<std::rc::Rc<Value>> {
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

    fn get_mixin_from_global_modules(&self, name: Identifier) -> Option<Mixin> {
        for module in &self.global_modules {
            if (**module).borrow().mixin_exists(name) {
                return (**module).borrow().get_mixin_no_err(name);
            }
        }

        None
    }

    pub fn add_module(
        &mut self,
        namespace: Option<Identifier>,
        module: std::rc::Rc<RefCell<Module>>,
        span: Span,
    ) -> SassResult<()> {
        match namespace {
            Some(namespace) => {
                (*self.modules)
                    .borrow_mut()
                    .insert(namespace, module, span)?;
            }
            None => {
                for name in (*self.scopes.global_variables()).borrow().keys() {
                    if (*module).borrow().var_exists(*name) {
                        return Err((
                            format!("This module and the new module both define a variable named \"{name}\".", name = name)
                        , span).into());
                    }
                }

                self.global_modules.push(module);
            }
        }

        Ok(())
    }

    pub fn to_module(self, extension_store: ExtensionStore) -> std::rc::Rc<RefCell<Module>> {
        debug_assert!(self.at_root());

        std::rc::Rc::new(RefCell::new(Module::new_env(self, extension_store)))
    }
}
