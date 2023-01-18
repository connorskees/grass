use codemap::{Span, Spanned};

use crate::{
    ast::{AstForwardRule, Configuration, Mixin},
    builtin::modules::{ForwardedModule, Module, ModuleScope, Modules},
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

    pub fn for_import(&self) -> Self {
        Self {
            scopes: self.scopes.new_closure(),
            modules: Arc::new(RefCell::new(Modules::new())),
            global_modules: Vec::new(),
            content: self.content.as_ref().map(Arc::clone),
            forwarded_modules: Arc::clone(&self.forwarded_modules),
        }
    }

    pub fn to_dummy_module(&self, span: Span) -> Module {
        Module::Environment {
            scope: ModuleScope::new(),
            upstream: Vec::new(),
            extension_store: ExtensionStore::new(span),
            env: self.clone(),
        }
    }

    pub fn import_forwards(&mut self, _env: Module) {
        //      if (module is _EnvironmentModule) {
        //   var forwarded = module._environment._forwardedModules;
        //   if (forwarded == null) return;

        //   // Omit modules from [forwarded] that are already globally available and
        //   // forwarded in this module.
        //   var forwardedModules = _forwardedModules;
        //   if (forwardedModules != null) {
        //     forwarded = {
        //       for (var entry in forwarded.entries)
        //         if (!forwardedModules.containsKey(entry.key) ||
        //             !_globalModules.containsKey(entry.key))
        //           entry.key: entry.value,
        //     };
        //   } else {
        //     forwardedModules = _forwardedModules ??= {};
        //   }

        //   var forwardedVariableNames =
        //       forwarded.keys.expand((module) => module.variables.keys).toSet();
        //   var forwardedFunctionNames =
        //       forwarded.keys.expand((module) => module.functions.keys).toSet();
        //   var forwardedMixinNames =
        //       forwarded.keys.expand((module) => module.mixins.keys).toSet();

        //   if (atRoot) {
        //     // Hide members from modules that have already been imported or
        //     // forwarded that would otherwise conflict with the @imported members.
        //     for (var entry in _importedModules.entries.toList()) {
        //       var module = entry.key;
        //       var shadowed = ShadowedModuleView.ifNecessary(module,
        //           variables: forwardedVariableNames,
        //           mixins: forwardedMixinNames,
        //           functions: forwardedFunctionNames);
        //       if (shadowed != null) {
        //         _importedModules.remove(module);
        //         if (!shadowed.isEmpty) _importedModules[shadowed] = entry.value;
        //       }
        //     }

        //     for (var entry in forwardedModules.entries.toList()) {
        //       var module = entry.key;
        //       var shadowed = ShadowedModuleView.ifNecessary(module,
        //           variables: forwardedVariableNames,
        //           mixins: forwardedMixinNames,
        //           functions: forwardedFunctionNames);
        //       if (shadowed != null) {
        //         forwardedModules.remove(module);
        //         if (!shadowed.isEmpty) forwardedModules[shadowed] = entry.value;
        //       }
        //     }

        //     _importedModules.addAll(forwarded);
        //     forwardedModules.addAll(forwarded);
        //   } else {
        //     (_nestedForwardedModules ??=
        //             List.generate(_variables.length - 1, (_) => []))
        //         .last
        //         .addAll(forwarded.keys);
        //   }

        //   // Remove existing member definitions that are now shadowed by the
        //   // forwarded modules.
        //   for (var variable in forwardedVariableNames) {
        //     _variableIndices.remove(variable);
        //     _variables.last.remove(variable);
        //     _variableNodes.last.remove(variable);
        //   }
        //   for (var function in forwardedFunctionNames) {
        //     _functionIndices.remove(function);
        //     _functions.last.remove(function);
        //   }
        //   for (var mixin in forwardedMixinNames) {
        //     _mixinIndices.remove(mixin);
        //     _mixins.last.remove(mixin);
        //   }
        // }
        // todo!()
    }

    pub fn to_implicit_configuration(&self) -> Configuration {
        //     var configuration = <String, ConfiguredValue>{};
        // for (var i = 0; i < _variables.length; i++) {
        //   var values = _variables[i];
        //   var nodes = _variableNodes[i];
        //   for (var entry in values.entries) {
        //     // Implicit configurations are never invalid, making [configurationSpan]
        //     // unnecessary, so we pass null here to avoid having to compute it.
        //     configuration[entry.key] =
        //         ConfiguredValue.implicit(entry.value, nodes[entry.key]!);
        //   }
        // }
        // return Configuration.implicit(configuration);
        todo!()
    }

    pub fn forward_module(&mut self, module: Arc<RefCell<Module>>, rule: AstForwardRule) {
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

    pub fn global_vars(&self) -> Arc<RefCell<BTreeMap<Identifier, Value>>> {
        self.scopes.global_variables()
    }

    pub fn global_mixins(&self) -> Arc<RefCell<BTreeMap<Identifier, Mixin>>> {
        self.scopes.global_mixins()
    }

    pub fn global_functions(&self) -> Arc<RefCell<BTreeMap<Identifier, SassFunction>>> {
        self.scopes.global_functions()
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
                for name in (*self.scopes.global_variables()).borrow().keys() {
                    if (*module).borrow().var_exists(*name) {
                        return Err((
                            format!("This module and the new module both define a variable named \"${name}\".", name = name)
                        , span).into());
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
