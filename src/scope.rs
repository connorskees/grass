use std::{
    cell::{Ref, RefCell},
    collections::BTreeMap,
    sync::Arc,
};

// todo: move file to evaluate

use codemap::Spanned;

use crate::{
    atrule::mixin::Mixin,
    builtin::GLOBAL_FUNCTIONS,
    common::Identifier,
    error::SassResult,
    value::{SassFunction, Value},
};

/// A singular scope
///
/// Contains variables, functions, and mixins
#[derive(Debug, Default, Clone)]
pub(crate) struct Scope {
    pub vars: BTreeMap<Identifier, Value>,
    pub mixins: BTreeMap<Identifier, Mixin>,
    pub functions: BTreeMap<Identifier, SassFunction>,
}

impl Scope {
    // `BTreeMap::new` is not yet const
    #[allow(clippy::missing_const_for_fn)]
    #[must_use]
    pub fn new() -> Self {
        Self {
            vars: BTreeMap::new(),
            mixins: BTreeMap::new(),
            functions: BTreeMap::new(),
        }
    }

    pub fn var_names(&self) -> impl Iterator<Item = Identifier> + '_ {
        self.vars.keys().copied()
    }

    fn get_var(&self, name: Spanned<Identifier>) -> SassResult<&Value> {
        match self.vars.get(&name.node) {
            Some(v) => Ok(v),
            None => Err(("Undefined variable.", name.span).into()),
        }
    }

    pub fn get_var_no_err(&self, name: Identifier) -> Option<&Value> {
        self.vars.get(&name)
    }

    pub fn insert_var(&mut self, s: Identifier, v: Value) -> Option<Value> {
        self.vars.insert(s, v)
    }

    pub fn var_exists(&self, name: Identifier) -> bool {
        self.vars.contains_key(&name)
    }

    pub fn get_mixin(&self, name: Identifier) -> Option<Mixin> {
        self.mixins.get(&name).cloned()
    }

    pub fn insert_mixin<T: Into<Identifier>>(&mut self, s: T, v: Mixin) -> Option<Mixin> {
        self.mixins.insert(s.into(), v)
    }

    pub fn mixin_exists(&self, name: Identifier) -> bool {
        self.mixins.contains_key(&name)
    }

    pub fn get_fn(&self, name: Identifier) -> Option<SassFunction> {
        self.functions.get(&name).cloned()
    }

    pub fn insert_fn(&mut self, s: Identifier, v: SassFunction) -> Option<SassFunction> {
        self.functions.insert(s, v)
    }

    pub fn fn_exists(&self, name: Identifier) -> bool {
        if self.functions.is_empty() {
            return false;
        }
        self.functions.contains_key(&name)
    }
}

#[derive(Debug, Default, Clone)]
pub(crate) struct Scopes(Vec<Arc<RefCell<Scope>>>);

impl Scopes {
    pub fn new() -> Self {
        Self(vec![Arc::new(RefCell::new(Scope::new()))])
    }

    pub fn new_closure(&self) -> Self {
        Self(self.0.iter().map(Arc::clone).collect())
    }

    pub fn global_scope(&self) -> Ref<Scope> {
        (*self.0[0]).borrow()
    }

    pub fn global_scope_arc(&self) -> Arc<RefCell<Scope>> {
        Arc::clone(&self.0[0])
    }

    pub fn find_var(&self, name: Identifier) -> Option<usize> {
        for (idx, scope) in self.0.iter().enumerate().rev() {
            if (**scope).borrow().var_exists(name) {
                return Some(idx);
            }
        }

        None
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn enter_new_scope(&mut self) {
        self.0.push(Arc::new(RefCell::new(Scope::new())));
    }

    pub fn exit_scope(&mut self) {
        self.0.pop();
    }
}

/// Variables
impl Scopes {
    pub fn insert_var(&mut self, idx: usize, name: Identifier, v: Value) -> Option<Value> {
        self.0[idx].borrow_mut().insert_var(name, v)
    }

    /// Always insert this variable into the innermost scope
    ///
    /// Used, for example, for variables from `@each` and `@for`
    pub fn insert_var_last(&mut self, name: Identifier, v: Value) -> Option<Value> {
        self.0[self.0.len() - 1].borrow_mut().insert_var(name, v)
    }

    pub fn get_var(&self, name: Spanned<Identifier>) -> SassResult<Value> {
        for scope in self.0.iter().rev() {
            match (**scope).borrow().get_var_no_err(name.node) {
                Some(var) => return Ok(var.clone()),
                None => continue,
            }
        }

        Err(("Undefined variable.", name.span).into())
    }

    pub fn var_exists(&self, name: Identifier) -> bool {
        for scope in &self.0 {
            if (**scope).borrow().var_exists(name) {
                return true;
            }
        }

        false
    }
}

/// Mixins
impl Scopes {
    pub fn insert_mixin(&mut self, name: Identifier, mixin: Mixin) -> Option<Mixin> {
        self.0[self.0.len() - 1]
            .borrow_mut()
            .insert_mixin(name, mixin)
    }

    pub fn get_mixin(&self, name: Spanned<Identifier>) -> SassResult<Mixin> {
        for scope in self.0.iter().rev() {
            match (**scope).borrow().get_mixin(name.node) {
                Some(mixin) => return Ok(mixin),
                None => continue,
            }
        }

        Err(("Undefined mixin.", name.span).into())
    }

    pub fn mixin_exists(&self, name: Identifier) -> bool {
        for scope in &self.0 {
            if (**scope).borrow().mixin_exists(name) {
                return true;
            }
        }

        false
    }
}

/// Functions
impl Scopes {
    pub fn insert_fn(&mut self, func: SassFunction) {
        self.0[self.0.len() - 1]
            .borrow_mut()
            .insert_fn(func.name(), func);
    }

    pub fn get_fn(&self, name: Identifier) -> Option<SassFunction> {
        for scope in self.0.iter().rev() {
            let func = (**scope).borrow().get_fn(name);

            if func.is_some() {
                return func;
            }
        }

        None
    }

    pub fn fn_exists(&self, name: Identifier) -> bool {
        for scope in &self.0 {
            if (**scope).borrow().fn_exists(name) {
                return true;
            }
        }

        GLOBAL_FUNCTIONS.contains_key(name.as_str())
    }
}
