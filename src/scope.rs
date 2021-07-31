use std::collections::BTreeMap;

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
#[derive(Debug, Default)]
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

    fn get_var(&self, name: Spanned<Identifier>) -> SassResult<&Value> {
        match self.vars.get(&name.node) {
            Some(v) => Ok(v),
            None => Err(("Undefined variable.", name.span).into()),
        }
    }

    fn get_var_no_err(&self, name: Identifier) -> Option<&Value> {
        self.vars.get(&name)
    }

    pub fn insert_var(&mut self, s: Identifier, v: Value) -> Option<Value> {
        self.vars.insert(s, v)
    }

    pub fn var_exists(&self, name: Identifier) -> bool {
        self.vars.contains_key(&name)
    }

    fn get_mixin(&self, name: Spanned<Identifier>) -> SassResult<Mixin> {
        match self.mixins.get(&name.node) {
            Some(v) => Ok(v.clone()),
            None => Err(("Undefined mixin.", name.span).into()),
        }
    }

    pub fn insert_mixin<T: Into<Identifier>>(&mut self, s: T, v: Mixin) -> Option<Mixin> {
        self.mixins.insert(s.into(), v)
    }

    pub fn mixin_exists(&self, name: Identifier) -> bool {
        self.mixins.contains_key(&name)
    }

    fn get_fn(&self, name: Identifier) -> Option<SassFunction> {
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

    fn merge(&mut self, other: Scope) {
        self.vars.extend(other.vars);
        self.mixins.extend(other.mixins);
        self.functions.extend(other.functions);
    }

    pub fn merge_module_scope(&mut self, other: Scope) {
        self.merge(other);
    }

    pub fn default_var_exists(&self, s: Identifier) -> bool {
        if let Some(default_var) = self.get_var_no_err(s) {
            !default_var.is_null()
        } else {
            false
        }
    }
}

#[derive(Debug, Default)]
pub(crate) struct Scopes(Vec<Scope>);

impl Scopes {
    pub const fn new() -> Self {
        Self(Vec::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn split_off(mut self, len: usize) -> (Scopes, Scopes) {
        let split = self.0.split_off(len);
        (self, Scopes(split))
    }

    pub fn enter_new_scope(&mut self) {
        self.0.push(Scope::new());
    }

    pub fn enter_scope(&mut self, scope: Scope) {
        self.0.push(scope);
    }

    pub fn exit_scope(&mut self) {
        self.0.pop();
    }

    pub fn merge(&mut self, mut other: Self) {
        self.0.append(&mut other.0);
    }
}

/// Variables
impl Scopes {
    pub fn insert_var(&mut self, s: Identifier, v: Value) -> Option<Value> {
        for scope in self.0.iter_mut().rev() {
            if scope.var_exists(s) {
                return scope.insert_var(s, v);
            }
        }
        if let Some(scope) = self.0.last_mut() {
            scope.insert_var(s, v)
        } else {
            let mut scope = Scope::new();
            scope.insert_var(s, v);
            self.0.push(scope);
            None
        }
    }

    /// Always insert this variable into the innermost scope
    ///
    /// Used, for example, for variables from `@each` and `@for`
    pub fn insert_var_last(&mut self, s: Identifier, v: Value) -> Option<Value> {
        if let Some(scope) = self.0.last_mut() {
            scope.insert_var(s, v)
        } else {
            let mut scope = Scope::new();
            scope.insert_var(s, v);
            self.0.push(scope);
            None
        }
    }

    pub fn default_var_exists(&self, name: Identifier) -> bool {
        for scope in self.0.iter().rev() {
            if scope.default_var_exists(name) {
                return true;
            }
        }

        false
    }

    pub fn get_var<'a>(
        &'a self,
        name: Spanned<Identifier>,
        global_scope: &'a Scope,
    ) -> SassResult<&Value> {
        for scope in self.0.iter().rev() {
            if scope.var_exists(name.node) {
                return scope.get_var(name);
            }
        }
        global_scope.get_var(name)
    }

    pub fn var_exists(&self, name: Identifier, global_scope: &Scope) -> bool {
        for scope in &self.0 {
            if scope.var_exists(name) {
                return true;
            }
        }
        global_scope.var_exists(name)
    }
}

/// Mixins
impl Scopes {
    pub fn insert_mixin(&mut self, s: Identifier, v: Mixin) -> Option<Mixin> {
        if let Some(scope) = self.0.last_mut() {
            scope.insert_mixin(s, v)
        } else {
            let mut scope = Scope::new();
            scope.insert_mixin(s, v);
            self.0.push(scope);
            None
        }
    }

    pub fn get_mixin<'a>(
        &'a self,
        name: Spanned<Identifier>,
        global_scope: &'a Scope,
    ) -> SassResult<Mixin> {
        for scope in self.0.iter().rev() {
            if scope.mixin_exists(name.node) {
                return scope.get_mixin(name);
            }
        }
        global_scope.get_mixin(name)
    }

    pub fn mixin_exists(&self, name: Identifier, global_scope: &Scope) -> bool {
        for scope in &self.0 {
            if scope.mixin_exists(name) {
                return true;
            }
        }
        global_scope.mixin_exists(name)
    }
}

/// Functions
impl Scopes {
    pub fn insert_fn(&mut self, s: Identifier, v: SassFunction) -> Option<SassFunction> {
        if let Some(scope) = self.0.last_mut() {
            scope.insert_fn(s, v)
        } else {
            let mut scope = Scope::new();
            scope.insert_fn(s, v);
            self.0.push(scope);
            None
        }
    }

    pub fn get_fn<'a>(&'a self, name: Identifier, global_scope: &'a Scope) -> Option<SassFunction> {
        for scope in self.0.iter().rev() {
            if scope.fn_exists(name) {
                return scope.get_fn(name);
            }
        }
        global_scope.get_fn(name)
    }

    pub fn fn_exists(&self, name: Identifier, global_scope: &Scope) -> bool {
        for scope in &self.0 {
            if scope.fn_exists(name) {
                return true;
            }
        }
        global_scope.fn_exists(name) || GLOBAL_FUNCTIONS.contains_key(name.as_str())
    }
}
