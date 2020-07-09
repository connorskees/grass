use std::collections::HashMap;

use codemap::Spanned;

use crate::{
    atrule::{Function, Mixin},
    builtin::GLOBAL_FUNCTIONS,
    common::Identifier,
    error::SassResult,
    value::Value,
};

#[derive(Debug, Default)]
pub(crate) struct Scope {
    vars: HashMap<Identifier, Spanned<Value>>,
    mixins: HashMap<Identifier, Mixin>,
    functions: HashMap<Identifier, Function>,
}

impl Scope {
    #[must_use]
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            mixins: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    fn get_var(&self, name: Spanned<&Identifier>) -> SassResult<&Value> {
        match self.vars.get(name.node) {
            Some(v) => Ok(&v.node),
            None => Err(("Undefined variable.", name.span).into()),
        }
    }

    pub fn insert_var(&mut self, s: Identifier, v: Spanned<Value>) -> Option<Spanned<Value>> {
        self.vars.insert(s, v)
    }

    pub fn var_exists(&self, name: &Identifier) -> bool {
        self.vars.contains_key(name)
    }

    fn get_mixin(&self, name: Spanned<&Identifier>) -> SassResult<Mixin> {
        match self.mixins.get(name.node) {
            Some(v) => Ok(v.clone()),
            None => Err(("Undefined mixin.", name.span).into()),
        }
    }

    pub fn insert_mixin<T: Into<Identifier>>(&mut self, s: T, v: Mixin) -> Option<Mixin> {
        self.mixins.insert(s.into(), v)
    }

    fn mixin_exists(&self, name: &Identifier) -> bool {
        self.mixins.contains_key(name)
    }

    fn get_fn(&self, name: Spanned<&Identifier>) -> SassResult<Function> {
        match self.functions.get(name.node) {
            Some(v) => Ok(v.clone()),
            None => Err(("Undefined function.", name.span).into()),
        }
    }

    pub fn insert_fn<T: Into<Identifier>>(&mut self, s: T, v: Function) -> Option<Function> {
        self.functions.insert(s.into(), v)
    }

    fn fn_exists(&self, name: &Identifier) -> bool {
        self.functions.contains_key(name)
    }

    fn merge(&mut self, other: Scope) {
        self.vars.extend(other.vars);
        self.mixins.extend(other.mixins);
        self.functions.extend(other.functions);
    }
}

#[derive(Debug, Default)]
pub(crate) struct Scopes(Vec<Scope>);

impl Scopes {
    pub const fn new() -> Self {
        Self(Vec::new())
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

    pub fn merge(&mut self, other: Scope) {
        if let Some(scope) = self.0.last_mut() {
            scope.merge(other)
        } else {
            panic!()
        }
    }
}

/// Variables
impl Scopes {
    pub fn insert_var(&mut self, s: Identifier, v: Spanned<Value>) -> Option<Spanned<Value>> {
        for scope in self.0.iter_mut().rev() {
            if scope.var_exists(&s) {
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

    pub fn insert_default_var(
        &mut self,
        s: Identifier,
        v: Spanned<Value>,
    ) -> Option<Spanned<Value>> {
        if let Some(scope) = self.0.last_mut() {
            if scope.var_exists(&s) {
                None
            } else {
                scope.insert_var(s, v)
            }
        } else {
            panic!()
        }
    }

    pub fn get_var<'a>(
        &'a self,
        name: Spanned<&Identifier>,
        global_scope: &'a Scope,
    ) -> SassResult<&Value> {
        for scope in self.0.iter().rev() {
            if scope.var_exists(&name.node) {
                return scope.get_var(name);
            }
        }
        global_scope.get_var(name)
    }

    pub fn var_exists(&self, name: &Identifier, global_scope: &Scope) -> bool {
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
        name: Spanned<&Identifier>,
        global_scope: &'a Scope,
    ) -> SassResult<Mixin> {
        for scope in self.0.iter().rev() {
            if scope.mixin_exists(&name.node) {
                return scope.get_mixin(name);
            }
        }
        global_scope.get_mixin(name)
    }

    pub fn mixin_exists(&self, name: &Identifier, global_scope: &Scope) -> bool {
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
    pub fn insert_fn(&mut self, s: Identifier, v: Function) -> Option<Function> {
        if let Some(scope) = self.0.last_mut() {
            scope.insert_fn(s, v)
        } else {
            let mut scope = Scope::new();
            scope.insert_fn(s, v);
            self.0.push(scope);
            None
        }
    }

    pub fn get_fn<'a>(
        &'a self,
        name: Spanned<&Identifier>,
        global_scope: &'a Scope,
    ) -> SassResult<Function> {
        for scope in self.0.iter().rev() {
            if scope.fn_exists(&name.node) {
                return scope.get_fn(name);
            }
        }
        global_scope.get_fn(name)
    }

    pub fn fn_exists(&self, name: &Identifier, global_scope: &Scope) -> bool {
        for scope in &self.0 {
            if scope.fn_exists(name) {
                return true;
            }
        }
        global_scope.fn_exists(name) || GLOBAL_FUNCTIONS.contains_key(name.as_str())
    }
}
