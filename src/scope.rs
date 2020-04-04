use std::cell::RefCell;
use std::collections::HashMap;

use crate::atrule::{Function, Mixin};
use crate::error::SassResult;
use crate::value::Value;

thread_local!(pub(crate) static GLOBAL_SCOPE: RefCell<Scope> = RefCell::new(Scope::new()));

pub(crate) fn get_global_var(s: &str) -> SassResult<Value> {
    GLOBAL_SCOPE.with(|scope| match scope.borrow().vars().get(s) {
        Some(v) => Ok(v.clone()),
        None => Err("Undefined variable.".into()),
    })
}

pub(crate) fn global_var_exists(v: &str) -> bool {
    GLOBAL_SCOPE.with(|scope| scope.borrow().vars().contains_key(v))
}

pub(crate) fn insert_global_var(s: &str, v: Value) -> SassResult<Option<Value>> {
    GLOBAL_SCOPE.with(|scope| scope.borrow_mut().insert_var(s, v))
}

pub(crate) fn get_global_fn(s: &str) -> SassResult<Function> {
    GLOBAL_SCOPE.with(|scope| match scope.borrow().functions().get(s) {
        Some(v) => Ok(v.clone()),
        None => Err("Undefined function.".into()),
    })
}

pub(crate) fn global_fn_exists(v: &str) -> bool {
    GLOBAL_SCOPE.with(|scope| scope.borrow().functions().contains_key(v))
}

pub(crate) fn insert_global_fn(s: &str, v: Function) -> Option<Function> {
    GLOBAL_SCOPE.with(|scope| scope.borrow_mut().insert_fn(s, v))
}

pub(crate) fn get_global_mixin(s: &str) -> SassResult<Mixin> {
    GLOBAL_SCOPE.with(|scope| match scope.borrow().mixins().get(s) {
        Some(v) => Ok(v.clone()),
        None => Err("Undefined mixin.".into()),
    })
}

pub(crate) fn global_mixin_exists(v: &str) -> bool {
    GLOBAL_SCOPE.with(|scope| scope.borrow().mixins().contains_key(v))
}

pub(crate) fn insert_global_mixin(s: &str, v: Mixin) -> Option<Mixin> {
    GLOBAL_SCOPE.with(|scope| scope.borrow_mut().insert_mixin(s, v))
}

#[derive(Debug, Clone)]
pub(crate) struct Scope {
    vars: HashMap<String, Value>,
    mixins: HashMap<String, Mixin>,
    functions: HashMap<String, Function>,
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

    pub const fn vars(&self) -> &HashMap<String, Value> {
        &self.vars
    }

    pub const fn functions(&self) -> &HashMap<String, Function> {
        &self.functions
    }

    pub const fn mixins(&self) -> &HashMap<String, Mixin> {
        &self.mixins
    }

    pub fn get_var(&self, v: &str) -> SassResult<Value> {
        let name = &v.replace('_', "-");
        match self.vars.get(name) {
            Some(v) => Ok(v.clone()),
            None => get_global_var(name),
        }
    }

    pub fn insert_var(&mut self, s: &str, v: Value) -> SassResult<Option<Value>> {
        Ok(self.vars.insert(s.replace('_', "-"), v.eval()?))
    }

    pub fn var_exists(&self, v: &str) -> bool {
        let name = &v.replace('_', "-");
        self.vars.contains_key(name) || global_var_exists(name)
    }

    pub fn get_mixin(&self, v: &str) -> SassResult<Mixin> {
        let name = &v.replace('_', "-");
        match self.mixins.get(name) {
            Some(v) => Ok(v.clone()),
            None => get_global_mixin(name),
        }
    }

    pub fn insert_mixin(&mut self, s: &str, v: Mixin) -> Option<Mixin> {
        self.mixins.insert(s.replace('_', "-"), v)
    }

    pub fn mixin_exists(&self, v: &str) -> bool {
        let name = &v.replace('_', "-");
        self.mixins.contains_key(name) || global_mixin_exists(name)
    }

    pub fn get_fn(&self, v: &str) -> SassResult<Function> {
        let name = &v.replace('_', "-");
        match self.functions.get(name) {
            Some(v) => Ok(v.clone()),
            None => get_global_fn(name),
        }
    }

    pub fn insert_fn(&mut self, s: &str, v: Function) -> Option<Function> {
        self.functions.insert(s.replace('_', "-"), v)
    }

    pub fn fn_exists(&self, v: &str) -> bool {
        let name = &v.replace('_', "-");
        self.functions.contains_key(name) || global_fn_exists(name)
    }

    pub fn extend(&mut self, other: Scope) {
        self.vars.extend(other.vars);
        self.mixins.extend(other.mixins);
        self.functions.extend(other.functions);
    }
}
