use std::cell::RefCell;
use std::collections::HashMap;

use crate::error::SassResult;
use crate::function::Function;
use crate::mixin::Mixin;
use crate::value::Value;

thread_local!(pub(crate) static GLOBAL_SCOPE: RefCell<Scope> = RefCell::new(Scope::new()));

pub(crate) fn get_global_var(s: &str) -> SassResult<Value> {
    GLOBAL_SCOPE.with(|scope| match scope.borrow().vars().get(s) {
        Some(v) => Ok(v.clone()),
        None => Err("Undefined variable.".into()),
    })
}

pub(crate) fn insert_global_var(s: &str, v: Value) -> SassResult<Option<Value>> {
    GLOBAL_SCOPE.with(|scope| scope.borrow_mut().insert_var(s, v))
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
        self.vars.contains_key(&v.replace('_', "-"))
    }

    pub fn get_mixin(&self, v: &str) -> SassResult<&Mixin> {
        match self.mixins.get(&v.replace('_', "-")) {
            Some(v) => Ok(v),
            None => Err("Undefined mixin.".into()),
        }
    }

    pub fn insert_mixin(&mut self, s: &str, v: Mixin) -> Option<Mixin> {
        self.mixins.insert(s.replace('_', "-"), v)
    }

    pub fn mixin_exists(&self, v: &str) -> bool {
        self.mixins.contains_key(&v.replace('_', "-"))
    }

    pub fn get_fn(&self, v: &str) -> SassResult<&Function> {
        match self.functions.get(&v.replace('_', "-")) {
            Some(v) => Ok(v),
            None => Err("Undefined function.".into()),
        }
    }

    pub fn insert_fn(&mut self, s: &str, v: Function) -> Option<Function> {
        self.functions.insert(s.replace('_', "-"), v)
    }

    pub fn fn_exists(&self, v: &str) -> bool {
        self.functions.contains_key(&v.replace('_', "-"))
    }

    pub fn extend(&mut self, other: Scope) {
        self.vars.extend(other.vars);
        self.mixins.extend(other.mixins);
        self.functions.extend(other.functions);
    }
}
