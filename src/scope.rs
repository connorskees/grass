use std::cell::RefCell;
use std::collections::HashMap;

use codemap::Spanned;

use crate::atrule::{Function, Mixin};
use crate::error::SassResult;
use crate::value::Value;

thread_local!(pub(crate) static GLOBAL_SCOPE: RefCell<Scope> = RefCell::new(Scope::new()));

pub(crate) fn get_global_var(s: Spanned<String>) -> SassResult<Spanned<Value>> {
    GLOBAL_SCOPE.with(|scope| match scope.borrow().vars().get(&s.node) {
        Some(v) => Ok(v.clone()),
        None => Err(("Undefined variable.", s.span).into()),
    })
}

pub(crate) fn global_var_exists(v: &str) -> bool {
    GLOBAL_SCOPE.with(|scope| scope.borrow().vars().contains_key(&v.replace('_', "-")))
}

pub(crate) fn insert_global_var(s: &str, v: Spanned<Value>) -> SassResult<Option<Spanned<Value>>> {
    GLOBAL_SCOPE.with(|scope| scope.borrow_mut().insert_var(s, v))
}

pub(crate) fn get_global_fn(s: Spanned<String>) -> SassResult<Function> {
    GLOBAL_SCOPE.with(|scope| match scope.borrow().functions().get(&s.node) {
        Some(v) => Ok(v.clone()),
        None => Err(("Undefined function.", s.span).into()),
    })
}

pub(crate) fn global_fn_exists(v: &str) -> bool {
    GLOBAL_SCOPE.with(|scope| {
        scope
            .borrow()
            .functions()
            .contains_key(&v.replace('_', "-"))
    })
}

pub(crate) fn insert_global_fn(s: &str, v: Function) -> Option<Function> {
    GLOBAL_SCOPE.with(|scope| scope.borrow_mut().insert_fn(s, v))
}

pub(crate) fn get_global_mixin(s: Spanned<String>) -> SassResult<Mixin> {
    GLOBAL_SCOPE.with(|scope| match scope.borrow().mixins().get(&s.node) {
        Some(v) => Ok(v.clone()),
        None => Err(("Undefined mixin.", s.span).into()),
    })
}

pub(crate) fn global_mixin_exists(v: &str) -> bool {
    GLOBAL_SCOPE.with(|scope| scope.borrow().mixins().contains_key(&v.replace('_', "-")))
}

pub(crate) fn insert_global_mixin(s: &str, v: Mixin) -> Option<Mixin> {
    GLOBAL_SCOPE.with(|scope| scope.borrow_mut().insert_mixin(s, v))
}

#[derive(Debug, Clone)]
pub(crate) struct Scope {
    vars: HashMap<String, Spanned<Value>>,
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

    pub const fn vars(&self) -> &HashMap<String, Spanned<Value>> {
        &self.vars
    }

    pub const fn functions(&self) -> &HashMap<String, Function> {
        &self.functions
    }

    pub const fn mixins(&self) -> &HashMap<String, Mixin> {
        &self.mixins
    }

    pub fn get_var(&self, mut name: Spanned<String>) -> SassResult<Spanned<Value>> {
        name.node = name.node.replace('_', "-");
        match self.vars.get(&name.node) {
            Some(v) => Ok(v.clone()),
            None => get_global_var(name),
        }
    }

    pub fn insert_var(&mut self, s: &str, v: Spanned<Value>) -> SassResult<Option<Spanned<Value>>> {
        let Spanned { node, span } = v;
        Ok(self.vars.insert(s.replace('_', "-"), node.eval(span)?))
    }

    pub fn var_exists(&self, v: &str) -> bool {
        let name = &v.replace('_', "-");
        self.vars.contains_key(name) || global_var_exists(name)
    }

    pub fn get_mixin(&self, mut name: Spanned<String>) -> SassResult<Mixin> {
        name.node = name.node.replace('_', "-");
        match self.mixins.get(&name.node) {
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

    pub fn get_fn(&self, mut name: Spanned<String>) -> SassResult<Function> {
        name.node = name.node.replace('_', "-");
        match self.functions.get(&name.node) {
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
