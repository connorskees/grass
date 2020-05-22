use std::cell::RefCell;
use std::collections::HashMap;

use codemap::Spanned;

use crate::atrule::{Function, Mixin};
use crate::common::Identifier;
use crate::error::SassResult;
use crate::value::Value;

thread_local!(pub(crate) static GLOBAL_SCOPE: RefCell<Scope> = RefCell::new(Scope::new()));

pub(crate) fn get_global_var<T: Into<Identifier>>(s: Spanned<T>) -> SassResult<Spanned<Value>> {
    GLOBAL_SCOPE.with(|scope| match scope.borrow().vars().get(&s.node.into()) {
        Some(v) => Ok(v.clone()),
        None => Err(("Undefined variable.", s.span).into()),
    })
}

/// Returns true if a variable exists in the *global* scope
pub(crate) fn global_var_exists<T: Into<Identifier>>(v: T) -> bool {
    GLOBAL_SCOPE.with(|scope| scope.borrow().vars().contains_key(&v.into()))
}

pub(crate) fn insert_global_var<T: Into<Identifier>>(
    s: T,
    v: Spanned<Value>,
) -> SassResult<Option<Spanned<Value>>> {
    GLOBAL_SCOPE.with(|scope| scope.borrow_mut().insert_var(s.into(), v))
}

pub(crate) fn get_global_fn<T: Into<Identifier>>(s: Spanned<T>) -> SassResult<Function> {
    GLOBAL_SCOPE.with(
        |scope| match scope.borrow().functions().get(&s.node.into()) {
            Some(v) => Ok(v.clone()),
            None => Err(("Undefined function.", s.span).into()),
        },
    )
}

/// Returns true if a function exists in the *global* scope
pub(crate) fn global_fn_exists<T: Into<Identifier>>(v: T) -> bool {
    GLOBAL_SCOPE.with(|scope| scope.borrow().functions().contains_key(&v.into()))
}

pub(crate) fn insert_global_fn<T: Into<Identifier>>(s: T, v: Function) -> Option<Function> {
    GLOBAL_SCOPE.with(|scope| scope.borrow_mut().insert_fn(s.into(), v))
}

pub(crate) fn get_global_mixin<T: Into<Identifier>>(s: Spanned<T>) -> SassResult<Mixin> {
    GLOBAL_SCOPE.with(|scope| match scope.borrow().mixins().get(&s.node.into()) {
        Some(v) => Ok(v.clone()),
        None => Err(("Undefined mixin.", s.span).into()),
    })
}

/// Returns true if a mixin exists in the *global* scope
pub(crate) fn global_mixin_exists<T: Into<Identifier>>(v: T) -> bool {
    GLOBAL_SCOPE.with(|scope| scope.borrow().mixins().contains_key(&v.into()))
}

pub(crate) fn insert_global_mixin<T: Into<Identifier>>(s: T, v: Mixin) -> Option<Mixin> {
    GLOBAL_SCOPE.with(|scope| scope.borrow_mut().insert_mixin(s.into(), v))
}

#[derive(Debug, Clone)]
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

    pub const fn vars(&self) -> &HashMap<Identifier, Spanned<Value>> {
        &self.vars
    }

    pub const fn functions(&self) -> &HashMap<Identifier, Function> {
        &self.functions
    }

    pub const fn mixins(&self) -> &HashMap<Identifier, Mixin> {
        &self.mixins
    }

    pub fn get_var<T: Into<Identifier>>(&self, name: Spanned<T>) -> SassResult<Spanned<Value>> {
        let name = name.map_node(|n| n.into());
        match self.vars.get(&name.node) {
            Some(v) => Ok(v.clone()),
            None => get_global_var(name),
        }
    }

    pub fn insert_var<T: Into<Identifier>>(
        &mut self,
        s: T,
        v: Spanned<Value>,
    ) -> SassResult<Option<Spanned<Value>>> {
        let Spanned { node, span } = v;
        Ok(self.vars.insert(s.into(), node.eval(span)?))
    }

    pub fn var_exists<T: Into<Identifier>>(&self, v: T) -> bool {
        let name = v.into();
        self.vars.contains_key(&name) || global_var_exists(name)
    }

    pub fn get_mixin<T: Into<Identifier>>(&self, name: Spanned<T>) -> SassResult<Mixin> {
        let name = name.map_node(|n| n.into());
        match self.mixins.get(&name.node) {
            Some(v) => Ok(v.clone()),
            None => get_global_mixin(name),
        }
    }

    pub fn insert_mixin<T: Into<Identifier>>(&mut self, s: T, v: Mixin) -> Option<Mixin> {
        self.mixins.insert(s.into(), v)
    }

    pub fn mixin_exists<T: Into<Identifier>>(&self, v: T) -> bool {
        let name = v.into();
        self.mixins.contains_key(&name) || global_mixin_exists(name)
    }

    pub fn get_fn<T: Into<Identifier>>(&self, name: Spanned<T>) -> SassResult<Function> {
        let name = name.map_node(|n| n.into());
        match self.functions.get(&name.node) {
            Some(v) => Ok(v.clone()),
            None => get_global_fn(name),
        }
    }

    pub fn insert_fn<T: Into<Identifier>>(&mut self, s: T, v: Function) -> Option<Function> {
        self.functions.insert(s.into(), v)
    }

    pub fn fn_exists<T: Into<Identifier>>(&self, v: T) -> bool {
        let name = v.into();
        self.functions.contains_key(&name) || global_fn_exists(name)
    }

    pub fn extend(&mut self, other: Scope) {
        self.vars.extend(other.vars);
        self.mixins.extend(other.mixins);
        self.functions.extend(other.functions);
    }
}
