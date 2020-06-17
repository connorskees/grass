use std::collections::HashMap;

use codemap::Spanned;

use crate::{
    atrule::{Function, Mixin},
    builtin::GLOBAL_FUNCTIONS,
    common::Identifier,
    error::SassResult,
    value::Value,
};

#[derive(Debug, Clone, Default)]
pub(crate) struct Scope {
    vars: HashMap<Identifier, Spanned<Value>>,
    mixins: HashMap<Identifier, Mixin>,
    functions: HashMap<Identifier, Function>,
}

// todo: separate struct for global scope?
impl Scope {
    #[must_use]
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            mixins: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    fn get_var_no_global(&self, name: &Spanned<Identifier>) -> SassResult<Spanned<Value>> {
        match self.vars.get(&name.node) {
            Some(v) => Ok(v.clone()),
            None => Err(("Undefined variable.", name.span).into()),
        }
    }

    pub fn get_var<T: Into<Identifier>>(
        &self,
        name: Spanned<T>,
        global_scope: &Scope,
    ) -> SassResult<Spanned<Value>> {
        let name = name.map_node(Into::into);
        match self.vars.get(&name.node) {
            Some(v) => Ok(v.clone()),
            None => global_scope.get_var_no_global(&name),
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

    pub fn var_exists_no_global(&self, name: &Identifier) -> bool {
        self.vars.contains_key(name)
    }

    pub fn var_exists<'a, T: Into<&'a Identifier>>(&self, v: T, global_scope: &Scope) -> bool {
        let name = v.into();
        self.vars.contains_key(name) || global_scope.var_exists_no_global(name)
    }

    fn get_mixin_no_global(&self, name: &Spanned<Identifier>) -> SassResult<Mixin> {
        match self.mixins.get(&name.node) {
            Some(v) => Ok(v.clone()),
            None => Err(("Undefined mixin.", name.span).into()),
        }
    }

    pub fn get_mixin<T: Into<Identifier>>(
        &self,
        name: Spanned<T>,
        global_scope: &Scope,
    ) -> SassResult<Mixin> {
        let name = name.map_node(Into::into);
        match self.mixins.get(&name.node) {
            Some(v) => Ok(v.clone()),
            None => global_scope.get_mixin_no_global(&name),
        }
    }

    pub fn insert_mixin<T: Into<Identifier>>(&mut self, s: T, v: Mixin) -> Option<Mixin> {
        self.mixins.insert(s.into(), v)
    }

    fn mixin_exists_no_global(&self, name: &Identifier) -> bool {
        self.mixins.contains_key(name)
    }

    pub fn mixin_exists<T: Into<Identifier>>(&self, v: T, global_scope: &Scope) -> bool {
        let name = v.into();
        self.mixins.contains_key(&name) || global_scope.mixin_exists_no_global(&name)
    }

    fn get_fn_no_global(&self, name: &Spanned<Identifier>) -> SassResult<Function> {
        match self.functions.get(&name.node) {
            Some(v) => Ok(v.clone()),
            None => Err(("Undefined function.", name.span).into()),
        }
    }

    pub fn get_fn<T: Into<Identifier>>(
        &self,
        name: Spanned<T>,
        global_scope: &Scope,
    ) -> SassResult<Function> {
        let name = name.map_node(Into::into);
        match self.functions.get(&name.node) {
            Some(v) => Ok(v.clone()),
            None => global_scope.get_fn_no_global(&name),
        }
    }

    pub fn insert_fn<T: Into<Identifier>>(&mut self, s: T, v: Function) -> Option<Function> {
        self.functions.insert(s.into(), v)
    }

    fn fn_exists_no_global(&self, name: &Identifier) -> bool {
        self.functions.contains_key(name)
    }

    pub fn fn_exists<T: Into<Identifier>>(&self, v: T, global_scope: &Scope) -> bool {
        let name = v.into();
        self.functions.contains_key(&name)
            || global_scope.fn_exists_no_global(&name)
            || GLOBAL_FUNCTIONS.contains_key(name.clone().into_inner().as_str())
            // special functions not in the `GLOBAL_FUNCTIONS` map
            || matches!(
                name.into_inner().as_str(),
                "function-exists"
                    | "content-exists"
                    | "mixin-exists"
                    | "variable-exists"
                    | "global-variable-exists"
                    | "get-function"
                    | "call"
            )
    }

    #[allow(dead_code)]
    pub fn extend(&mut self, other: Scope) {
        self.vars.extend(other.vars);
        self.mixins.extend(other.mixins);
        self.functions.extend(other.functions);
    }
}
