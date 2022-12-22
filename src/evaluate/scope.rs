use std::{
    cell::{Cell, RefCell},
    collections::BTreeMap,
    sync::Arc,
};

use codemap::Spanned;

use crate::{
    atrule::mixin::Mixin,
    builtin::GLOBAL_FUNCTIONS,
    common::Identifier,
    error::SassResult,
    value::{SassFunction, Value},
};

#[derive(Debug, Default, Clone)]
pub(crate) struct Scopes {
    variables: Arc<RefCell<Vec<Arc<RefCell<BTreeMap<Identifier, Value>>>>>>,
    mixins: Arc<RefCell<Vec<Arc<RefCell<BTreeMap<Identifier, Mixin>>>>>>,
    functions: Arc<RefCell<Vec<Arc<RefCell<BTreeMap<Identifier, SassFunction>>>>>>,
    len: Arc<Cell<usize>>,
}

impl Scopes {
    pub fn new() -> Self {
        Self {
            variables: Arc::new(RefCell::new(vec![Arc::new(RefCell::new(BTreeMap::new()))])),
            mixins: Arc::new(RefCell::new(vec![Arc::new(RefCell::new(BTreeMap::new()))])),
            functions: Arc::new(RefCell::new(vec![Arc::new(RefCell::new(BTreeMap::new()))])),
            len: Arc::new(Cell::new(1)),
        }
    }

    pub fn new_closure(&self) -> Self {
        debug_assert_eq!(self.len(), (*self.variables).borrow().len());
        Self {
            variables: Arc::new(RefCell::new(
                (*self.variables).borrow().iter().map(Arc::clone).collect(),
            )),
            mixins: Arc::new(RefCell::new(
                (*self.mixins).borrow().iter().map(Arc::clone).collect(),
            )),
            functions: Arc::new(RefCell::new(
                (*self.functions).borrow().iter().map(Arc::clone).collect(),
            )),
            len: Arc::new(Cell::new(self.len())),
        }
    }

    pub fn global_variables(&self) -> Arc<RefCell<BTreeMap<Identifier, Value>>> {
        debug_assert_eq!(self.len(), (*self.variables).borrow().len());
        Arc::clone(&(*self.variables).borrow()[0])
    }

    pub fn global_functions(&self) -> Arc<RefCell<BTreeMap<Identifier, SassFunction>>> {
        Arc::clone(&(*self.functions).borrow()[0])
    }

    pub fn global_mixins(&self) -> Arc<RefCell<BTreeMap<Identifier, Mixin>>> {
        Arc::clone(&(*self.mixins).borrow()[0])
    }

    pub fn find_var(&self, name: Identifier) -> Option<usize> {
        debug_assert_eq!(self.len(), (*self.variables).borrow().len());
        for (idx, scope) in (*self.variables).borrow().iter().enumerate().rev() {
            if (**scope).borrow().contains_key(&name) {
                return Some(idx);
            }
        }

        None
    }

    pub fn len(&self) -> usize {
        (*self.len).get()
    }

    pub fn enter_new_scope(&mut self) {
        let len = self.len();
        debug_assert_eq!(self.len(), (*self.variables).borrow().len());
        (*self.len).set(len + 1);
        (*self.variables)
            .borrow_mut()
            .push(Arc::new(RefCell::new(BTreeMap::new())));
        (*self.mixins)
            .borrow_mut()
            .push(Arc::new(RefCell::new(BTreeMap::new())));
        (*self.functions)
            .borrow_mut()
            .push(Arc::new(RefCell::new(BTreeMap::new())));
    }

    pub fn exit_scope(&mut self) {
        debug_assert_eq!(self.len(), (*self.variables).borrow().len());
        let len = self.len();
        (*self.len).set(len - 1);
        (*self.variables).borrow_mut().pop();
        (*self.mixins).borrow_mut().pop();
        (*self.functions).borrow_mut().pop();
    }
}

/// Variables
impl Scopes {
    pub fn insert_var(&mut self, idx: usize, name: Identifier, v: Value) -> Option<Value> {
        debug_assert_eq!(self.len(), (*self.variables).borrow().len());
        (*(*self.variables).borrow_mut()[idx])
            .borrow_mut()
            .insert(name, v)
    }

    /// Always insert this variable into the innermost scope
    ///
    /// Used, for example, for variables from `@each` and `@for`
    pub fn insert_var_last(&mut self, name: Identifier, v: Value) -> Option<Value> {
        debug_assert_eq!(self.len(), (*self.variables).borrow().len());
        (*(*self.variables).borrow_mut()[self.len() - 1])
            .borrow_mut()
            .insert(name, v)
    }

    pub fn get_var(&self, name: Spanned<Identifier>) -> SassResult<Value> {
        debug_assert_eq!(self.len(), (*self.variables).borrow().len());
        for scope in (*self.variables).borrow().iter().rev() {
            match (**scope).borrow().get(&name.node) {
                Some(var) => return Ok(var.clone()),
                None => continue,
            }
        }

        Err(("Undefined variable.", name.span).into())
    }

    pub fn var_exists(&self, name: Identifier) -> bool {
        debug_assert_eq!(self.len(), (*self.variables).borrow().len());
        for scope in (*self.variables).borrow().iter() {
            if (**scope).borrow().contains_key(&name) {
                return true;
            }
        }

        false
    }
}

/// Mixins
impl Scopes {
    pub fn insert_mixin(&mut self, name: Identifier, mixin: Mixin) {
        debug_assert_eq!(self.len(), (*self.variables).borrow().len());
        (*(*self.mixins).borrow_mut().last_mut().unwrap())
            .borrow_mut()
            .insert(name, mixin);
    }

    pub fn get_mixin(&self, name: Spanned<Identifier>) -> SassResult<Mixin> {
        debug_assert_eq!(self.len(), (*self.variables).borrow().len());
        for scope in (*self.mixins).borrow().iter().rev() {
            match (**scope).borrow().get(&name.node) {
                Some(mixin) => return Ok(mixin.clone()),
                None => continue,
            }
        }

        Err(("Undefined mixin.", name.span).into())
    }

    pub fn mixin_exists(&self, name: Identifier) -> bool {
        debug_assert_eq!(self.len(), (*self.variables).borrow().len());
        for scope in (*self.mixins).borrow().iter() {
            if (**scope).borrow().contains_key(&name) {
                return true;
            }
        }

        false
    }
}

/// Functions
impl Scopes {
    pub fn insert_fn(&mut self, func: SassFunction) {
        debug_assert_eq!(self.len(), (*self.variables).borrow().len());
        (*(*self.functions).borrow_mut().last_mut().unwrap())
            .borrow_mut()
            .insert(func.name(), func);
    }

    pub fn get_fn(&self, name: Identifier) -> Option<SassFunction> {
        debug_assert_eq!(self.len(), (*self.variables).borrow().len());
        for scope in (*self.functions).borrow().iter().rev() {
            let func = (**scope).borrow().get(&name).cloned();

            if func.is_some() {
                return func;
            }
        }

        None
    }

    pub fn fn_exists(&self, name: Identifier) -> bool {
        debug_assert_eq!(self.len(), (*self.variables).borrow().len());
        for scope in (*self.functions).borrow().iter() {
            if (**scope).borrow().contains_key(&name) {
                return true;
            }
        }

        GLOBAL_FUNCTIONS.contains_key(name.as_str())
    }
}
