use crate::{
    builtin::modules::Modules,
    common::Identifier,
    scope::{Scope, Scopes},
    value::Value,
};
use std::{
    cell::{Ref, RefCell, RefMut},
    sync::Arc,
};

use super::visitor::CallableContentBlock;

#[derive(Debug, Clone)]
pub(crate) struct Environment {
    pub scopes: Arc<RefCell<Scopes>>,
    pub global_scope: Arc<RefCell<Scope>>,
    pub modules: Modules,
    // todo: maybe arc
    pub content: Option<Arc<CallableContentBlock>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            scopes: Arc::new(RefCell::new(Scopes::new())),
            global_scope: Arc::new(RefCell::new(Scope::new())),
            modules: Modules::default(),
            content: None,
        }
    }

    pub fn new_for_content(
        &self,
        scopes: Arc<RefCell<Scopes>>,
        content_at_decl: Option<Arc<CallableContentBlock>>,
    ) -> Self {
        Self {
            scopes, //: Arc::clone(&self.scopes), //: Arc::new(RefCell::new(self.scopes().slice(scope_idx))),
            global_scope: Arc::clone(&self.global_scope),
            modules: self.modules.clone(),
            content: content_at_decl,
        }
    }

    pub fn new_closure_idx(&self, scope_idx: usize) -> Self {
        Self {
            scopes: Arc::new(RefCell::new(self.scopes().slice(scope_idx))),
            global_scope: Arc::clone(&self.global_scope),
            modules: self.modules.clone(),
            content: self.content.as_ref().map(Arc::clone),
        }
    }

    pub fn new_closure(&self) -> Self {
        Self {
            scopes: Arc::new(RefCell::new(self.scopes().clone())),
            global_scope: Arc::clone(&self.global_scope),
            modules: self.modules.clone(),
            content: self.content.clone(),
        }
    }

    pub fn insert_var(&mut self, name: Identifier, value: Value, is_global: bool) {
        todo!()
    }

    pub fn at_root(&self) -> bool {
        (*self.scopes).borrow().is_empty()
    }

    pub fn scopes(&self) -> Ref<Scopes> {
        (*self.scopes).borrow()
    }

    pub fn scopes_mut(&mut self) -> RefMut<Scopes> {
        (*self.scopes).borrow_mut()
    }

    pub fn global_scope(&self) -> Ref<Scope> {
        (*self.global_scope).borrow()
    }

    pub fn global_scope_mut(&mut self) -> RefMut<Scope> {
        (*self.global_scope).borrow_mut()
    }
}
