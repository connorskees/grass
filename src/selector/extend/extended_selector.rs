use std::{
    cell::RefCell,
    hash::{Hash, Hasher},
    rc::Rc,
};

use crate::selector::{Selector, SelectorList};

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct ExtendedSelector(Rc<RefCell<SelectorList>>);

impl Hash for ExtendedSelector {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.borrow().hash(state);
    }
}

impl ExtendedSelector {
    pub fn new(selector: SelectorList) -> Self {
        Self(Rc::new(RefCell::new(selector)))
    }

    pub fn into_selector(self) -> Selector {
        Selector(self.0.borrow().clone())
    }

    pub fn set_inner(&mut self, selector: SelectorList) {
        self.0.replace(selector);
    }
}
