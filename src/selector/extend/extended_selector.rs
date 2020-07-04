use std::{
    cell::RefCell,
    hash::{Hash, Hasher},
    rc::Rc,
    vec::IntoIter
};

use crate::selector::{Selector, SelectorList};

#[derive(Debug, Clone)]
pub(crate) struct ExtendedSelector(Rc<RefCell<SelectorList>>);

impl PartialEq for ExtendedSelector {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for ExtendedSelector {}

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

/// We could use a `HashSet` here, but since selectors are
/// in a `RefCell`, we may unintentionally cause (library) UB.
///
/// Initial benchmarking found that a `Vec` was actually *faster*
/// than a `HashSet`, the reason for which I am unsure.
#[derive(Clone, Debug)]
pub(crate) struct SelectorHashSet(Vec<ExtendedSelector>);

impl SelectorHashSet {
    pub const fn new() -> Self {
        Self(Vec::new())
    }

    pub fn insert(&mut self, selector: ExtendedSelector) {
        if !self.0.contains(&selector) {
            self.0.push(selector);
        }
    }

    pub fn into_iter(self) -> IntoIter<ExtendedSelector> {
        self.0.into_iter()
    }
}
