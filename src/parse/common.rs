use std::slice::IterMut;

use codemap::Spanned;

use crate::{value::Value, Token};

pub(crate) struct NeverEmptyVec<T> {
    first: T,
    rest: Vec<T>,
}

impl<T> NeverEmptyVec<T> {
    pub const fn new(first: T) -> Self {
        Self {
            first,
            rest: Vec::new(),
        }
    }

    pub fn last(&self) -> &T {
        self.rest.last().unwrap_or(&self.first)
    }

    pub fn last_mut(&mut self) -> &mut T {
        self.rest.last_mut().unwrap_or(&mut self.first)
    }

    pub fn first(&mut self) -> &T {
        &self.first
    }

    pub fn first_mut(&mut self) -> &mut T {
        &mut self.first
    }

    pub fn push(&mut self, value: T) {
        self.rest.push(value)
    }

    pub fn pop(&mut self) -> Option<T> {
        self.rest.pop()
    }

    pub fn iter_mut(&mut self) -> IterMut<'_, T> {
        self.rest.iter_mut()
    }

    pub fn len(&self) -> usize {
        self.rest.len()
    }

    pub fn is_empty(&self) -> bool {
        self.rest.is_empty()
    }
}

/// A toplevel element beginning with something other than
/// `$`, `@`, `/`, whitespace, or a control character is either a
/// selector or a style.
#[derive(Debug)]
pub(super) enum SelectorOrStyle {
    Selector(String),
    Style(String, Option<Box<Spanned<Value>>>),
}

#[derive(Debug, Clone)]
pub(super) struct Branch {
    pub cond: Vec<Token>,
    pub toks: Vec<Token>,
}

impl Branch {
    pub fn new(cond: Vec<Token>, toks: Vec<Token>) -> Branch {
        Branch { cond, toks }
    }
}
