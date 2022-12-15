// use std::ops::{BitAnd, BitOr, BitOrAssign};

// use codemap::Spanned;

// use crate::{common::Identifier, interner::InternedString, value::Value};

// #[derive(Debug, Clone)]
// pub(crate) struct NeverEmptyVec<T> {
//     first: T,
//     rest: Vec<T>,
// }

// impl<T> NeverEmptyVec<T> {
//     pub const fn new(first: T) -> Self {
//         Self {
//             first,
//             rest: Vec::new(),
//         }
//     }

//     pub fn last(&self) -> &T {
//         self.rest.last().unwrap_or(&self.first)
//     }

//     pub fn push(&mut self, value: T) {
//         self.rest.push(value);
//     }

//     pub fn pop(&mut self) -> Option<T> {
//         self.rest.pop()
//     }

//     pub fn is_empty(&self) -> bool {
//         self.rest.is_empty()
//     }
// }

// /// A toplevel element beginning with something other than
// /// `$`, `@`, `/`, whitespace, or a control character is either a
// /// selector or a style.
// #[derive(Debug)]
// pub(super) enum SelectorOrStyle {
//     Selector(String),
//     Style(InternedString, Option<Box<Spanned<Value>>>),
//     ModuleVariableRedeclaration(Identifier),
// }
