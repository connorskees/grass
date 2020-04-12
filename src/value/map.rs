use std::slice::Iter;
use std::vec::IntoIter;

use codemap::Span;

use super::Value;
use crate::common::{Brackets, ListSeparator};
use crate::error::SassResult;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct SassMap(Vec<(Value, Value)>);

impl SassMap {
    pub const fn new() -> SassMap {
        SassMap(Vec::new())
    }

    pub fn get(self, key: &Value, span: Span) -> SassResult<Option<Value>> {
        for (k, v) in self.0 {
            if k.equals(key.clone(), span)? {
                return Ok(Some(v));
            }
        }
        Ok(None)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[allow(dead_code)]
    pub fn remove(&mut self, key: &Value) {
        self.0.retain(|(ref k, ..)| k != key);
    }

    pub fn merge(&mut self, other: SassMap) {
        for (key, value) in other {
            self.insert(key, value);
        }
    }

    pub fn iter(&self) -> Iter<(Value, Value)> {
        self.0.iter()
    }

    pub fn keys(self) -> Vec<Value> {
        self.0.into_iter().map(|(k, ..)| k).collect()
    }

    pub fn values(self) -> Vec<Value> {
        self.0.into_iter().map(|(.., v)| v).collect()
    }

    pub fn entries(self) -> Vec<Value> {
        self.0
            .into_iter()
            .map(|(k, v)| Value::List(vec![k, v], ListSeparator::Space, Brackets::None))
            .collect()
    }

    /// Returns true if the key already exists
    pub fn insert(&mut self, key: Value, value: Value) -> bool {
        for (ref k, ref mut v) in &mut self.0 {
            if k == &key {
                *v = value;
                return true;
            }
        }
        self.0.push((key, value));
        false
    }
}

impl IntoIterator for SassMap {
    type Item = (Value, Value);
    type IntoIter = IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
