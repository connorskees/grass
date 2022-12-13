use std::{slice::Iter, vec::IntoIter};

use crate::{
    common::{Brackets, ListSeparator},
    value::Value,
};

#[derive(Debug, Clone, Default)]
pub(crate) struct SassMap(Vec<(Value, Value)>);

impl PartialEq for SassMap {
    fn eq(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        }
        for (key, value) in &self.0 {
            if !other
                .0
                .iter()
                .any(|(key2, value2)| key == key2 && value == value2)
            {
                return false;
            }
        }
        true
    }
}

impl Eq for SassMap {}

impl SassMap {
    pub const fn new() -> SassMap {
        SassMap(Vec::new())
    }

    pub const fn new_with(elements: Vec<(Value, Value)>) -> SassMap {
        SassMap(elements)
    }

    /// We take by value here (consuming the map) in order to
    /// save a clone of the value, since the only place this
    /// should be called is in a builtin function, which throws
    /// away the map immediately anyway
    pub fn get(self, key: &Value) -> Option<Value> {
        for (k, v) in self.0 {
            if &k == key {
                return Some(v);
            }
        }

        None
    }

    pub fn get_ref(&self, key: &Value) -> Option<&Value> {
        for (k, v) in &self.0 {
            if k == key {
                return Some(v);
            }
        }

        None
    }

    pub fn remove(&mut self, key: &Value) {
        self.0.retain(|(ref k, ..)| k.not_equals(key));
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

    pub fn as_list(self) -> Vec<Value> {
        self.0
            .into_iter()
            .map(|(k, v)| Value::List(vec![k, v], ListSeparator::Space, Brackets::None))
            .collect()
    }

    #[allow(clippy::missing_const_for_fn)]
    pub fn entries(self) -> Vec<(Value, Value)> {
        self.0
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

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl IntoIterator for SassMap {
    type Item = (Value, Value);
    type IntoIter = IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
