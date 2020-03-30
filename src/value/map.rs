use std::slice::Iter;

use super::Value;
use crate::error::SassResult;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct SassMap(Vec<(Value, Value)>);

impl SassMap {
    pub fn new() -> SassMap {
        SassMap(Vec::new())
    }

    pub fn get(self, key: Value) -> SassResult<Option<Value>> {
        for (k, v) in self.0 {
            if k.equals(key.clone())? {
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
        self.0.extend(other.0);
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

    pub fn insert(&mut self, key: Value, value: Value) {
        for &mut (ref k, ref mut v) in &mut self.0 {
            if k == &key {
                *v = value;
                return;
            }
        }
        self.0.push((key, value));
    }
}
