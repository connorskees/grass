use std::{hash::Hasher, ops::Deref, sync::Arc};

use codemap::Spanned;
use indexmap::{map::IntoIter, IndexMap};

use crate::{
    common::{Brackets, ListSeparator},
    evaluate::unwrap_arc,
    value::Value,
};

#[derive(Debug, Clone, Default)]
pub(crate) struct SassMap(IndexMap<SpannedValueWrapper, Arc<Value>>);

#[derive(Debug, Eq, Clone)]
#[repr(transparent)]
pub(crate) struct SpannedValueWrapper(pub Spanned<Value>);

impl SpannedValueWrapper {
    pub fn wrap_ref(val: &Spanned<Value>) -> &SpannedValueWrapper {
        // SAFETY: `SpannedValueWrapper` is a repr(transparent) wrapper over
        // Spanned<Value>. this is effectively a transmute. it saves us a lot of
        // key clones
        unsafe { &*(val as *const Spanned<Value> as *const SpannedValueWrapper) }
    }
}

impl PartialEq for SpannedValueWrapper {
    fn eq(&self, other: &Self) -> bool {
        self.0.node == other.0.node
    }
}

impl Deref for SpannedValueWrapper {
    type Target = Spanned<Value>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::hash::Hash for SpannedValueWrapper {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.node.hash(state);
    }
}

impl PartialEq for SassMap {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for SassMap {}

impl SassMap {
    pub fn new() -> SassMap {
        SassMap(IndexMap::new())
    }

    pub const fn new_with(elements: IndexMap<SpannedValueWrapper, Arc<Value>>) -> SassMap {
        SassMap(elements)
    }

    pub fn get(mut self, key: &Spanned<Value>) -> Option<Arc<Value>> {
        self.0.remove(SpannedValueWrapper::wrap_ref(key))
    }

    pub fn get_ref(&self, key: &Spanned<Value>) -> Option<Arc<Value>> {
        self.0.get(SpannedValueWrapper::wrap_ref(key)).map(Arc::clone)
    }

    pub fn key_exists(&self, key: &Spanned<Value>) -> bool {
        self.0.contains_key(SpannedValueWrapper::wrap_ref(key))
    }

    pub fn remove(&mut self, key: &Spanned<Value>) {
        self.0.remove(SpannedValueWrapper::wrap_ref(key));
        // self.0.retain(|ref k, _| k.not_equals(key));
    }

    pub fn merge(&mut self, other: SassMap) {
        self.0.extend(other)
        // for (key, value) in other {
        //     self.insert(key, value);
        // }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&SpannedValueWrapper, &Arc<Value>)> + '_ {
        self.0.iter()
    }

    pub fn keys(self) -> Vec<Value> {
        self.0.into_keys().map(|k| k.0.node).collect()
    }

    pub fn values(self) -> Vec<Arc<Value>> {
        self.0.into_values().collect()
    }

    pub fn as_list(self) -> Vec<Arc<Value>> {
        self.0
            .into_iter()
            .map(|(k, v)| {
                Arc::new(Value::List(
                    vec![Arc::new(k.0.node), v],
                    ListSeparator::Space,
                    Brackets::None,
                ))
            })
            .collect()
    }

    /// Returns true if the key already exists
    pub fn insert(&mut self, key: Spanned<Value>, value: Arc<Value>) -> bool {
        self.0.insert(SpannedValueWrapper(key), value).is_some()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl IntoIterator for SassMap {
    type Item = (SpannedValueWrapper, Arc<Value>);
    type IntoIter = IntoIter<SpannedValueWrapper, Arc<Value>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
