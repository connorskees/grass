use std::sync::Arc;

use codemap::Spanned;

use crate::{interner::InternedString, value::Value};

/// A style: `color: red`
#[derive(Clone, Debug)]
pub(crate) struct Style {
    pub property: InternedString,
    pub value: Arc<Spanned<Value>>,
    pub declared_as_custom_property: bool,
}
