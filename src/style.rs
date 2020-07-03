use codemap::Spanned;

use crate::{error::SassResult, value::Value};

/// A style: `color: red`
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Style {
    pub property: String,
    pub value: Box<Spanned<Value>>,
}

impl Style {
    pub fn to_string(&self) -> SassResult<String> {
        Ok(format!(
            "{}: {};",
            self.property,
            self.value.node.to_css_string(self.value.span)?
        ))
    }
}
