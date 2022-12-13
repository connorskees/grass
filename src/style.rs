use codemap::Spanned;

use crate::{error::SassResult, interner::InternedString, value::Value};

/// A style: `color: red`
#[derive(Clone, Debug)]
pub(crate) struct Style {
    pub property: InternedString,
    pub value: Box<Spanned<Value>>,
    pub declared_as_custom_property: bool,
}

impl Style {
    pub fn to_string(&self) -> SassResult<String> {
        
        Ok(format!(
            "{}:{}{};",
            self.property,
            if self.declared_as_custom_property { "" } else { " " },
            self.value.node.to_css_string(self.value.span, false)?
        ))
    }
}
