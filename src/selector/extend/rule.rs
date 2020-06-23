use codemap::Span;

use crate::selector::Selector;

#[derive(Clone, Debug)]
pub(crate) struct ExtendRule {
    pub selector: Selector,
    pub is_optional: bool,
    pub span: Span,
}

impl ExtendRule {
    pub const fn new(selector: Selector, is_optional: bool, span: Span) -> Self {
        Self {
            selector,
            is_optional,
            span,
        }
    }
}
