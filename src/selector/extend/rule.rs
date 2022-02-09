use codemap::Span;

use crate::selector::Selector;

#[derive(Clone, Debug)]
pub(crate) struct ExtendRule {
    #[allow(dead_code)]
    pub selector: Selector,
    pub is_optional: bool,
    #[allow(dead_code)]
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
