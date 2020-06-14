use codemap::Span;

use crate::error::SassResult;

use super::{ComplexSelector, CssMediaQuery, SimpleSelector};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub(super) struct Extension {
    /// The selector in which the `@extend` appeared.
    pub extender: ComplexSelector,

    /// The selector that's being extended.
    ///
    /// `None` for one-off extensions.
    pub target: Option<SimpleSelector>,

    /// The minimum specificity required for any selector generated from this
    /// extender.
    pub specificity: i32,

    /// Whether this extension is optional.
    pub is_optional: bool,

    /// Whether this is a one-off extender representing a selector that was
    /// originally in the document, rather than one defined with `@extend`.
    pub is_original: bool,

    /// The media query context to which this extend is restricted, or `None` if
    /// it can apply within any context.
    // todo: Option
    pub media_context: Vec<CssMediaQuery>,

    /// The span in which `extender` was defined.
    pub span: Option<Span>,
}

impl Extension {
    pub fn one_off(extender: ComplexSelector, specificity: Option<i32>, is_original: bool) -> Self {
        Self {
            specificity: specificity.unwrap_or_else(|| extender.max_specificity()),
            extender,
            target: None,
            span: None,
            is_optional: true,
            is_original,
            media_context: Vec::new(),
        }
    }

    /// Asserts that the `media_context` for a selector is compatible with the
    /// query context for this extender.
    // todo: this should return a `Result`. it currently does not because the cascade effect
    // from this returning a `Result` will make some code returning `Option`s much uglier (we can't
    // use `?` to return both `Option` and `Result` from the same function)
    pub fn assert_compatible_media_context(&self, media_context: &Option<Vec<CssMediaQuery>>) {
        if let Some(media_context) = media_context {
            if &self.media_context == media_context {
                return;
            }
        }

        // Err(("You may not @extend selectors across media queries.", self.span.unwrap()).into())
    }
}
