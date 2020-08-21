use std::fmt;

use crate::{parse::AstNode, selector::Selector};

#[derive(Debug, Clone)]
pub(crate) struct MediaRule {
    pub super_selector: Selector,
    pub query: String,
    pub body: Vec<AstNode>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub(crate) struct MediaQuery {
    /// The modifier, probably either "not" or "only".
    ///
    /// This may be `None` if no modifier is in use.
    pub modifier: Option<String>,

    /// The media type, for example "screen" or "print".
    ///
    /// This may be `None`. If so, `self.features` will not be empty.
    pub media_type: Option<String>,

    /// Feature queries, including parentheses.
    pub features: Vec<String>,
}

#[allow(dead_code)]
impl MediaQuery {
    pub fn is_condition(&self) -> bool {
        self.modifier.is_none() && self.media_type.is_none()
    }

    pub fn matches_all_types(&self) -> bool {
        self.media_type.is_none()
            || self
                .media_type
                .as_ref()
                .map_or(false, |v| v.to_ascii_lowercase() == "all")
    }

    pub fn condition(features: Vec<String>) -> Self {
        Self {
            modifier: None,
            media_type: None,
            features,
        }
    }

    #[allow(dead_code, unused_variables)]
    pub fn merge(other: &Self) -> Self {
        todo!()
    }
}

impl fmt::Display for MediaQuery {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(modifier) = &self.modifier {
            f.write_str(modifier)?;
        }
        if let Some(media_type) = &self.media_type {
            f.write_str(media_type)?;
            if !&self.features.is_empty() {
                f.write_str(" and ")?;
            }
        }
        f.write_str(&self.features.join(" and "))
    }
}
