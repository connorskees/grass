#![allow(dead_code)]
use std::fmt;

use crate::{parse::Stmt, selector::Selector};

#[derive(Debug, Clone)]
pub(crate) struct MediaRule {
    pub super_selector: Selector,
    pub query: String,
    pub body: Vec<Stmt>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
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

    fn merge(&self, other: &Self) -> MediaQueryMergeResult {
        let this_modifier = self.modifier.as_ref().map(|m| m.to_ascii_lowercase());
        let this_type = self.media_type.as_ref().map(|m| m.to_ascii_lowercase());
        let other_modifier = other.modifier.as_ref().map(|m| m.to_ascii_lowercase());
        let other_type = other.media_type.as_ref().map(|m| m.to_ascii_lowercase());

        if this_type.is_none() && other_type.is_none() {
            return MediaQueryMergeResult::Success(Self::condition(
                self.features
                    .iter()
                    .chain(&other.features)
                    .cloned()
                    .collect(),
            ));
        }

        let modifier;
        let media_type;
        let features;

        if (this_modifier.as_deref() == Some("not")) != (other_modifier.as_deref() == Some("not")) {
            if this_modifier == other_modifier {
                let negative_features = if this_modifier.as_deref() == Some("not") {
                    &self.features
                } else {
                    &other.features
                };

                let positive_features = if this_modifier.as_deref() == Some("not") {
                    &other.features
                } else {
                    &self.features
                };

                // If the negative features are a subset of the positive features, the
                // query is empty. For example, `not screen and (color)` has no
                // intersection with `screen and (color) and (grid)`.
                //
                // However, `not screen and (color)` *does* intersect with `screen and
                // (grid)`, because it means `not (screen and (color))` and so it allows
                // a screen with no color but with a grid.

                if negative_features
                    .iter()
                    .all(|feat| positive_features.contains(&feat))
                {
                    return MediaQueryMergeResult::Empty;
                } else {
                    return MediaQueryMergeResult::Unrepresentable;
                }
            } else if self.matches_all_types() || other.matches_all_types() {
                return MediaQueryMergeResult::Unrepresentable;
            }

            if this_modifier.as_deref() == Some("not") {
                modifier = &other_modifier;
                media_type = &other_type;
                features = other.features.clone();
            } else {
                modifier = &this_modifier;
                media_type = &this_type;
                features = self.features.clone();
            }
        } else if this_modifier.as_deref() == Some("not") {
            debug_assert_eq!(other_modifier.as_deref(), Some("not"));

            // CSS has no way of representing "neither screen nor print".
            if this_type != other_type {
                return MediaQueryMergeResult::Unrepresentable;
            }

            let more_features = if self.features.len() > other.features.len() {
                &self.features
            } else {
                &other.features
            };

            let fewer_features = if self.features.len() > other.features.len() {
                &other.features
            } else {
                &self.features
            };

            // If one set of features is a superset of the other, use those features
            // because they're strictly narrower.
            if fewer_features
                .iter()
                .all(|feat| more_features.contains(feat))
            {
                modifier = &this_modifier; // "not"
                media_type = &this_type;
                features = more_features.clone();
            } else {
                // Otherwise, there's no way to represent the intersection.
                return MediaQueryMergeResult::Unrepresentable;
            }
        } else if self.matches_all_types() {
            modifier = &other_modifier;

            // Omit the type if either input query did, since that indicates that they
            // aren't targeting a browser that requires "all and".
            media_type = if other.matches_all_types() && this_type.is_none() {
                &None
            } else {
                &other_type
            };

            features = self
                .features
                .iter()
                .chain(&other.features)
                .cloned()
                .collect();
        } else if other.matches_all_types() {
            modifier = &this_modifier;
            media_type = &this_type;
            features = self
                .features
                .iter()
                .chain(&other.features)
                .cloned()
                .collect();
        } else if this_type != other_type {
            return MediaQueryMergeResult::Empty;
        } else {
            if this_modifier.is_some() {
                modifier = &this_modifier
            } else {
                modifier = &other_modifier;
            }

            media_type = &this_type;
            features = self
                .features
                .iter()
                .chain(&other.features)
                .cloned()
                .collect();
        }

        MediaQueryMergeResult::Success(MediaQuery {
            media_type: if media_type == &this_type {
                self.media_type.clone()
            } else {
                other.media_type.clone()
            },
            modifier: if modifier == &this_modifier {
                self.modifier.clone()
            } else {
                other.modifier.clone()
            },
            features,
        })
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

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
enum MediaQueryMergeResult {
    Empty,
    Unrepresentable,
    Success(MediaQuery),
}
