use std::convert::TryFrom;

use codemap::Spanned;

use crate::{common::unvendor, error::SassError};

#[derive(Debug)]
pub enum AtRuleKind {
    // Sass specific @rules
    /// Loads mixins, functions, and variables from other Sass
    /// stylesheets, and combines CSS from multiple stylesheets together
    Use,

    /// Loads a Sass stylesheet and makes its mixins, functions,
    /// and variables available when your stylesheet is loaded
    /// with the `@use` rule
    Forward,

    /// Extends the CSS at-rule to load styles, mixins, functions,
    /// and variables from other stylesheets
    ///
    /// The definition inside `grass` however differs in that
    /// the @import rule refers to a plain css import
    /// e.g. `@import url(foo);`
    Import,

    Mixin,
    Content,
    Include,

    /// Defines custom functions that can be used in SassScript
    /// expressions
    Function,
    Return,

    /// Allows selectors to inherit styles from one another
    Extend,

    /// Puts styles within it at the root of the CSS document
    AtRoot,

    /// Causes compilation to fail with an error message
    Error,

    /// Prints a warning without stopping compilation entirely
    Warn,

    /// Prints a message for debugging purposes
    Debug,

    If,
    Each,
    For,
    While,

    // CSS @rules
    /// Defines the character set used by the style sheet
    Charset,

    /// A conditional group rule that will apply its content if the
    /// browser meets the criteria of the given condition
    Supports,

    /// Describes the aspect of intermediate steps in a CSS animation sequence
    Keyframes,
    Media,

    /// An unknown at-rule
    Unknown(String),
}

impl TryFrom<&Spanned<String>> for AtRuleKind {
    type Error = Box<SassError>;
    fn try_from(c: &Spanned<String>) -> Result<Self, Box<SassError>> {
        match c.node.as_str() {
            "use" => return Ok(Self::Use),
            "forward" => return Ok(Self::Forward),
            "import" => return Ok(Self::Import),
            "mixin" => return Ok(Self::Mixin),
            "include" => return Ok(Self::Include),
            "function" => return Ok(Self::Function),
            "return" => return Ok(Self::Return),
            "extend" => return Ok(Self::Extend),
            "at-root" => return Ok(Self::AtRoot),
            "error" => return Ok(Self::Error),
            "warn" => return Ok(Self::Warn),
            "debug" => return Ok(Self::Debug),
            "if" => return Ok(Self::If),
            "each" => return Ok(Self::Each),
            "for" => return Ok(Self::For),
            "while" => return Ok(Self::While),
            "charset" => return Ok(Self::Charset),
            "supports" => return Ok(Self::Supports),
            "content" => return Ok(Self::Content),
            "media" => return Ok(Self::Media),
            "else" => return Err(("This at-rule is not allowed here.", c.span).into()),
            "" => return Err(("Expected identifier.", c.span).into()),
            _ => {}
        }

        Ok(match unvendor(&c.node) {
            "keyframes" => Self::Keyframes,
            _ => Self::Unknown(c.node.to_owned()),
        })
    }
}
