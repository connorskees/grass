use std::convert::TryFrom;

use codemap::Spanned;

use crate::error::SassError;

#[derive(Clone, Debug, Eq, PartialEq)]
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
    type Error = SassError;
    fn try_from(c: &Spanned<String>) -> Result<Self, SassError> {
        Ok(match c.node.as_str() {
            "use" => Self::Use,
            "forward" => Self::Forward,
            "import" => Self::Import,
            "mixin" => Self::Mixin,
            "include" => Self::Include,
            "function" => Self::Function,
            "return" => Self::Return,
            "extend" => Self::Extend,
            "at-root" => Self::AtRoot,
            "error" => Self::Error,
            "warn" => Self::Warn,
            "debug" => Self::Debug,
            "if" => Self::If,
            "each" => Self::Each,
            "for" => Self::For,
            "while" => Self::While,
            "charset" => Self::Charset,
            "supports" => Self::Supports,
            "keyframes" => Self::Keyframes,
            "content" => Self::Content,
            "media" => Self::Media,
            "else" => return Err(("This at-rule is not allowed here.", c.span).into()),
            "" => return Err(("Expected identifier.", c.span).into()),
            s => Self::Unknown(s.to_owned()),
        })
    }
}
