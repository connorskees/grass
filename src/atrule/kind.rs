use std::fmt::{self, Display};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AtRuleKind {
    // SASS specific @rules
    /// Loads mixins, functions, and variables from other Sass
    /// stylesheets, and combines CSS from multiple stylesheets together
    Use,
    /// Loads a Sass stylesheet and makes its mixins, functions,
    /// and variables available when your stylesheet is loaded
    /// with the `@use` rule
    Forward,
    /// Extends the CSS at-rule to load styles, mixins, functions,
    /// and variables from other stylesheets
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
    Else,
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

    /// An unknown at rule.
    /// For forward compatibility, they are parsed the same as @media
    Unknown(String),
}

impl From<&str> for AtRuleKind {
    fn from(c: &str) -> Self {
        match c.to_ascii_lowercase().as_str() {
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
            "else" => Self::Else,
            "each" => Self::Each,
            "for" => Self::For,
            "while" => Self::While,
            "charset" => Self::Charset,
            "supports" => Self::Supports,
            "keyframes" => Self::Keyframes,
            "content" => Self::Content,
            s => Self::Unknown(s.to_owned()),
        }
    }
}

impl Display for AtRuleKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Use => write!(f, "@use"),
            Self::Forward => write!(f, "@forward"),
            Self::Import => write!(f, "@import"),
            Self::Mixin => write!(f, "@mixin"),
            Self::Include => write!(f, "@include"),
            Self::Function => write!(f, "@function"),
            Self::Return => write!(f, "@return"),
            Self::Extend => write!(f, "@extend"),
            Self::AtRoot => write!(f, "@at-root"),
            Self::Error => write!(f, "@error"),
            Self::Warn => write!(f, "@warn"),
            Self::Debug => write!(f, "@debug"),
            Self::If => write!(f, "@if"),
            Self::Else => write!(f, "@else"),
            Self::Each => write!(f, "@each"),
            Self::For => write!(f, "@for"),
            Self::While => write!(f, "@while"),
            Self::Charset => write!(f, "@charset"),
            Self::Supports => write!(f, "@supports"),
            Self::Keyframes => write!(f, "@keyframes"),
            Self::Content => write!(f, "@content"),
            Self::Unknown(s) => write!(f, "@{}", s),
        }
    }
}
