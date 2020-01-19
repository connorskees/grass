use std::collections::HashMap;
use std::convert::TryFrom;
use std::default::Default;
use std::fmt::{self, Display};

use crate::mixin::Mixin;
use crate::Token;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Symbol {
    /// .
    Period,
    /// #
    Hash,
    /// @
    At,
    /// $
    Dollar,
    /// (
    OpenParen,
    /// )
    CloseParen,
    /// {
    OpenCurlyBrace,
    /// }
    CloseCurlyBrace,
    /// [
    OpenSquareBrace,
    /// ]
    CloseSquareBrace,
    /// ,
    Comma,
    /// +
    Plus,
    /// -
    Minus,
    /// *
    Mul,
    /// /
    Div,
    /// :
    Colon,
    /// ;
    SemiColon,
    /// ~
    Tilde,
    /// >
    Gt,
    /// <
    Lt,
    /// ^
    Xor,
    /// =
    Equal,
    /// |
    BitOr,
    /// &
    BitAnd,
    /// %
    Percent,
    /// "
    DoubleQuote,
    /// '
    SingleQuote,
}

impl Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Period => write!(f, "."),
            Self::Hash => write!(f, "#"),
            Self::At => write!(f, "@"),
            Self::Dollar => write!(f, "$"),
            Self::OpenParen => write!(f, "("),
            Self::CloseParen => write!(f, "),"),
            Self::OpenCurlyBrace => write!(f, "{{"),
            Self::CloseCurlyBrace => write!(f, "}}"),
            Self::OpenSquareBrace => write!(f, "["),
            Self::CloseSquareBrace => write!(f, "]"),
            Self::Comma => write!(f, ","),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Colon => write!(f, ":"),
            Self::SemiColon => write!(f, ";"),
            Self::Tilde => write!(f, "~"),
            Self::Gt => write!(f, ">"),
            Self::Lt => write!(f, "<"),
            Self::Xor => write!(f, "^"),
            Self::Equal => write!(f, "="),
            Self::BitOr => write!(f, "|"),
            Self::BitAnd => write!(f, "&"),
            Self::Percent => write!(f, "%"),
            Self::DoubleQuote => write!(f, "\""),
            Self::SingleQuote => write!(f, "'"),
        }
    }
}

impl TryFrom<char> for Symbol {
    type Error = &'static str;

    fn try_from(c: char) -> Result<Self, Self::Error> {
        match c {
            '.' => Ok(Self::Period),
            '#' => Ok(Self::Hash),
            '@' => Ok(Self::At),
            '$' => Ok(Self::Dollar),
            '(' => Ok(Self::OpenParen),
            ')' => Ok(Self::CloseParen),
            '{' => Ok(Self::OpenCurlyBrace),
            '}' => Ok(Self::CloseCurlyBrace),
            '[' => Ok(Self::OpenSquareBrace),
            ']' => Ok(Self::CloseSquareBrace),
            ',' => Ok(Self::Comma),
            '+' => Ok(Self::Plus),
            '-' => Ok(Self::Minus),
            '*' => Ok(Self::Mul),
            '/' => Ok(Self::Div),
            ':' => Ok(Self::Colon),
            ';' => Ok(Self::SemiColon),
            '~' => Ok(Self::Tilde),
            '>' => Ok(Self::Gt),
            '<' => Ok(Self::Lt),
            '^' => Ok(Self::Xor),
            '=' => Ok(Self::Equal),
            '|' => Ok(Self::BitOr),
            '&' => Ok(Self::BitAnd),
            '%' => Ok(Self::Percent),
            '"' => Ok(Self::DoubleQuote),
            '\'' => Ok(Self::SingleQuote),
            _ => Err("invalid symbol"),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MediaQuery {}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AtRule {
    // SASS specific @rules
    /// Loads mixins, functions, and variables from other Sass stylesheets, and combines CSS from multiple stylesheets together
    Use,
    /// Loads a Sass stylesheet and makes its mixins, functions, and variables available when your stylesheet is loaded with the `@use` rule
    Forward,
    /// Extends the CSS at-rule to load styles, mixins, functions, and variables from other stylesheets
    Import,
    Mixin,
    Include,
    /// Defines custom functions that can be used in SassScript expressions
    Function,
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
    /// Tells the CSS engine that all its content must be considered prefixed with an XML namespace
    Namespace,
    /// A conditional group rule that will apply its content if the device meets the criteria of the condition defined using a media query
    Media,
    /// A conditional group rule that will apply its content if the browser meets the criteria of the given condition
    Supports,
    /// Describes the aspect of layout changes that will be applied when printing the document
    Page,
    /// Describes the aspect of an external font to be downloaded
    FontFace,
    /// Describes the aspect of intermediate steps in a CSS animation sequence
    Keyframes,

    // @rules related to @font-feature-values
    FontFeatureValues,
    Swash,
    Ornaments,
    Annotation,
    Stylistic,
    Styleset,
    CharacterVariant,

    // Experimental CSS @rules
    /// Describes the aspects of the viewport for small screen devices
    ///
    /// Currently at the Working Draft stage
    Viewport,
    /// A conditional group rule that will apply its content if the document in which the style sheet is applied meets the criteria of the given condition
    ///
    /// Deferred to Level 4 of CSS Spec
    Document,
    /// Defines specific counter styles that are not part of the predefined set of styles
    ///
    /// At the Candidate Recommendation stage
    CounterStyle,
}

impl TryFrom<&str> for AtRule {
    type Error = &'static str;

    fn try_from(c: &str) -> Result<Self, &'static str> {
        match c {
            "use" => Ok(Self::Use),
            "forward" => Ok(Self::Forward),
            "import" => Ok(Self::Import),
            "mixin" => Ok(Self::Mixin),
            "include" => Ok(Self::Include),
            "function" => Ok(Self::Function),
            "extend" => Ok(Self::Extend),
            "atroot" => Ok(Self::AtRoot),
            "error" => Ok(Self::Error),
            "warn" => Ok(Self::Warn),
            "debug" => Ok(Self::Debug),
            "if" => Ok(Self::If),
            "each" => Ok(Self::Each),
            "for" => Ok(Self::For),
            "while" => Ok(Self::While),
            "charset" => Ok(Self::Charset),
            "namespace" => Ok(Self::Namespace),
            "media" => Ok(Self::Media),
            "supports" => Ok(Self::Supports),
            "page" => Ok(Self::Page),
            "fontface" => Ok(Self::FontFace),
            "keyframes" => Ok(Self::Keyframes),
            "fontfeaturevalues" => Ok(Self::FontFeatureValues),
            "swash" => Ok(Self::Swash),
            "ornaments" => Ok(Self::Ornaments),
            "annotation" => Ok(Self::Annotation),
            "stylistic" => Ok(Self::Stylistic),
            "styleset" => Ok(Self::Styleset),
            "charactervariant" => Ok(Self::CharacterVariant),
            "viewport" => Ok(Self::Viewport),
            "document" => Ok(Self::Document),
            "counterstyle" => Ok(Self::CounterStyle),
            _ => Err("invalid at rule"),
        }
    }
}

impl Display for AtRule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Use => write!(f, "@use"),
            Self::Forward => write!(f, "@forward"),
            Self::Import => write!(f, "@import"),
            Self::Mixin => write!(f, "@mixin"),
            Self::Include => write!(f, "@include"),
            Self::Function => write!(f, "@function"),
            Self::Extend => write!(f, "@extend"),
            Self::AtRoot => write!(f, "@atroot"),
            Self::Error => write!(f, "@error"),
            Self::Warn => write!(f, "@warn"),
            Self::Debug => write!(f, "@debug"),
            Self::If => write!(f, "@if"),
            Self::Each => write!(f, "@each"),
            Self::For => write!(f, "@for"),
            Self::While => write!(f, "@while"),
            Self::Charset => write!(f, "@charset"),
            Self::Namespace => write!(f, "@namespace"),
            Self::Media => write!(f, "@media"),
            Self::Supports => write!(f, "@supports"),
            Self::Page => write!(f, "@page"),
            Self::FontFace => write!(f, "@fontface"),
            Self::Keyframes => write!(f, "@keyframes"),
            Self::FontFeatureValues => write!(f, "@fontfeaturevalues"),
            Self::Swash => write!(f, "@swash"),
            Self::Ornaments => write!(f, "@ornaments"),
            Self::Annotation => write!(f, "@annotation"),
            Self::Stylistic => write!(f, "@stylistic"),
            Self::Styleset => write!(f, "@styleset"),
            Self::CharacterVariant => write!(f, "@charactervariant"),
            Self::Viewport => write!(f, "@viewport"),
            Self::Document => write!(f, "@document"),
            Self::CounterStyle => write!(f, "@counterstyle"),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Whitespace {
    Space,
    Tab,
    Newline,
    CarriageReturn,
}

impl Display for Whitespace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Space => write!(f, " "),
            Self::Tab => write!(f, "\t"),
            Self::Newline => writeln!(f),
            Self::CarriageReturn => write!(f, "\r"),
        }
    }
}

impl TryFrom<char> for Whitespace {
    type Error = &'static str;

    fn try_from(c: char) -> Result<Self, Self::Error> {
        match c {
            ' ' => Ok(Self::Space),
            '\t' => Ok(Self::Tab),
            '\n' => Ok(Self::Newline),
            '\r' => Ok(Self::CarriageReturn),
            _ => Err("invalid whitespace"),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Op {
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
}

impl Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Equal => write!(f, "=="),
            Self::NotEqual => write!(f, "!="),
            Self::GreaterThanEqual => write!(f, ">="),
            Self::LessThanEqual => write!(f, "<="),
            Self::GreaterThan => write!(f, ">"),
            Self::LessThan => write!(f, "<"),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Keyword {
    Important,
    Infinity,
    NaN,
    Auto,
    Inherit,
    Initial,
    Unset,
    True,
    False,
    Not,
    And,
    Or,
    Null,
    In,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Important => write!(f, "!important"),
            Self::Infinity => write!(f, "Infinity"),
            Self::NaN => write!(f, "NaN"),
            Self::Auto => write!(f, "auto"),
            Self::Inherit => write!(f, "inherit"),
            Self::Initial => write!(f, "initial"),
            Self::Unset => write!(f, "unset"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Not => write!(f, "not"),
            Self::And => write!(f, "and"),
            Self::Or => write!(f, "or"),
            Self::Null => write!(f, "null"),
            Self::In => write!(f, "in"),
        }
    }
}

impl Into<&'static str> for Keyword {
    fn into(self) -> &'static str {
        match self {
            Self::Important => "!important",
            Self::Infinity => "Infinity",
            Self::NaN => "NaN",
            Self::Auto => "auto",
            Self::Inherit => "inherit",
            Self::Initial => "initial",
            Self::Unset => "unset",
            Self::True => "true",
            Self::False => "false",
            Self::Not => "not",
            Self::And => "and",
            Self::Or => "or",
            Self::Null => "null",
            Self::In => "in",
        }
    }
}

impl TryFrom<&str> for Keyword {
    type Error = &'static str;

    fn try_from(kw: &str) -> Result<Self, Self::Error> {
        // todo: case insensitive?
        match kw {
            "important" => Ok(Self::Important),
            "infinity" => Ok(Self::Infinity),
            "nan" => Ok(Self::NaN),
            "auto" => Ok(Self::Auto),
            "inherit" => Ok(Self::Inherit),
            "initial" => Ok(Self::Initial),
            "unset" => Ok(Self::Unset),
            "true" => Ok(Self::True),
            "false" => Ok(Self::False),
            "not" => Ok(Self::Not),
            "and" => Ok(Self::And),
            "or" => Ok(Self::Or),
            "null" => Ok(Self::Null),
            "in" => Ok(Self::In),
            _ => Err("invalid keyword"),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Pos {
    line: u32,
    column: u32,
}

impl Pos {
    pub const fn new() -> Self {
        Pos { line: 1, column: 1 }
    }

    pub const fn line(self) -> u32 {
        self.line
    }

    pub const fn column(self) -> u32 {
        self.column
    }

    pub fn newline(&mut self) {
        self.line += 1;
        self.column = 0;
    }

    pub fn next_char(&mut self) {
        self.column += 1;
    }

    pub fn chars(&mut self, num: u32) {
        self.column += num;
    }
}

impl Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line:{} col:{}", self.line, self.column)
    }
}

#[derive(Debug, Clone, Default)]
pub struct Scope {
    pub vars: HashMap<String, Vec<Token>>,
    pub mixins: HashMap<String, Mixin>,
}

impl Scope {
    #[must_use]
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            mixins: HashMap::new(),
        }
    }

    pub fn merge(&mut self, other: Scope) {
        self.vars.extend(other.vars);
        self.mixins.extend(other.mixins);
    }
}

#[derive(Debug)]
pub(crate) enum Printer {
    Error(Pos, String),
    Warn(Pos, String),
    Debug(Pos, String),
}
