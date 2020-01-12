use std::convert::TryFrom;
use std::fmt::{self, Display};

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
    OpenBrace,
    /// }
    CloseBrace,
    /// [
    OpenBracket,
    /// ]
    CloseBracket,
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
            Symbol::Period => write!(f, "."),
            Symbol::Hash => write!(f, "#"),
            Symbol::At => write!(f, "@"),
            Symbol::Dollar => write!(f, "$"),
            Symbol::OpenParen => write!(f, "("),
            Symbol::CloseParen => write!(f, "),"),
            Symbol::OpenBrace => write!(f, "{{"),
            Symbol::CloseBrace => write!(f, "}}"),
            Symbol::OpenBracket => write!(f, "["),
            Symbol::CloseBracket => write!(f, "]"),
            Symbol::Comma => write!(f, ","),
            Symbol::Plus => write!(f, "+"),
            Symbol::Minus => write!(f, "-"),
            Symbol::Mul => write!(f, "*"),
            Symbol::Div => write!(f, "/"),
            Symbol::Colon => write!(f, ":"),
            Symbol::SemiColon => write!(f, ";"),
            Symbol::Tilde => write!(f, "~"),
            Symbol::Gt => write!(f, ">"),
            Symbol::Lt => write!(f, "<"),
            Symbol::Xor => write!(f, "^"),
            Symbol::Equal => write!(f, "="),
            Symbol::BitOr => write!(f, "|"),
            Symbol::BitAnd => write!(f, "&"),
            Symbol::Percent => write!(f, "%"),
            Symbol::DoubleQuote => write!(f, "\""),
            Symbol::SingleQuote => write!(f, "'"),
        }
    }
}

impl TryFrom<char> for Symbol {
    type Error = &'static str;

    fn try_from(c: char) -> Result<Self, Self::Error> {
        match c {
            '.' => Ok(Symbol::Period),
            '#' => Ok(Symbol::Hash),
            '@' => Ok(Symbol::At),
            '$' => Ok(Symbol::Dollar),
            '(' => Ok(Symbol::OpenParen),
            ')' => Ok(Symbol::CloseParen),
            '{' => Ok(Symbol::OpenBrace),
            '}' => Ok(Symbol::CloseBrace),
            '[' => Ok(Symbol::OpenBracket),
            ']' => Ok(Symbol::CloseBracket),
            ',' => Ok(Symbol::Comma),
            '+' => Ok(Symbol::Plus),
            '-' => Ok(Symbol::Minus),
            '*' => Ok(Symbol::Mul),
            '/' => Ok(Symbol::Div),
            ':' => Ok(Symbol::Colon),
            ';' => Ok(Symbol::SemiColon),
            '~' => Ok(Symbol::Tilde),
            '>' => Ok(Symbol::Gt),
            '<' => Ok(Symbol::Lt),
            '^' => Ok(Symbol::Xor),
            '=' => Ok(Symbol::Equal),
            '|' => Ok(Symbol::BitOr),
            '&' => Ok(Symbol::BitAnd),
            '%' => Ok(Symbol::Percent),
            '"' => Ok(Symbol::DoubleQuote),
            '\'' => Ok(Symbol::SingleQuote),
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
            "use" => Ok(AtRule::Use),
            "forward" => Ok(AtRule::Forward),
            "import" => Ok(AtRule::Import),
            "mixin" => Ok(AtRule::Mixin),
            "include" => Ok(AtRule::Include),
            "function" => Ok(AtRule::Function),
            "extend" => Ok(AtRule::Extend),
            "atroot" => Ok(AtRule::AtRoot),
            "error" => Ok(AtRule::Error),
            "warn" => Ok(AtRule::Warn),
            "debug" => Ok(AtRule::Debug),
            "if" => Ok(AtRule::If),
            "each" => Ok(AtRule::Each),
            "for" => Ok(AtRule::For),
            "while" => Ok(AtRule::While),
            "charset" => Ok(AtRule::Charset),
            "namespace" => Ok(AtRule::Namespace),
            "media" => Ok(AtRule::Media),
            "supports" => Ok(AtRule::Supports),
            "page" => Ok(AtRule::Page),
            "fontface" => Ok(AtRule::FontFace),
            "keyframes" => Ok(AtRule::Keyframes),
            "fontfeaturevalues" => Ok(AtRule::FontFeatureValues),
            "swash" => Ok(AtRule::Swash),
            "ornaments" => Ok(AtRule::Ornaments),
            "annotation" => Ok(AtRule::Annotation),
            "stylistic" => Ok(AtRule::Stylistic),
            "styleset" => Ok(AtRule::Styleset),
            "charactervariant" => Ok(AtRule::CharacterVariant),
            "viewport" => Ok(AtRule::Viewport),
            "document" => Ok(AtRule::Document),
            "counterstyle" => Ok(AtRule::CounterStyle),
            _ => Err("invalid at rule"),
        }
    }
}

impl Display for AtRule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AtRule::Use => write!(f, "@use"),
            AtRule::Forward => write!(f, "@forward"),
            AtRule::Import => write!(f, "@import"),
            AtRule::Mixin => write!(f, "@mixin"),
            AtRule::Include => write!(f, "@include"),
            AtRule::Function => write!(f, "@function"),
            AtRule::Extend => write!(f, "@extend"),
            AtRule::AtRoot => write!(f, "@atroot"),
            AtRule::Error => write!(f, "@error"),
            AtRule::Warn => write!(f, "@warn"),
            AtRule::Debug => write!(f, "@debug"),
            AtRule::If => write!(f, "@if"),
            AtRule::Each => write!(f, "@each"),
            AtRule::For => write!(f, "@for"),
            AtRule::While => write!(f, "@while"),
            AtRule::Charset => write!(f, "@charset"),
            AtRule::Namespace => write!(f, "@namespace"),
            AtRule::Media => write!(f, "@media"),
            AtRule::Supports => write!(f, "@supports"),
            AtRule::Page => write!(f, "@page"),
            AtRule::FontFace => write!(f, "@fontface"),
            AtRule::Keyframes => write!(f, "@keyframes"),
            AtRule::FontFeatureValues => write!(f, "@fontfeaturevalues"),
            AtRule::Swash => write!(f, "@swash"),
            AtRule::Ornaments => write!(f, "@ornaments"),
            AtRule::Annotation => write!(f, "@annotation"),
            AtRule::Stylistic => write!(f, "@stylistic"),
            AtRule::Styleset => write!(f, "@styleset"),
            AtRule::CharacterVariant => write!(f, "@charactervariant"),
            AtRule::Viewport => write!(f, "@viewport"),
            AtRule::Document => write!(f, "@document"),
            AtRule::CounterStyle => write!(f, "@counterstyle"),
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
            Whitespace::Space => write!(f, " "),
            Whitespace::Tab => write!(f, "\t"),
            Whitespace::Newline => writeln!(f),
            Whitespace::CarriageReturn => write!(f, "\r"),
        }
    }
}

impl TryFrom<char> for Whitespace {
    type Error = &'static str;

    fn try_from(c: char) -> Result<Self, Self::Error> {
        match c {
            ' ' => Ok(Whitespace::Space),
            '\t' => Ok(Whitespace::Tab),
            '\n' => Ok(Whitespace::Newline),
            '\r' => Ok(Whitespace::CarriageReturn),
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
            Op::Equal => write!(f, "=="),
            Op::NotEqual => write!(f, "!="),
            Op::GreaterThanEqual => write!(f, ">="),
            Op::LessThanEqual => write!(f, "<="),
            Op::GreaterThan => write!(f, ">"),
            Op::LessThan => write!(f, "<"),
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
            Keyword::Important => write!(f, "!important"),
            Keyword::Infinity => write!(f, "Infinity"),
            Keyword::NaN => write!(f, "NaN"),
            Keyword::Auto => write!(f, "auto"),
            Keyword::Inherit => write!(f, "inherit"),
            Keyword::Initial => write!(f, "initial"),
            Keyword::Unset => write!(f, "unset"),
            Keyword::True => write!(f, "true"),
            Keyword::False => write!(f, "false"),
            Keyword::Not => write!(f, "not"),
            Keyword::And => write!(f, "and"),
            Keyword::Or => write!(f, "or"),
            Keyword::Null => write!(f, "null"),
            Keyword::In => write!(f, "in"),
        }
    }
}

impl Into<&'static str> for Keyword {
    fn into(self) -> &'static str {
        match self {
            Keyword::Important => "!important",
            Keyword::Infinity => "Infinity",
            Keyword::NaN => "NaN",
            Keyword::Auto => "auto",
            Keyword::Inherit => "inherit",
            Keyword::Initial => "initial",
            Keyword::Unset => "unset",
            Keyword::True => "true",
            Keyword::False => "false",
            Keyword::Not => "not",
            Keyword::And => "and",
            Keyword::Or => "or",
            Keyword::Null => "null",
            Keyword::In => "in",
        }
    }
}

impl TryFrom<&str> for Keyword {
    type Error = &'static str;

    fn try_from(kw: &str) -> Result<Self, Self::Error> {
        // todo: case insensitive?
        match kw {
            "important" => Ok(Keyword::Important),
            "infinity" => Ok(Keyword::Infinity),
            "nan" => Ok(Keyword::NaN),
            "auto" => Ok(Keyword::Auto),
            "inherit" => Ok(Keyword::Inherit),
            "initial" => Ok(Keyword::Initial),
            "unset" => Ok(Keyword::Unset),
            "true" => Ok(Keyword::True),
            "false" => Ok(Keyword::False),
            "not" => Ok(Keyword::Not),
            "and" => Ok(Keyword::And),
            "or" => Ok(Keyword::Or),
            "null" => Ok(Keyword::Null),
            "in" => Ok(Keyword::In),
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
