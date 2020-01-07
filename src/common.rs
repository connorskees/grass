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
    GreaterThanEqual,
    LessThanEqual,
}

impl Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Equal => write!(f, "=="),
            Op::NotEqual => write!(f, "!="),
            Op::GreaterThanEqual => write!(f, ">="),
            Op::LessThanEqual => write!(f, "<="),
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

#[derive(Debug)]
pub enum Color {
    AliceBlue,            // = 0xF0F8FF,
    AntiqueWhite,         // = 0xFAEBD7,
    Aqua,                 // = 0x00FFFF,
    Aquamarine,           // = 0x7FFFD4,
    Azure,                // = 0xF0FFFF,
    Beige,                // = 0xF5F5DC,
    Bisque,               // = 0xFFE4C4,
    Black,                // = 0x000000,
    BlanchedAlmond,       // = 0xFFEBCD,
    Blue,                 // = 0x0000FF,
    BlueViolet,           // = 0x8A2BE2,
    Brown,                // = 0xA52A2A,
    BurlyWood,            // = 0xDEB887,
    CadetBlue,            // = 0x5F9EA0,
    Chartreuse,           // = 0x7FFF00,
    Chocolate,            // = 0xD2691E,
    Coral,                // = 0xFF7F50,
    CornflowerBlue,       // = 0x6495ED,
    Cornsilk,             // = 0xFFF8DC,
    Crimson,              // = 0xDC143C,
    Cyan,                 //0x00FFFF
    DarkBlue,             // = 0x00008B,
    DarkCyan,             // = 0x008B8B,
    DarkGoldenRod,        // = 0xB8860B,
    DarkGray,             // = 0xA9A9A9,
    DarkGrey,             //0xA9A9A9
    DarkGreen,            // = 0x006400,
    DarkKhaki,            // = 0xBDB76B,
    DarkMagenta,          // = 0x8B008B,
    DarkOliveGreen,       // = 0x556B2F,
    DarkOrange,           // = 0xFF8C00,
    DarkOrchid,           // = 0x9932CC,
    DarkRed,              // = 0x8B0000,
    DarkSalmon,           // = 0xE9967A,
    DarkSeaGreen,         // = 0x8FBC8F,
    DarkSlateBlue,        // = 0x483D8B,
    DarkSlateGray,        // = 0x2F4F4F,
    DarkSlateGrey,        //0x2F4F4F
    DarkTurquoise,        // = 0x00CED1,
    DarkViolet,           // = 0x9400D3,
    DeepPink,             // = 0xFF1493,
    DeepSkyBlue,          // = 0x00BFFF,
    DimGray,              // = 0x696969,
    DimGrey,              //0x696969
    DodgerBlue,           // = 0x1E90FF,
    FireBrick,            // = 0xB22222,
    FloralWhite,          // = 0xFFFAF0,
    ForestGreen,          // = 0x228B22,
    Fuchsia,              // = 0xFF00FF,
    Gainsboro,            // = 0xDCDCDC,
    GhostWhite,           // = 0xF8F8FF,
    Gold,                 // = 0xFFD700,
    GoldenRod,            // = 0xDAA520,
    Gray,                 // = 0x808080,
    Grey,                 //0x808080
    Green,                // = 0x008000,
    GreenYellow,          // = 0xADFF2F,
    HoneyDew,             // = 0xF0FFF0,
    HotPink,              // = 0xFF69B4,
    IndianRed,            // = 0xCD5C5C,
    Indigo,               // = 0x4B0082,
    Ivory,                // = 0xFFFFF0,
    Khaki,                // = 0xF0E68C,
    Lavender,             // = 0xE6E6FA,
    LavenderBlush,        // = 0xFFF0F5,
    LawnGreen,            // = 0x7CFC00,
    LemonChiffon,         // = 0xFFFACD,
    LightBlue,            // = 0xADD8E6,
    LightCoral,           // = 0xF08080,
    LightCyan,            // = 0xE0FFFF,
    LightGoldenRodYellow, // = 0xFAFAD2,
    LightGray,            // = 0xD3D3D3,
    LightGrey,            //0xD3D3D3
    LightGreen,           // = 0x90EE90,
    LightPink,            // = 0xFFB6C1,
    LightSalmon,          // = 0xFFA07A,
    LightSeaGreen,        // = 0x20B2AA,
    LightSkyBlue,         // = 0x87CEFA,
    LightSlateGray,       // = 0x778899,
    LightSlateGrey,       //0x778899
    LightSteelBlue,       // = 0xB0C4DE,
    LightYellow,          // = 0xFFFFE0,
    Lime,                 // = 0x00FF00,
    LimeGreen,            // = 0x32CD32,
    Linen,                // = 0xFAF0E6,
    Magenta,              //0xFF00FF
    Maroon,               // = 0x800000,
    MediumAquaMarine,     // = 0x66CDAA,
    MediumBlue,           // = 0x0000CD,
    MediumOrchid,         // = 0xBA55D3,
    MediumPurple,         // = 0x9370DB,
    MediumSeaGreen,       // = 0x3CB371,
    MediumSlateBlue,      // = 0x7B68EE,
    MediumSpringGreen,    // = 0x00FA9A,
    MediumTurquoise,      // = 0x48D1CC,
    MediumVioletRed,      // = 0xC71585,
    MidnightBlue,         // = 0x191970,
    MintCream,            // = 0xF5FFFA,
    MistyRose,            // = 0xFFE4E1,
    Moccasin,             // = 0xFFE4B5,
    NavajoWhite,          // = 0xFFDEAD,
    Navy,                 // = 0x000080,
    OldLace,              // = 0xFDF5E6,
    Olive,                // = 0x808000,
    OliveDrab,            // = 0x6B8E23,
    Orange,               // = 0xFFA500,
    OrangeRed,            // = 0xFF4500,
    Orchid,               // = 0xDA70D6,
    PaleGoldenRod,        // = 0xEEE8AA,
    PaleGreen,            // = 0x98FB98,
    PaleTurquoise,        // = 0xAFEEEE,
    PaleVioletRed,        // = 0xDB7093,
    PapayaWhip,           // = 0xFFEFD5,
    PeachPuff,            // = 0xFFDAB9,
    Peru,                 // = 0xCD853F,
    Pink,                 // = 0xFFC0CB,
    Plum,                 // = 0xDDA0DD,
    PowderBlue,           // = 0xB0E0E6,
    Purple,               // = 0x800080,
    RebeccaPurple,        // = 0x663399,
    Red,                  // = 0xFF0000,
    RosyBrown,            // = 0xBC8F8F,
    RoyalBlue,            // = 0x4169E1,
    SaddleBrown,          // = 0x8B4513,
    Salmon,               // = 0xFA8072,
    SandyBrown,           // = 0xF4A460,
    SeaGreen,             // = 0x2E8B57,
    SeaShell,             // = 0xFFF5EE,
    Sienna,               // = 0xA0522D,
    Silver,               // = 0xC0C0C0,
    SkyBlue,              // = 0x87CEEB,
    SlateBlue,            // = 0x6A5ACD,
    SlateGray,            // = 0x708090,
    SlateGrey,            //0x708090
    Snow,                 // = 0xFFFAFA,
    SpringGreen,          // = 0x00FF7F,
    SteelBlue,            // = 0x4682B4,
    Tan,                  // = 0xD2B48C,
    Teal,                 // = 0x008080,
    Thistle,              // = 0xD8BFD8,
    Tomato,               // = 0xFF6347,
    Turquoise,            // = 0x40E0D0,
    Violet,               // = 0xEE82EE,
    Wheat,                // = 0xF5DEB3,
    White,                // = 0xFFFFFF,
    WhiteSmoke,           // = 0xF5F5F5,
    Yellow,               // = 0xFFFF00,
    YellowGreen,          // = 0x9ACD32,
    Other(u32),
}

impl TryFrom<&str> for Color {
    type Error = &'static str;

    fn try_from(string: &str) -> Result<Self, Self::Error> {
        match string {
            "aliceblue" => Ok(Color::AliceBlue),
            "antiquewhite" => Ok(Color::AntiqueWhite),
            "aqua" => Ok(Color::Aqua),
            "aquamarine" => Ok(Color::Aquamarine),
            "azure" => Ok(Color::Azure),
            "beige" => Ok(Color::Beige),
            "bisque" => Ok(Color::Bisque),
            "black" => Ok(Color::Black),
            "blanchedalmond" => Ok(Color::BlanchedAlmond),
            "blue" => Ok(Color::Blue),
            "blueviolet" => Ok(Color::BlueViolet),
            "brown" => Ok(Color::Brown),
            "burlywood" => Ok(Color::BurlyWood),
            "cadetblue" => Ok(Color::CadetBlue),
            "chartreuse" => Ok(Color::Chartreuse),
            "chocolate" => Ok(Color::Chocolate),
            "coral" => Ok(Color::Coral),
            "cornflowerblue" => Ok(Color::CornflowerBlue),
            "cornsilk" => Ok(Color::Cornsilk),
            "crimson" => Ok(Color::Crimson),
            "cyan" => Ok(Color::Cyan),
            "darkblue" => Ok(Color::DarkBlue),
            "darkcyan" => Ok(Color::DarkCyan),
            "darkgoldenrod" => Ok(Color::DarkGoldenRod),
            "darkgray" => Ok(Color::DarkGray),
            "darkgrey" => Ok(Color::DarkGrey),
            "darkgreen" => Ok(Color::DarkGreen),
            "darkkhaki" => Ok(Color::DarkKhaki),
            "darkmagenta" => Ok(Color::DarkMagenta),
            "darkolivegreen" => Ok(Color::DarkOliveGreen),
            "darkorange" => Ok(Color::DarkOrange),
            "darkorchid" => Ok(Color::DarkOrchid),
            "darkred" => Ok(Color::DarkRed),
            "darksalmon" => Ok(Color::DarkSalmon),
            "darkseagreen" => Ok(Color::DarkSeaGreen),
            "darkslateblue" => Ok(Color::DarkSlateBlue),
            "darkslategray" => Ok(Color::DarkSlateGray),
            "darkslategrey" => Ok(Color::DarkSlateGrey),
            "darkturquoise" => Ok(Color::DarkTurquoise),
            "darkviolet" => Ok(Color::DarkViolet),
            "deeppink" => Ok(Color::DeepPink),
            "deepskyblue" => Ok(Color::DeepSkyBlue),
            "dimgray" => Ok(Color::DimGray),
            "dimgrey" => Ok(Color::DimGrey),
            "dodgerblue" => Ok(Color::DodgerBlue),
            "firebrick" => Ok(Color::FireBrick),
            "floralwhite" => Ok(Color::FloralWhite),
            "forestgreen" => Ok(Color::ForestGreen),
            "fuchsia" => Ok(Color::Fuchsia),
            "gainsboro" => Ok(Color::Gainsboro),
            "ghostwhite" => Ok(Color::GhostWhite),
            "gold" => Ok(Color::Gold),
            "goldenrod" => Ok(Color::GoldenRod),
            "gray" => Ok(Color::Gray),
            "grey" => Ok(Color::Grey),
            "green" => Ok(Color::Green),
            "greenyellow" => Ok(Color::GreenYellow),
            "honeydew" => Ok(Color::HoneyDew),
            "hotpink" => Ok(Color::HotPink),
            "indianred" => Ok(Color::IndianRed),
            "indigo" => Ok(Color::Indigo),
            "ivory" => Ok(Color::Ivory),
            "khaki" => Ok(Color::Khaki),
            "lavender" => Ok(Color::Lavender),
            "lavenderblush" => Ok(Color::LavenderBlush),
            "lawngreen" => Ok(Color::LawnGreen),
            "lemonchiffon" => Ok(Color::LemonChiffon),
            "lightblue" => Ok(Color::LightBlue),
            "lightcoral" => Ok(Color::LightCoral),
            "lightcyan" => Ok(Color::LightCyan),
            "lightgoldenrodyellow" => Ok(Color::LightGoldenRodYellow),
            "lightgray" => Ok(Color::LightGray),
            "lightgrey" => Ok(Color::LightGrey),
            "lightgreen" => Ok(Color::LightGreen),
            "lightpink" => Ok(Color::LightPink),
            "lightsalmon" => Ok(Color::LightSalmon),
            "lightseagreen" => Ok(Color::LightSeaGreen),
            "lightskyblue" => Ok(Color::LightSkyBlue),
            "lightslategray" => Ok(Color::LightSlateGray),
            "lightslategrey" => Ok(Color::LightSlateGrey),
            "lightsteelblue" => Ok(Color::LightSteelBlue),
            "lightyellow" => Ok(Color::LightYellow),
            "lime" => Ok(Color::Lime),
            "limegreen" => Ok(Color::LimeGreen),
            "linen" => Ok(Color::Linen),
            "magenta" => Ok(Color::Magenta),
            "maroon" => Ok(Color::Maroon),
            "mediumaquamarine" => Ok(Color::MediumAquaMarine),
            "mediumblue" => Ok(Color::MediumBlue),
            "mediumorchid" => Ok(Color::MediumOrchid),
            "mediumpurple" => Ok(Color::MediumPurple),
            "mediumseagreen" => Ok(Color::MediumSeaGreen),
            "mediumslateblue" => Ok(Color::MediumSlateBlue),
            "mediumspringgreen" => Ok(Color::MediumSpringGreen),
            "mediumturquoise" => Ok(Color::MediumTurquoise),
            "mediumvioletred" => Ok(Color::MediumVioletRed),
            "midnightblue" => Ok(Color::MidnightBlue),
            "mintcream" => Ok(Color::MintCream),
            "mistyrose" => Ok(Color::MistyRose),
            "moccasin" => Ok(Color::Moccasin),
            "navajowhite" => Ok(Color::NavajoWhite),
            "navy" => Ok(Color::Navy),
            "oldlace" => Ok(Color::OldLace),
            "olive" => Ok(Color::Olive),
            "olivedrab" => Ok(Color::OliveDrab),
            "orange" => Ok(Color::Orange),
            "orangered" => Ok(Color::OrangeRed),
            "orchid" => Ok(Color::Orchid),
            "palegoldenrod" => Ok(Color::PaleGoldenRod),
            "palegreen" => Ok(Color::PaleGreen),
            "paleturquoise" => Ok(Color::PaleTurquoise),
            "palevioletred" => Ok(Color::PaleVioletRed),
            "papayawhip" => Ok(Color::PapayaWhip),
            "peachpuff" => Ok(Color::PeachPuff),
            "peru" => Ok(Color::Peru),
            "pink" => Ok(Color::Pink),
            "plum" => Ok(Color::Plum),
            "powderblue" => Ok(Color::PowderBlue),
            "purple" => Ok(Color::Purple),
            "rebeccapurple" => Ok(Color::RebeccaPurple),
            "red" => Ok(Color::Red),
            "rosybrown" => Ok(Color::RosyBrown),
            "royalblue" => Ok(Color::RoyalBlue),
            "saddlebrown" => Ok(Color::SaddleBrown),
            "salmon" => Ok(Color::Salmon),
            "sandybrown" => Ok(Color::SandyBrown),
            "seagreen" => Ok(Color::SeaGreen),
            "seashell" => Ok(Color::SeaShell),
            "sienna" => Ok(Color::Sienna),
            "silver" => Ok(Color::Silver),
            "skyblue" => Ok(Color::SkyBlue),
            "slateblue" => Ok(Color::SlateBlue),
            "slategray" => Ok(Color::SlateGray),
            "slategrey" => Ok(Color::SlateGrey),
            "snow" => Ok(Color::Snow),
            "springgreen" => Ok(Color::SpringGreen),
            "steelblue" => Ok(Color::SteelBlue),
            "tan" => Ok(Color::Tan),
            "teal" => Ok(Color::Teal),
            "thistle" => Ok(Color::Thistle),
            "tomato" => Ok(Color::Tomato),
            "turquoise" => Ok(Color::Turquoise),
            "violet" => Ok(Color::Violet),
            "wheat" => Ok(Color::Wheat),
            "white" => Ok(Color::White),
            "whitesmoke" => Ok(Color::WhiteSmoke),
            "yellow" => Ok(Color::Yellow),
            "yellowgreen" => Ok(Color::YellowGreen),
            _ => Err("invalid color"),
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
