use std::convert::TryFrom;
use std::fmt::{self, Display};
use std::iter::Peekable;

use crate::common::{Pos, Scope, Symbol};
use crate::function::Function;
use crate::mixin::Mixin;
use crate::utils::devour_whitespace;
use crate::{Token, TokenKind};

#[derive(Debug, Clone)]
pub(crate) enum AtRule {
    Error(Pos, String),
    Warn(Pos, String),
    Debug(Pos, String),
    Mixin(String, Box<Mixin>),
    Function(String, Box<Function>),
    Return(Vec<Token>),
}

impl AtRule {
    pub fn from_tokens<I: Iterator<Item = Token>>(
        rule: &AtRuleKind,
        pos: Pos,
        toks: &mut Peekable<I>,
        scope: &Scope,
    ) -> AtRule {
        devour_whitespace(toks);
        match rule {
            AtRuleKind::Error => {
                let message = toks
                    .take_while(|x| x.kind != TokenKind::Symbol(Symbol::SemiColon))
                    .map(|x| x.kind.to_string())
                    .collect::<String>();
                AtRule::Error(pos, message)
            }
            AtRuleKind::Warn => {
                let message = toks
                    .take_while(|x| x.kind != TokenKind::Symbol(Symbol::SemiColon))
                    .map(|x| x.kind.to_string())
                    .collect::<String>();
                devour_whitespace(toks);
                AtRule::Warn(pos, message)
            }
            AtRuleKind::Debug => {
                let message = toks
                    .by_ref()
                    .take_while(|x| x.kind != TokenKind::Symbol(Symbol::SemiColon))
                    .map(|x| x.kind.to_string())
                    .collect::<String>();
                devour_whitespace(toks);
                AtRule::Debug(pos, message)
            }
            AtRuleKind::Mixin => {
                let (name, mixin) = match Mixin::decl_from_tokens(toks, scope) {
                    Ok(m) => m,
                    Err(e) => return AtRule::Error(e.0, e.1),
                };
                AtRule::Mixin(name, Box::new(mixin))
            }
            AtRuleKind::Function => {
                let (name, func) = match Function::decl_from_tokens(toks, scope) {
                    Ok(m) => m,
                    Err(e) => return AtRule::Error(e.0, e.1),
                };
                AtRule::Function(name, Box::new(func))
            }
            AtRuleKind::Return => AtRule::Return(
                // todo: return may not end in semicolon
                toks.take_while(|t| t.kind != TokenKind::Symbol(Symbol::SemiColon))
                    .collect(),
            ),
            AtRuleKind::Use => todo!("@use not yet implemented"),
            AtRuleKind::Annotation => todo!("@annotation not yet implemented"),
            AtRuleKind::AtRoot => todo!("@at-root not yet implemented"),
            AtRuleKind::Charset => todo!("@charset not yet implemented"),
            AtRuleKind::Each => todo!("@each not yet implemented"),
            AtRuleKind::Extend => todo!("@extend not yet implemented"),
            AtRuleKind::If => todo!("@if not yet implemented"),
            AtRuleKind::Else => todo!("@else not yet implemented"),
            AtRuleKind::For => todo!("@for not yet implemented"),
            AtRuleKind::While => todo!("@while not yet implemented"),
            AtRuleKind::Media => todo!("@media not yet implemented"),
            AtRuleKind::Keyframes => todo!("@keyframes not yet implemented"),
            _ => todo!("encountered unimplemented at rule"),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AtRuleKind {
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

impl TryFrom<&str> for AtRuleKind {
    type Error = &'static str;

    fn try_from(c: &str) -> Result<Self, &'static str> {
        match c {
            "use" => Ok(Self::Use),
            "forward" => Ok(Self::Forward),
            "import" => Ok(Self::Import),
            "mixin" => Ok(Self::Mixin),
            "include" => Ok(Self::Include),
            "function" => Ok(Self::Function),
            "return" => Ok(Self::Return),
            "extend" => Ok(Self::Extend),
            "atroot" => Ok(Self::AtRoot),
            "error" => Ok(Self::Error),
            "warn" => Ok(Self::Warn),
            "debug" => Ok(Self::Debug),
            "if" => Ok(Self::If),
            "else" => Ok(Self::Else),
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
            Self::AtRoot => write!(f, "@atroot"),
            Self::Error => write!(f, "@error"),
            Self::Warn => write!(f, "@warn"),
            Self::Debug => write!(f, "@debug"),
            Self::If => write!(f, "@if"),
            Self::Else => write!(f, "@else"),
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
