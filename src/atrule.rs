use std::fmt::{self, Display};
use std::iter::Peekable;

use crate::common::{Pos, Scope, Symbol};
use crate::error::SassResult;
use crate::function::Function;
use crate::mixin::Mixin;
use crate::selector::Selector;
use crate::utils::{devour_whitespace, parse_interpolation};
use crate::{eat_expr, Expr, Stmt};
use crate::{RuleSet, Token, TokenKind};

#[derive(Debug, Clone)]
pub(crate) enum AtRule {
    Error(Pos, String),
    Warn(Pos, String),
    Debug(Pos, String),
    Mixin(String, Box<Mixin>),
    Function(String, Box<Function>),
    Return(Vec<Token>),
    // todo: emit only when non-ascii char is found
    Charset(Vec<Token>),
    Unknown(UnknownAtRule),
}

#[derive(Debug, Clone)]
pub(crate) struct UnknownAtRule {
    pub name: String,
    pub super_selector: Selector,
    pub params: String,
    pub body: Vec<Stmt>,
}

impl AtRule {
    pub fn from_tokens<I: Iterator<Item = Token>>(
        rule: &AtRuleKind,
        pos: Pos,
        toks: &mut Peekable<I>,
        scope: &Scope,
    ) -> SassResult<AtRule> {
        devour_whitespace(toks);
        Ok(match rule {
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
                let (name, mixin) = Mixin::decl_from_tokens(toks, scope)?;
                AtRule::Mixin(name, Box::new(mixin))
            }
            AtRuleKind::Function => {
                let (name, func) = Function::decl_from_tokens(toks, scope)?;
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
            AtRuleKind::Charset => AtRule::Charset(
                toks.take_while(|t| t.kind != TokenKind::Symbol(Symbol::SemiColon))
                    .collect(),
            ),
            AtRuleKind::Each => todo!("@each not yet implemented"),
            AtRuleKind::Extend => todo!("@extend not yet implemented"),
            AtRuleKind::If => todo!("@if not yet implemented"),
            AtRuleKind::Else => todo!("@else not yet implemented"),
            AtRuleKind::For => todo!("@for not yet implemented"),
            AtRuleKind::While => todo!("@while not yet implemented"),
            AtRuleKind::Keyframes => todo!("@keyframes not yet implemented"),
            AtRuleKind::Unknown(name) => {
                let mut params = String::new();
                while let Some(tok) = toks.next() {
                    match tok.kind {
                        TokenKind::Symbol(Symbol::OpenCurlyBrace) => break,
                        TokenKind::Interpolation => {
                            params.push_str(
                                &parse_interpolation(toks, scope)?
                                    .into_iter()
                                    .map(|x| x.kind.to_string())
                                    .collect::<String>(),
                            );
                            continue;
                        }
                        TokenKind::Variable(..) => params.push('$'),
                        _ => {}
                    }
                    params.push_str(&tok.kind.to_string());
                }

                let body = eat_unknown_atrule_body(toks, scope, &Selector::new())?;

                let u = UnknownAtRule {
                    name: name.clone(),
                    super_selector: Selector::new(),
                    params: params.trim().to_owned(),
                    body,
                };

                AtRule::Unknown(u)
            }
            _ => todo!("encountered unimplemented at rule"),
        })
    }
}

fn eat_unknown_atrule_body<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<Vec<Stmt>> {
    let mut stmts = Vec::new();
    while let Some(expr) = eat_expr(toks, scope, super_selector)? {
        match expr {
            Expr::Style(s) => stmts.push(Stmt::Style(s)),
            Expr::Styles(s) => stmts.extend(s.into_iter().map(Stmt::Style)),
            Expr::Include(s) => stmts.extend(s),
            Expr::MixinDecl(..) | Expr::FunctionDecl(..) | Expr::Debug(..) | Expr::Warn(..) => {
                todo!()
            }
            Expr::Selector(selector) => {
                let rules = eat_unknown_atrule_body(toks, scope, &super_selector.zip(&selector))?;
                stmts.push(Stmt::RuleSet(RuleSet {
                    super_selector: super_selector.clone(),
                    selector,
                    rules,
                }));
            }
            Expr::VariableDecl(..) => {
                todo!()
                // self.scope.insert_var(&name, *val);
            }
            Expr::MultilineComment(s) => stmts.push(Stmt::MultilineComment(s)),
        }
    }
    Ok(stmts)
}

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
    /// Tells the CSS engine that all its content must be considered
    /// prefixed with an XML namespace
    Namespace,
    /// A conditional group rule that will apply its content if the
    /// browser meets the criteria of the given condition
    Supports,
    /// Describes the aspect of layout changes that will be
    /// applied when printing the document
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
    /// A conditional group rule that will apply its content if the document in
    /// which the style sheet is applied meets the criteria of the given condition
    ///
    /// Deferred to Level 4 of CSS Spec
    Document,
    /// Defines specific counter styles that are not part of the predefined set of styles
    ///
    /// At the Candidate Recommendation stage
    CounterStyle,

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
            "namespace" => Self::Namespace,
            "supports" => Self::Supports,
            "page" => Self::Page,
            "fontface" => Self::FontFace,
            "keyframes" => Self::Keyframes,
            "fontfeaturevalues" => Self::FontFeatureValues,
            "swash" => Self::Swash,
            "ornaments" => Self::Ornaments,
            "annotation" => Self::Annotation,
            "stylistic" => Self::Stylistic,
            "styleset" => Self::Styleset,
            "charactervariant" => Self::CharacterVariant,
            "viewport" => Self::Viewport,
            "document" => Self::Document,
            "counterstyle" => Self::CounterStyle,
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
            Self::Namespace => write!(f, "@namespace"),
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
            Self::Unknown(s) => write!(f, "@{}", s),
        }
    }
}
