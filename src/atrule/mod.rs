use std::fmt::{self, Display};
use std::iter::Peekable;

use num_traits::cast::ToPrimitive;

use crate::common::Pos;
use crate::error::SassResult;
use crate::scope::Scope;
use crate::selector::Selector;
use crate::unit::Unit;
use crate::utils::{
    devour_whitespace, eat_ident, read_until_closing_curly_brace, read_until_open_curly_brace,
    read_until_semicolon_or_closing_curly_brace,
};
use crate::value::{Number, Value};
use crate::{Stmt, Token};

pub(crate) use function::Function;
pub(crate) use if_rule::If;
pub(crate) use mixin::{eat_include, Mixin};
use parse::eat_stmts;
use unknown::UnknownAtRule;

mod function;
mod if_rule;
mod mixin;
mod parse;
mod unknown;

#[derive(Debug, Clone)]
pub(crate) enum AtRule {
    Error(Pos, String),
    Warn(Pos, String),
    Debug(Pos, String),
    Mixin(String, Box<Mixin>),
    Function(String, Box<Function>),
    Return(Vec<Token>),
    Charset,
    Content,
    Unknown(UnknownAtRule),
    For(Vec<Stmt>),
    If(If),
}

impl AtRule {
    pub fn from_tokens<I: Iterator<Item = Token>>(
        rule: &AtRuleKind,
        pos: Pos,
        toks: &mut Peekable<I>,
        scope: &mut Scope,
        super_selector: &Selector,
    ) -> SassResult<AtRule> {
        devour_whitespace(toks);
        Ok(match rule {
            AtRuleKind::Error => {
                let message = toks
                    .take_while(|x| x.kind != ';')
                    .map(|x| x.kind.to_string())
                    .collect::<String>();
                AtRule::Error(pos, message)
            }
            AtRuleKind::Warn => {
                let message = toks
                    .take_while(|x| x.kind != ';')
                    .map(|x| x.kind.to_string())
                    .collect::<String>();
                devour_whitespace(toks);
                AtRule::Warn(pos, message)
            }
            AtRuleKind::Debug => {
                let message = toks
                    .by_ref()
                    .take_while(|x| x.kind != ';')
                    .map(|x| x.kind.to_string())
                    .collect::<String>();
                devour_whitespace(toks);
                AtRule::Debug(pos, message)
            }
            AtRuleKind::Mixin => {
                let (name, mixin) = Mixin::decl_from_tokens(toks, scope, super_selector)?;
                AtRule::Mixin(name, Box::new(mixin))
            }
            AtRuleKind::Function => {
                let (name, func) = Function::decl_from_tokens(toks, scope.clone(), super_selector)?;
                AtRule::Function(name, Box::new(func))
            }
            AtRuleKind::Return => {
                let v = read_until_semicolon_or_closing_curly_brace(toks);
                if toks.peek().unwrap().kind == ';' {
                    toks.next();
                }
                devour_whitespace(toks);
                AtRule::Return(v)
            }
            AtRuleKind::Use => todo!("@use not yet implemented"),
            AtRuleKind::Annotation => todo!("@annotation not yet implemented"),
            AtRuleKind::AtRoot => todo!("@at-root not yet implemented"),
            AtRuleKind::Charset => {
                read_until_semicolon_or_closing_curly_brace(toks);
                if toks.peek().unwrap().kind == ';' {
                    toks.next();
                }
                devour_whitespace(toks);
                AtRule::Charset
            }
            AtRuleKind::Each => todo!("@each not yet implemented"),
            AtRuleKind::Extend => todo!("@extend not yet implemented"),
            AtRuleKind::If => AtRule::If(If::from_tokens(toks)?),
            AtRuleKind::Else => todo!("@else not yet implemented"),
            AtRuleKind::For => {
                let mut stmts = Vec::new();
                devour_whitespace(toks);
                let var = match toks.next().ok_or("expected \"$\".")?.kind {
                    '$' => eat_ident(toks, scope, super_selector)?,
                    _ => return Err("expected \"$\".".into()),
                };
                devour_whitespace(toks);
                if toks.peek().is_none()
                    || eat_ident(toks, scope, super_selector)?.to_ascii_lowercase() != "from"
                {
                    return Err("Expected \"from\".".into());
                }
                devour_whitespace(toks);
                let mut from_toks = Vec::new();
                let mut through = 0;
                while let Some(tok) = toks.next() {
                    let mut these_toks = vec![tok];
                    match these_toks[0].kind.to_ascii_lowercase() {
                        't' => {
                            these_toks.push(toks.next().unwrap());
                            match these_toks[1].kind.to_ascii_lowercase() {
                                'h' => {
                                    let r = toks.next().unwrap();
                                    these_toks.push(r);
                                    if &r.kind != &'r' {
                                        from_toks.extend(these_toks);
                                        continue;
                                    }
                                    let o = toks.next().unwrap();
                                    these_toks.push(o);
                                    if o.kind != 'o' {
                                        from_toks.extend(these_toks);
                                        continue;
                                    }
                                    let u = toks.next().unwrap();
                                    these_toks.push(u);
                                    if u.kind != 'u' {
                                        from_toks.extend(these_toks);
                                        continue;
                                    }
                                    let g = toks.next().unwrap();
                                    these_toks.push(g);
                                    if g.kind != 'g' {
                                        from_toks.extend(these_toks);
                                        continue;
                                    }
                                    let h = toks.next().unwrap();
                                    these_toks.push(h);
                                    if h.kind != 'h' {
                                        from_toks.extend(these_toks);
                                        continue;
                                    }
                                    let peek = toks.peek().unwrap().kind;
                                    if peek.is_alphanumeric() || peek == '\\' {
                                        from_toks.extend(these_toks);
                                        continue;
                                    }
                                    through = 1;
                                    break;
                                }
                                'o' => {
                                    if toks.peek().unwrap().kind.is_whitespace() {
                                        break;
                                    } else {
                                        from_toks.extend(these_toks);
                                    }
                                }
                                _ => {
                                    from_toks.extend(these_toks);
                                }
                            }
                        }
                        '{' => {
                            return Err("Expected \"to\" or \"through\".".into());
                        }
                        _ => from_toks.extend(these_toks),
                    }
                }
                let from = match Value::from_tokens(
                    &mut from_toks.into_iter().peekable(),
                    scope,
                    super_selector,
                )? {
                    Value::Dimension(n, _) => match n.to_integer().to_usize() {
                        Some(v) => v,
                        None => return Err(format!("{} is not a int.", n).into()),
                    },
                    v => return Err(format!("{} is not an integer.", v).into()),
                };
                devour_whitespace(toks);
                let to_toks = read_until_open_curly_brace(toks);
                toks.next();
                let to = match Value::from_tokens(
                    &mut to_toks.into_iter().peekable(),
                    scope,
                    super_selector,
                )? {
                    Value::Dimension(n, _) => match n.to_integer().to_usize() {
                        Some(v) => v,
                        None => return Err(format!("{} is not a int.", n).into()),
                    },
                    v => return Err(format!("{} is not an integer.", v).into()),
                };
                let body = read_until_closing_curly_brace(toks);
                // body.push(toks.next().unwrap());
                toks.next();
                // dbg!(&body);

                devour_whitespace(toks);

                let (mut x, mut y);
                let iter: &mut dyn std::iter::Iterator<Item = usize> = if from < to {
                    x = from..(to + through);
                    &mut x
                } else {
                    y = ((to - through)..(from + 1)).skip(1).rev();
                    &mut y
                };

                for i in iter {
                    scope.insert_var(&var, Value::Dimension(Number::from(i), Unit::None))?;
                    stmts.extend(eat_stmts(
                        &mut body.clone().into_iter().peekable(),
                        scope,
                        super_selector,
                    )?);
                }

                AtRule::For(stmts)
            }
            AtRuleKind::While => todo!("@while not yet implemented"),
            AtRuleKind::Keyframes => todo!("@keyframes not yet implemented"),
            AtRuleKind::Unknown(name) => AtRule::Unknown(UnknownAtRule::from_tokens(
                toks,
                name,
                scope,
                super_selector,
            )?),
            AtRuleKind::Content => AtRule::Content,
            _ => todo!("encountered unimplemented at rule"),
        })
    }
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
            Self::Content => write!(f, "@content"),
            Self::Unknown(s) => write!(f, "@{}", s),
        }
    }
}
