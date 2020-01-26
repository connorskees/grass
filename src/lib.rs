#![warn(
    clippy::all,
    clippy::restriction,
    clippy::pedantic,
    clippy::nursery,
    clippy::cargo
)]
#![deny(missing_debug_implementations)]
#![allow(
    // explicit return makes some things look ugly
    clippy::implicit_return,
    // Self { .. } is less explicit than Foo { .. }
    clippy::use_self,
    // this is way too pedantic -- some things don't need docs!
    clippy::missing_docs_in_private_items,
    // this crate is too new to deny todo!()
    clippy::todo,
    // unreachable!() has many valid use cases
    clippy::unreachable,
    // _ => {} has many valid use cases
    clippy::wildcard_enum_match_arm,
    // .expect() has many valid use cases, like when we know a value is `Some(..)`
    clippy::option_expect_used,
    // for now, panic() is an acceptable solution
    clippy::panic,
    // for now, some functions require a lot of lines
    // future refactoring should make functions small and make
    // this lint less annoying
    clippy::too_many_lines,
    // this is too pedantic -- we are allowed to add numbers!
    clippy::integer_arithmetic,
    // this is too pedantic for now -- the library is changing too quickly for
    // good docs to be written
    clippy::missing_errors_doc,
    // this incorrectly results in errors for types that derive `Debug`
    // https://github.com/rust-lang/rust-clippy/issues/4980
    clippy::let_underscore_must_use,
    // this is too pedantic -- it results in some names being less explicit
    // than they should
    clippy::module_name_repetitions
)]
#![cfg_attr(feature = "nightly", feature(track_caller))]
// todo! handle erroring on styles at the toplevel
use std::fmt::{self, Display};
use std::fs;
use std::io::Write;
use std::iter::{Iterator, Peekable};
use std::path::Path;

use crate::atrule::{AtRule, AtRuleKind};
use crate::common::{Keyword, Op, Pos, Scope, Symbol, Whitespace};
use crate::css::Css;
use crate::error::SassError;
use crate::format::PrettyPrinter;
use crate::function::Function;
use crate::imports::import;
use crate::lexer::Lexer;
use crate::mixin::{eat_include, Mixin};
use crate::selector::{Attribute, Selector};
use crate::style::Style;
use crate::utils::{devour_whitespace, eat_variable_value, IsComment, IsWhitespace};
use crate::value::Value;

mod args;
mod atrule;
mod builtin;
mod color;
mod common;
mod css;
mod error;
mod format;
mod function;
mod imports;
mod lexer;
mod mixin;
mod selector;
mod style;
mod units;
mod utils;
mod value;

pub type SassResult<T> = Result<T, SassError>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Token {
    pos: Pos,
    pub kind: TokenKind,
}

impl IsWhitespace for Token {
    fn is_whitespace(&self) -> bool {
        if let TokenKind::Whitespace(_) = self.kind {
            return true;
        }
        false
    }
}

impl IsWhitespace for &Token {
    fn is_whitespace(&self) -> bool {
        if let TokenKind::Whitespace(_) = self.kind {
            return true;
        }
        false
    }
}

impl IsComment for Token {
    fn is_comment(&self) -> bool {
        if let TokenKind::MultilineComment(_) = self.kind {
            return true;
        }
        false
    }
}

impl IsComment for &Token {
    fn is_comment(&self) -> bool {
        if let TokenKind::MultilineComment(_) = self.kind {
            return true;
        }
        false
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum TokenKind {
    Ident(String),
    Symbol(Symbol),
    AtRule(AtRuleKind),
    Keyword(Keyword),
    Number(String),
    Whitespace(Whitespace),
    Variable(String),
    Attribute(Attribute),
    Op(Op),
    MultilineComment(String),
    Interpolation,
}

impl Display for TokenKind {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Ident(s) | TokenKind::Number(s) => write!(f, "{}", s),
            TokenKind::Symbol(s) => write!(f, "{}", s),
            TokenKind::AtRule(s) => write!(f, "{}", s),
            TokenKind::Op(s) => write!(f, "{}", s),
            TokenKind::Whitespace(s) => write!(f, "{}", s),
            TokenKind::Attribute(s) => write!(f, "{}", s),
            TokenKind::Keyword(kw) => write!(f, "{}", kw),
            TokenKind::MultilineComment(s) => write!(f, "/*{}*/", s),
            TokenKind::Variable(s) => write!(f, "${}", s),
            TokenKind::Interpolation => {
                panic!("we don't want to format TokenKind::Interpolation using Display")
            }
        }
    }
}

/// Represents a parsed SASS stylesheet with nesting
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StyleSheet(Vec<Stmt>);

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum Stmt {
    /// A [`Style`](/grass/style/struct.Style)
    Style(Style),
    /// A  [`RuleSet`](/grass/struct.RuleSet.html)
    RuleSet(RuleSet),
    /// A multiline comment: `/* foo bar */`
    MultilineComment(String),
}

/// Represents a single rule set. Rule sets can contain other rule sets
///
/// ```scss
/// a {
///   color: blue;
///   b {
///     color: red;
///   }
/// }
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct RuleSet {
    selector: Selector,
    rules: Vec<Stmt>,
    // potential optimization: we don't *need* to own the selector
    super_selector: Selector,
}

/// An intermediate representation of what are essentially single lines
/// todo! rename this
#[derive(Clone, Debug)]
enum Expr {
    /// A style: `color: red`
    Style(Style),
    /// A collection of styles, from a mixin or function
    // Styles(Vec<Style>),
    /// A full selector `a > h1`
    Selector(Selector),
    /// A variable declaration `$var: 1px`
    VariableDecl(String, Value),
    /// A mixin declaration `@mixin foo {}`
    MixinDecl(String, Mixin),
    FunctionDecl(String, Function),
    /// An include statement `@include foo;`
    Include(Vec<Stmt>),
    /// A multiline comment: `/* foobar */`
    MultilineComment(String),
    Debug(Pos, String),
    Warn(Pos, String),
    // /// Function call: `calc(10vw - 1px)`
    // FuncCall(String, Vec<Token>),
}

impl StyleSheet {
    #[inline]
    pub fn new(input: &str) -> SassResult<StyleSheet> {
        Ok(StyleSheet(
            StyleSheetParser {
                global_scope: Scope::new(),
                lexer: Lexer::new(input).peekable(),
                rules: Vec::new(),
                scope: 0,
                file: String::from("stdin"),
            }
            .parse_toplevel()?
            .0,
        ))
    }

    #[inline]
    pub fn from_path<P: AsRef<Path> + Into<String>>(p: P) -> SassResult<StyleSheet> {
        Ok(StyleSheet(
            StyleSheetParser {
                global_scope: Scope::new(),
                lexer: Lexer::new(&String::from_utf8(fs::read(p.as_ref())?)?).peekable(),
                rules: Vec::new(),
                scope: 0,
                file: p.into(),
            }
            .parse_toplevel()?
            .0,
        ))
    }

    pub(crate) fn export_from_path<P: AsRef<Path> + Into<String>>(
        p: P,
    ) -> SassResult<(Vec<Stmt>, Scope)> {
        Ok(StyleSheetParser {
            global_scope: Scope::new(),
            lexer: Lexer::new(&String::from_utf8(fs::read(p.as_ref())?)?).peekable(),
            rules: Vec::new(),
            scope: 0,
            file: p.into(),
        }
        .parse_toplevel()?)
    }

    /// Print the internal representation of a parsed stylesheet
    ///
    /// Very closely resembles the origin SASS, but contains only things translatable
    /// to pure CSS
    ///
    /// Used mainly in debugging, but can at times be useful
    #[inline]
    #[allow(dead_code)]
    pub fn pretty_print<W: Write>(&self, buf: W) -> SassResult<()> {
        PrettyPrinter::new(buf).pretty_print(self)
    }

    #[inline]
    #[allow(dead_code)]
    fn pretty_print_selectors<W: Write>(&self, buf: W) -> SassResult<()> {
        PrettyPrinter::new(buf).pretty_print_preserve_super_selectors(self)
    }

    /// Write the internal representation as CSS to `buf`
    ///
    /// ```norun
    /// use std::io::{BufWriter, stdout};
    /// use grass::{SassResult, StyleSheet};
    /// # use tempfile::Builder;
    /// # use std::io::Write;
    ///
    /// fn main() -> SassResult<()> {
    ///     # let mut file = Builder::new().prefix("input.scss").tempfile().unwrap();
    ///     # write!(file, "a {{\n  color: red}}")?;
    ///     let mut buf = BufWriter::new(stdout());
    ///     StyleSheet::from_path("input.scss")?.print_as_css(&mut buf)
    /// }
    /// ```
    #[inline]
    pub fn print_as_css<W: Write>(self, buf: &mut W) -> SassResult<()> {
        Css::from_stylesheet(self).pretty_print(buf)
    }
}

#[derive(Debug, Clone)]
struct StyleSheetParser<'a> {
    global_scope: Scope,
    lexer: Peekable<Lexer<'a>>,
    rules: Vec<Stmt>,
    scope: u32,
    file: String,
}

impl<'a> StyleSheetParser<'a> {
    fn parse_toplevel(mut self) -> SassResult<(Vec<Stmt>, Scope)> {
        let mut rules: Vec<Stmt> = Vec::new();
        while let Some(Token { kind, pos }) = self.lexer.peek() {
            match kind {
                TokenKind::Ident(_)
                | TokenKind::Attribute(_)
                | TokenKind::Interpolation
                | TokenKind::Symbol(Symbol::Hash)
                | TokenKind::Symbol(Symbol::Colon)
                | TokenKind::Symbol(Symbol::Mul)
                | TokenKind::Symbol(Symbol::Period) => rules
                    .extend(self.eat_rules(&Selector(Vec::new()), &mut self.global_scope.clone())),
                TokenKind::Whitespace(_) => {
                    self.lexer.next();
                    continue;
                }
                TokenKind::Variable(_) => {
                    let Token { pos, kind } = self
                        .lexer
                        .next()
                        .expect("this must exist because we have already peeked");
                    let name = match kind {
                        TokenKind::Variable(n) => n,
                        _ => unsafe { std::hint::unreachable_unchecked() },
                    };
                    devour_whitespace(&mut self.lexer);
                    if self
                        .lexer
                        .next()
                        .unwrap_or_else(|| self.error(pos, "expected value after variable"))
                        .kind
                        != TokenKind::Symbol(Symbol::Colon)
                    {
                        self.error(pos, "unexpected variable use at toplevel");
                    }
                    let val = eat_variable_value(&mut self.lexer, &self.global_scope)
                        .unwrap_or_else(|err| self.error(err.0, &err.1));
                    self.global_scope.vars.insert(name, val);
                }
                TokenKind::MultilineComment(_) => {
                    let comment = match self
                        .lexer
                        .next()
                        .expect("this must exist because we have already peeked")
                        .kind
                    {
                        TokenKind::MultilineComment(c) => c,
                        _ => unsafe { std::hint::unreachable_unchecked() },
                    };
                    rules.push(Stmt::MultilineComment(comment));
                }
                TokenKind::AtRule(AtRuleKind::Import) => {
                    let Token { pos, .. } = self
                        .lexer
                        .next()
                        .expect("this must exist because we have already peeked");
                    devour_whitespace(&mut self.lexer);
                    let mut file_name = String::new();
                    match self
                        .lexer
                        .next()
                        .unwrap_or_else(|| self.error(pos, "expected value after @import"))
                        .kind
                    {
                        TokenKind::Symbol(Symbol::DoubleQuote) => {
                            while let Some(tok) = self.lexer.next() {
                                if tok.kind == TokenKind::Symbol(Symbol::DoubleQuote) {
                                    break;
                                }
                                file_name.push_str(&tok.kind.to_string());
                            }
                        }
                        TokenKind::Symbol(Symbol::SingleQuote) => {
                            while let Some(tok) = self.lexer.next() {
                                if tok.kind == TokenKind::Symbol(Symbol::SingleQuote) {
                                    break;
                                }
                                file_name.push_str(&tok.kind.to_string());
                            }
                        }
                        _ => todo!("expected ' or \" after @import"),
                    }
                    let Token { kind, pos } = self
                        .lexer
                        .next()
                        .expect("this must exist because we have already peeked");
                    if kind != TokenKind::Symbol(Symbol::SemiColon) {
                        self.error(pos, "expected `;` after @import declaration");
                    }

                    let (new_rules, new_scope) = import(file_name)?;
                    rules.extend(new_rules);
                    self.global_scope.merge(new_scope);
                }
                TokenKind::AtRule(_) => {
                    if let Some(Token {
                        kind: TokenKind::AtRule(ref rule),
                        pos,
                    }) = self.lexer.next()
                    {
                        match AtRule::from_tokens(rule, pos, &mut self.lexer, &self.global_scope) {
                            AtRule::Mixin(name, mixin) => {
                                self.global_scope.mixins.insert(name, *mixin);
                            }
                            AtRule::Function(name, func) => {
                                self.global_scope.functions.insert(name, *func);
                            }
                            AtRule::Error(pos, message) => self.error(pos, &message),
                            AtRule::Warn(pos, message) => self.warn(pos, &message),
                            AtRule::Debug(pos, message) => self.debug(pos, &message),
                            AtRule::Return(_) => todo!("@return in unexpected location!"),
                        }
                    }
                }
                TokenKind::Symbol(Symbol::BitAnd) => {
                    return Err(SassError::new("Base-level rules cannot contain the parent-selector-referencing character '&'.", pos.clone()))
                }
                _ => match self.lexer.next() {
                    Some(Token { pos, .. }) => self.error(pos, "unexpected toplevel token"),
                    _ => unsafe { std::hint::unreachable_unchecked() },
                },
            };
        }
        Ok((rules, self.global_scope))
    }

    fn eat_rules(&mut self, super_selector: &Selector, scope: &mut Scope) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while let Some(expr) = eat_expr(&mut self.lexer, scope, super_selector)
            .unwrap_or_else(|error| self.error(error.0, &error.1))
        {
            match expr {
                Expr::Style(s) => stmts.push(Stmt::Style(s)),
                Expr::MixinDecl(name, mixin) => {
                    scope.mixins.insert(name, mixin);
                }
                Expr::FunctionDecl(name, func) => {
                    scope.functions.insert(name, func);
                }
                Expr::Selector(s) => {
                    self.scope += 1;
                    let rules = self.eat_rules(&super_selector.zip(&s), scope);
                    stmts.push(Stmt::RuleSet(RuleSet {
                        super_selector: super_selector.clone(),
                        selector: s,
                        rules,
                    }));
                    self.scope -= 1;
                    if self.scope == 0 {
                        return stmts;
                    }
                }
                Expr::VariableDecl(name, val) => {
                    if self.scope == 0 {
                        scope.vars.insert(name.clone(), val.clone());
                        self.global_scope.vars.insert(name, val);
                    } else {
                        scope.vars.insert(name, val);
                    }
                }
                Expr::Include(rules) => stmts.extend(rules),
                Expr::Debug(pos, ref message) => self.debug(pos, message),
                Expr::Warn(pos, ref message) => self.warn(pos, message),
                Expr::MultilineComment(s) => stmts.push(Stmt::MultilineComment(s)),
            }
        }
        stmts
    }
}

pub(crate) fn eat_expr<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    scope: &Scope,
    super_selector: &Selector,
) -> Result<Option<Expr>, (Pos, String)> {
    let mut values = Vec::with_capacity(5);
    while let Some(tok) = toks.peek() {
        match &tok.kind {
            TokenKind::Symbol(Symbol::SemiColon) => {
                toks.next();
                devour_whitespace(toks);
                return Ok(Some(Expr::Style(match Style::from_tokens(values, scope) {
                    Ok(x) => x,
                    Err(_) => return Ok(None),
                })));
            }
            TokenKind::Symbol(Symbol::CloseCurlyBrace) => {
                if values.is_empty() {
                    toks.next();
                    devour_whitespace(toks);
                    return Ok(None);
                }
                return Ok(Some(Expr::Style(match Style::from_tokens(values, scope) {
                    Ok(x) => x,
                    Err(_) => return Ok(None),
                })));
            }
            TokenKind::Symbol(Symbol::OpenCurlyBrace) => {
                toks.next();
                devour_whitespace(toks);
                return Ok(Some(Expr::Selector(Selector::from_tokens(
                    &mut values.into_iter().peekable(),
                    scope,
                ))));
            }
            TokenKind::Variable(_) => {
                let tok = toks
                    .next()
                    .expect("this must exist because we have already peeked");
                let name = match tok.kind {
                    TokenKind::Variable(n) => n,
                    _ => unsafe { std::hint::unreachable_unchecked() },
                };
                if let TokenKind::Symbol(Symbol::Colon) =
                    toks.peek().expect("expected something after variable").kind
                {
                    toks.next();
                    devour_whitespace(toks);
                    return Ok(Some(Expr::VariableDecl(
                        name,
                        eat_variable_value(toks, scope)?,
                    )));
                } else {
                    values.push(Token {
                        kind: TokenKind::Variable(name),
                        pos: tok.pos,
                    });
                }
            }
            TokenKind::MultilineComment(_) => {
                let tok = toks
                    .next()
                    .expect("this must exist because we have already peeked");
                devour_whitespace(toks);
                if values.is_empty() {
                    let s = match tok.kind {
                        TokenKind::MultilineComment(s) => s,
                        _ => unsafe { std::hint::unreachable_unchecked() },
                    };
                    return Ok(Some(Expr::MultilineComment(s)));
                } else {
                    values.push(tok);
                }
            }
            TokenKind::AtRule(AtRuleKind::Include) => {
                return Ok(Some(Expr::Include(eat_include(
                    toks,
                    scope,
                    super_selector,
                )?)));
            }
            TokenKind::AtRule(_) => {
                if let Some(Token {
                    kind: TokenKind::AtRule(ref rule),
                    pos,
                }) = toks.next()
                {
                    return match AtRule::from_tokens(rule, pos, toks, scope) {
                        AtRule::Mixin(name, mixin) => Ok(Some(Expr::MixinDecl(name, *mixin))),
                        AtRule::Function(name, func) => Ok(Some(Expr::FunctionDecl(name, *func))),
                        AtRule::Debug(a, b) => Ok(Some(Expr::Debug(a, b))),
                        AtRule::Warn(a, b) => Ok(Some(Expr::Warn(a, b))),
                        AtRule::Error(a, b) => Err((a, b)),
                        AtRule::Return(_) => todo!("@return in unexpected location!"),
                    };
                }
            }
            TokenKind::Interpolation => values.extend(eat_interpolation(toks)),
            _ => match toks.next() {
                Some(tok) => values.push(tok),
                _ => unsafe { std::hint::unreachable_unchecked() },
            },
        };
    }
    Ok(None)
}

fn eat_interpolation<I: Iterator<Item = Token>>(toks: &mut Peekable<I>) -> Vec<Token> {
    let mut vals = Vec::new();
    let mut n = 0;
    for tok in toks {
        match tok.kind {
            TokenKind::Symbol(Symbol::OpenCurlyBrace) => n += 1,
            TokenKind::Symbol(Symbol::CloseCurlyBrace) => n -= 1,
            TokenKind::Interpolation => n += 1,
            _ => {}
        }
        vals.push(tok);
        if n == 0 {
            break;
        }
    }
    vals
}

/// Functions that print to stdout or stderr
impl<'a> StyleSheetParser<'a> {
    fn debug(&self, pos: Pos, message: &str) {
        eprintln!("{}:{} Debug: {}", self.file, pos.line(), message);
    }

    fn warn(&self, pos: Pos, message: &str) {
        eprintln!(
            "Warning: {}\n\t{} {}:{} todo!(scope)",
            message,
            self.file,
            pos.line(),
            pos.column()
        );
    }

    fn error(&self, pos: Pos, message: &str) -> ! {
        eprintln!("Error: {}", message);
        eprintln!(
            "{} {}:{} todo!(scope) on line {} at column {}",
            self.file,
            pos.line(),
            pos.column(),
            pos.line(),
            pos.column()
        );
        let padding = vec![' '; format!("{}", pos.line()).len() + 1]
            .iter()
            .collect::<String>();
        eprintln!("{}|", padding);
        eprint!("{} | ", pos.line());
        eprintln!("todo! get line to print as error");
        eprintln!(
            "{}| {}^",
            padding,
            vec![' '; pos.column() as usize].iter().collect::<String>()
        );
        eprintln!("{}|", padding);
        std::process::exit(1);
    }
}
