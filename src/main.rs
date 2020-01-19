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
#![feature(track_caller)]
// todo! handle erroring on styles at the toplevel
use std::fmt::{self, Display};
use std::fs;
use std::io::{self, stdout, BufWriter, Write};
use std::iter::{Iterator, Peekable};
use std::path::Path;

use crate::common::{AtRule, Keyword, Op, Pos, Printer, Scope, Symbol, Whitespace};
use crate::css::Css;
use crate::error::SassError;
use crate::format::PrettyPrinter;
use crate::imports::import;
use crate::lexer::Lexer;
use crate::mixin::{eat_include, Mixin};
use crate::selector::{Attribute, Selector};
use crate::style::Style;
use crate::units::Unit;
use crate::utils::{devour_whitespace, eat_variable_value, IsWhitespace};

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

type SassResult<T> = Result<T, SassError>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Token {
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TokenKind {
    Ident(String),
    Symbol(Symbol),
    String(String),
    AtRule(AtRule),
    Keyword(Keyword),
    Number(String),
    Unit(Unit),
    Whitespace(Whitespace),
    Variable(String),
    Attribute(Attribute),
    Op(Op),
    MultilineComment(String),
    Interpolation,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Ident(s) | TokenKind::Number(s) => write!(f, "{}", s),
            TokenKind::Symbol(s) => write!(f, "{}", s),
            TokenKind::String(s) => write!(f, "\"{}\"", s),
            TokenKind::AtRule(s) => write!(f, "{}", s),
            TokenKind::Op(s) => write!(f, "{}", s),
            TokenKind::Unit(s) => write!(f, "{}", s),
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
pub enum Stmt {
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
pub struct RuleSet {
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
    VariableDecl(String, Vec<Token>),
    /// A mixin declaration `@mixin foo {}`
    MixinDecl(String, Mixin),
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

    pub fn from_path<P: AsRef<Path> + Into<String>>(p: P) -> SassResult<StyleSheet> {
        Ok(StyleSheet(
            StyleSheetParser {
                global_scope: Scope::new(),
                lexer: Lexer::new(&fs::read_to_string(p.as_ref())?).peekable(),
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
            lexer: Lexer::new(&fs::read_to_string(p.as_ref())?).peekable(),
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
    pub fn pretty_print<W: Write>(&self, buf: W) -> io::Result<()> {
        PrettyPrinter::new(buf).pretty_print(self)
    }

    #[allow(dead_code)]
    fn pretty_print_selectors<W: Write>(&self, buf: W) -> io::Result<()> {
        PrettyPrinter::new(buf).pretty_print_preserve_super_selectors(self)
    }

    /// Write the internal representation as CSS to `buf`
    pub fn print_as_css<W: Write>(self, buf: &mut W) -> io::Result<()> {
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
        while let Some(Token { kind, .. }) = self.lexer.peek() {
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
                TokenKind::AtRule(AtRule::Import) => {
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
                TokenKind::AtRule(AtRule::Mixin) => {
                    let (name, mixin) =
                        Mixin::from_tokens(&mut self.lexer, &self.global_scope).unwrap();
                    self.global_scope.mixins.insert(name, mixin);
                }
                TokenKind::AtRule(_) => {
                    if let Some(Token {
                        kind: TokenKind::AtRule(ref rule),
                        pos,
                    }) = self.lexer.next()
                    {
                        match eat_at_rule(rule, pos, &mut self.lexer, &self.global_scope) {
                            Ok(_) => todo!(),
                            Err(Printer::Error(pos, message)) => self.error(pos, &message),
                            Err(Printer::Warn(pos, message)) => self.warn(pos, &message),
                            Err(Printer::Debug(pos, message)) => self.debug(pos, &message),
                        }
                    }
                }
                _ => {
                    if let Some(Token { pos, .. }) = self.lexer.next() {
                        self.error(pos, "unexpected toplevel token")
                    } else {
                        unsafe { std::hint::unreachable_unchecked() }
                    }
                }
            };
        }
        Ok((rules, self.global_scope))
    }

    fn eat_rules(&mut self, super_selector: &Selector, scope: &mut Scope) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while let Some(tok) = eat_expr(&mut self.lexer, scope, super_selector)
            .unwrap_or_else(|error| self.error(error.0, &error.1))
        {
            match tok {
                Expr::Style(s) => stmts.push(Stmt::Style(s)),
                Expr::MixinDecl(name, mixin) => {
                    scope.mixins.insert(name, mixin);
                }
                Expr::Include(rules) => {
                    stmts.extend(rules);
                }
                Expr::Debug(pos, ref message) => {
                    self.debug(pos, message);
                }
                Expr::Warn(pos, ref message) => {
                    self.warn(pos, message);
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
                Expr::MultilineComment(s) => stmts.push(Stmt::MultilineComment(s)),
            }
        }
        stmts
    }
}

fn eat_at_rule<I: Iterator<Item = Token>>(
    rule: &AtRule,
    pos: Pos,
    toks: &mut Peekable<I>,
    scope: &Scope,
) -> Result<Expr, Printer> {
    match rule {
        AtRule::Error => {
            devour_whitespace(toks);
            let message = toks
                .take_while(|x| x.kind != TokenKind::Symbol(Symbol::SemiColon))
                .map(|x| x.kind.to_string())
                .collect::<String>();
            Err(Printer::Error(pos, message))
        }
        AtRule::Warn => {
            devour_whitespace(toks);
            let message = toks
                .take_while(|x| x.kind != TokenKind::Symbol(Symbol::SemiColon))
                .map(|x| x.kind.to_string())
                .collect::<String>();
            Err(Printer::Warn(pos, message))
        }
        AtRule::Debug => {
            devour_whitespace(toks);
            let message = toks
                .by_ref()
                .take_while(|x| x.kind != TokenKind::Symbol(Symbol::SemiColon))
                .map(|x| x.kind.to_string())
                .collect::<String>();
            Err(Printer::Debug(pos, message))
        }
        AtRule::Mixin => {
            let (name, mixin) = Mixin::from_tokens(toks, scope)?;
            Ok(Expr::MixinDecl(name, mixin))
        }
        // AtRule::Include => return Some(self.eat_include()),
        _ => todo!("encountered unimplemented at rule"),
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
            TokenKind::Symbol(Symbol::SemiColon) | TokenKind::Symbol(Symbol::CloseCurlyBrace) => {
                toks.next();
                devour_whitespace(toks);
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
                let name = if let TokenKind::Variable(n) = tok.kind {
                    n
                } else {
                    unsafe { std::hint::unreachable_unchecked() }
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
            TokenKind::AtRule(AtRule::Include) => {
                return Ok(Some(Expr::Include(eat_include(
                    toks,
                    scope,
                    super_selector,
                )?)));
            }
            TokenKind::AtRule(AtRule::Mixin) => {
                toks.next();
                let (name, mixin) = Mixin::from_tokens(toks, scope).unwrap();
                return Ok(Some(Expr::MixinDecl(name, mixin)));
            }
            TokenKind::AtRule(_) => {
                if let Some(Token {
                    kind: TokenKind::AtRule(ref rule),
                    pos,
                }) = toks.next()
                {
                    match eat_at_rule(rule, pos, toks, scope) {
                        Ok(a) => return Ok(Some(a)),
                        Err(Printer::Debug(a, b)) => return Ok(Some(Expr::Debug(a, b))),
                        Err(Printer::Warn(a, b)) => return Ok(Some(Expr::Warn(a, b))),
                        Err(Printer::Error(a, b)) => return Err((a, b)),
                    }
                }
            }
            TokenKind::Interpolation => {
                while let Some(tok) = toks.next() {
                    if tok.kind == TokenKind::Symbol(Symbol::CloseCurlyBrace) {
                        values.push(tok);
                        break;
                    }
                    values.push(tok);
                }
            }
            _ => {
                if let Some(tok) = toks.next() {
                    values.push(tok)
                } else {
                    unsafe { std::hint::unreachable_unchecked() }
                }
            }
        };
    }
    Ok(None)
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

fn main() -> SassResult<()> {
    let mut stdout = BufWriter::new(stdout());
    let mut args = std::env::args();
    args.next();
    for arg in args {
        let s = StyleSheet::from_path(arg)?;
        s.print_as_css(&mut stdout)?;
    }
    // dbg!(s);
    // s.pretty_print(&mut stdout)?;
    // s.pretty_print_selectors(&mut stdout)?;
    Ok(())
}

#[cfg(test)]
macro_rules! test {
    ($func:ident, $input:literal) => {
        #[test]
        fn $func() {
            let mut buf = Vec::new();
            StyleSheet::new($input)
                .expect(concat!("failed to parse on ", $input))
                .print_as_css(&mut buf)
                .expect(concat!("failed to pretty print on ", $input));
            assert_eq!(
                String::from($input),
                String::from_utf8(buf).expect("produced invalid utf8")
            );
        }
    };
    ($func:ident, $input:literal, $output:literal) => {
        #[test]
        fn $func() {
            let mut buf = Vec::new();
            StyleSheet::new($input)
                .expect(concat!("failed to parse on ", $input))
                .print_as_css(&mut buf)
                .expect(concat!("failed to pretty print on ", $input));
            assert_eq!(
                String::from($output),
                String::from_utf8(buf).expect("produced invalid utf8")
            );
        }
    };
}

#[cfg(test)]
mod test_variables {
    use super::StyleSheet;
    test!(
        basic_variable,
        "$height: 1px;\na {\n  height: $height;\n}\n",
        "a {\n  height: 1px;\n}\n"
    );
    test!(
        variable_redeclaration,
        "$a: 1px;\n$a: 2px;\na {\n  height: $a;\n}\n",
        "a {\n  height: 2px;\n}\n"
    );
    test!(
        variable_shadowing,
        "$a: 1px;\n$b: $a;\na {\n  height: $b;\n}\n",
        "a {\n  height: 1px;\n}\n"
    );
    test!(
        variable_shadowing_val_does_not_change,
        "$a: 1px;\n$b: $a; $a: 2px;\na {\n  height: $b;\n}\n",
        "a {\n  height: 1px;\n}\n"
    );
    test!(
        variable_shadowing_val_does_not_change_complex,
        "a {\n  color: red;\n}\n$y: before;\n$x: 1 2 $y;\n$y: after;\nfoo {\n  a: $x;\n}",
        "a {\n  color: red;\n}\nfoo {\n  a: 1 2 before;\n}\n"
    );
    test!(
        variable_whitespace,
        "$a   :    1px   ;\na {\n  height: $a;\n}\n",
        "a {\n  height: 1px;\n}\n"
    );
    test!(
        style_after_variable,
        "$a: 1px;\na {\n  height: $a;\n  color: red;\n}\n",
        "a {\n  height: 1px;\n  color: red;\n}\n"
    );
    test!(
        literal_and_variable_as_val,
        "$a: 1px;\na {\n  height: 1 $a;\n}\n",
        "a {\n  height: 1 1px;\n}\n"
    );
    test!(
        literal_and_variable_as_var,
        "$a: 1px;\n$b: 1 $a;\na {\n  height: $b;\n}\n",
        "a {\n  height: 1 1px;\n}\n"
    );
    test!(
        eats_whitespace_after_variable_value,
        "a {\n  b {\n    $c: red;\n  }\n  color: red;\n}\n",
        "a {\n  color: red;\n}\n"
    );
    test!(
        variable_changes_through_new_ruleset,
        "a {\n  $c: red;\nb {\n    $c: blue;\n  }\n  color: $c;\n}\n",
        "a {\n  color: blue;\n}\n"
    );
}

#[cfg(test)]
mod test_selectors {
    use super::StyleSheet;
    test!(
        selector_nesting_el_mul_el,
        "a, b {\n  a, b {\n  color: red\n}\n}\n",
        "a a, a b, b a, b b {\n  color: red;\n}\n"
    );
    test!(selector_element, "a {\n  color: red;\n}\n");
    test!(selector_id, "#id {\n  color: red;\n}\n");
    test!(selector_class, ".class {\n  color: red;\n}\n");
    test!(selector_el_descendant, "a a {\n  color: red;\n}\n");
    test!(selector_universal, "* {\n  color: red;\n}\n");
    test!(selector_el_class_and, "a.class {\n  color: red;\n}\n");
    test!(selector_el_id_and, "a#class {\n  color: red;\n}\n");
    test!(
        selector_el_class_descendant,
        "a .class {\n  color: red;\n}\n"
    );
    test!(selector_el_id_descendant, "a #class {\n  color: red;\n}\n");
    test!(
        selector_el_universal_descendant,
        "a * {\n  color: red;\n}\n"
    );
    test!(
        selector_universal_el_descendant,
        "* a {\n  color: red;\n}\n"
    );

    test!(selector_attribute_any, "[attr] {\n  color: red;\n}\n");
    test!(
        selector_attribute_equals,
        "[attr=val] {\n  color: red;\n}\n"
    );
    test!(
        selector_attribute_single_quotes,
        "[attr='val'] {\n  color: red;\n}\n"
    );
    test!(
        selector_attribute_double_quotes,
        "[attr=\"val\"] {\n  color: red;\n}\n"
    );
    test!(selector_attribute_in, "[attr~=val] {\n  color: red;\n}\n");
    test!(
        selector_attribute_begins_hyphen_or_exact,
        "[attr|=val] {\n  color: red;\n}\n"
    );
    test!(
        selector_attribute_starts_with,
        "[attr^=val] {\n  color: red;\n}\n"
    );
    test!(
        selector_attribute_ends_with,
        "[attr$=val] {\n  color: red;\n}\n"
    );
    test!(
        selector_attribute_contains,
        "[attr*=val] {\n  color: red;\n}\n"
    );
    test!(selector_el_attribute_and, "a[attr] {\n  color: red;\n}\n");
    test!(
        selector_el_attribute_descendant,
        "a [attr] {\n  color: red;\n}\n"
    );
    test!(selector_el_mul_el, "a, b {\n  color: red;\n}\n");
    test!(
        selector_el_immediate_child_el,
        "a > b {\n  color: red;\n}\n"
    );
    test!(selector_el_following_el, "a + b {\n  color: red;\n}\n");
    test!(selector_el_preceding_el, "a ~ b {\n  color: red;\n}\n");
    test!(selector_pseudo, ":pseudo {\n  color: red;\n}\n");
    test!(selector_el_and_pseudo, "a:pseudo {\n  color: red;\n}\n");
    test!(
        selector_el_pseudo_descendant,
        "a :pseudo {\n  color: red;\n}\n"
    );
    test!(
        selector_pseudo_el_descendant,
        ":pseudo a {\n  color: red;\n}\n"
    );
    test!(
        selector_pseudo_paren_comma,
        ":pseudo(a, b, c) {\n  color: red;\n}\n"
    );
    test!(
        selector_pseudo_paren_space,
        ":pseudo(a b c) {\n  color: red;\n}\n"
    );
    test!(
        selector_el_pseudo_paren_and,
        "a:pseudo(a, b, c) {\n  color: red;\n}\n"
    );
    test!(
        selector_el_pseudo_paren_descendant,
        "a :pseudo(a, b, c) {\n  color: red;\n}\n"
    );
    test!(
        selector_pseudo_paren_el_descendant,
        ":pseudo(a, b, c) a {\n  color: red;\n}\n"
    );
    test!(
        selector_pseudo_paren_el_nested,
        "a {\n  :pseudo(a, b, c) {\n  color: red;\n  }\n}\n",
        "a :pseudo(a, b, c) {\n  color: red;\n}\n"
    );
    test!(selector_mul, "a, b {\n  color: red;\n}\n");
    test!(
        outer_ampersand,
        "a, b {\n& c {\n  color: red;\n}\n}\n",
        "a c, b c {\n  color: red;\n}\n"
    );
    test!(
        inner_ampersand,
        "a, b {\na & c {\n  color: red;\n}\n}\n",
        "a a c, a b c {\n  color: red;\n}\n"
    );
    test!(
        ampersand_multiple_whitespace,
        " a  ,  b   {\n&c {\n  color: red;\n}\n}\n",
        "ac, bc {\n  color: red;\n}\n"
    );
    test!(
        ampersand_alone,
        "a, b {\n& {\n  color: red;\n}\n}\n",
        "a, b {\n  color: red;\n}\n"
    );
    test!(
        bem_dash_dash_selector,
        "a {\n&--b {\n  color: red;\n}\n}\n",
        "a--b {\n  color: red;\n}\n"
    );
    // test!(
    //     bem_underscore_selector,
    //     "a {\n&__b {\n  color: red;\n}\n}\n",
    //     "a__b {\n  color: red;\n}\n"
    // );
    test!(
        selector_interpolation_start,
        "#{a}bc {\n  color: red;\n}\n",
        "abc {\n  color: red;\n}\n"
    );
    test!(
        selector_interpolation_middle,
        "a#{b}c {\n  color: red;\n}\n",
        "abc {\n  color: red;\n}\n"
    );
    test!(
        selector_interpolation_end,
        "ab#{c} {\n  color: red;\n}\n",
        "abc {\n  color: red;\n}\n"
    );
    test!(
        selector_interpolation_variable,
        "$a: foo;\nab#{$a} {\n  color: red;\n}\n",
        "abfoo {\n  color: red;\n}\n"
    );
    test!(
        selector_whitespace,
        "  a  >  b  ,  c  ~  d  e  .f  #g  :h  i.j  [  k  ]  { color: red }",
        "a > b, c ~ d e .f #g :h i.j [k] {\n  color: red;\n}\n"
    );
}

#[cfg(test)]
mod test_units {
    use super::StyleSheet;
    test!(unit_none, "a {\n  height: 1;\n}\n");
    test!(unit_not_attached, "a {\n  height: 1 px;\n}\n");
    test!(unit_px, "a {\n  height: 1px;\n}\n");
    test!(unit_em, "a {\n  height: 1em;\n}\n");
    test!(unit_rem, "a {\n  height: 1rem;\n}\n");
    test!(unit_percent, "a {\n  height: 1%;\n}\n");
}

#[cfg(test)]
mod test_comments {
    use super::StyleSheet;
    test!(
        removes_inner_comments,
        "a {\n  color: red/* hi */;\n}\n",
        "a {\n  color: red;\n}\n"
    );
    test!(
        removes_inner_comments_whitespace,
        "a {\n  color: red    /* hi */;\n}\n",
        "a {\n  color: red;\n}\n"
    );
    test!(
        preserves_outer_comments_before,
        "a {\n  /* hi */\n  color: red;\n}\n"
    );
    test!(
        preserves_outer_comments_after,
        "a {\n  color: red;\n  /* hi */\n}\n"
    );
    test!(
        preserves_outer_comments_two,
        "a {\n  /* foo */\n  /* bar */\n  color: red;\n}\n"
    );
    test!(
        preserves_toplevel_comment_before,
        "/* foo */\na {\n  color: red;\n}\n"
    );
    test!(
        preserves_toplevel_comment_after,
        "a {\n  color: red;\n}\n/* foo */\n"
    );
    test!(
        removes_single_line_comment,
        "// a { color: red }\na {\n  height: 1 1px;\n}\n",
        "a {\n  height: 1 1px;\n}\n"
    );
}

#[cfg(test)]
mod test_styles {
    use super::StyleSheet;
    test!(basic_style, "a {\n  color: red;\n}\n");
    test!(two_styles, "a {\n  color: red;\n  color: blue;\n}\n");
    test!(
        two_inner_rulesets,
        "a {\n  b {\n  color: red;\n}\n  c {\n  color: white;\n}\n}\n",
        "a b {\n  color: red;\n}\na c {\n  color: white;\n}\n"
    );
    test!(
        two_rulesets,
        "a {\n  color: red;\n}\nc {\n  color: white;\n}\n"
    );
    test!(
        two_inner_outer_rulesets,
        "a {\n  b {\n  color: red;\n}\n  c {\n  color: white;\n}\n}\na {\n  b {\n  color: red;\n}\n  c {\n  color: white;\n}\n}\n",
        "a b {\n  color: red;\n}\na c {\n  color: white;\n}\na b {\n  color: red;\n}\na c {\n  color: white;\n}\n"
    );
    test!(
        removes_empty_outer_styles,
        "a {\n  b {\n    color: red;\n  }\n",
        "a b {\n  color: red;\n}\n"
    );
    test!(removes_empty_styles, "a {}\n", "");
    test!(
        doesnt_eat_style_after_ruleset,
        "a {\n  b {\n  color: red;\n}\n  color: blue;\n}\n",
        "a {\n  color: blue;\n}\na b {\n  color: red;\n}\n"
    );
    test!(
        multiline_style,
        "a {\n  color: red\n  blue;\n}\n",
        "a {\n  color: red blue;\n}\n"
    );
    test!(hyphenated_style_property, "a {\n  font-family: Arial;\n}\n");
    test!(hyphenated_style_value, "a {\n  color: Open-Sans;\n}\n");
    test!(
        space_separated_style_value,
        "a {\n  border: solid red;\n}\n"
    );
    test!(single_quoted_style_value, "a {\n  font: 'Open-Sans';\n}\n");
    test!(
        double_quoted_style_value,
        "a {\n  font: \"Open-Sans\";\n}\n"
    );
    test!(
        comma_style_value,
        "a {\n  font: Open-Sans, sans-serif;\n}\n"
    );
    test!(
        style_interpolation_start,
        "a {\n  #{c}olor: red;\n}\n",
        "a {\n  color: red;\n}\n"
    );
    test!(
        style_interpolation_middle,
        "a {\n  co#{l}or: red;\n}\n",
        "a {\n  color: red;\n}\n"
    );
    test!(
        style_interpolation_end,
        "a {\n  colo#{r}: red;\n}\n",
        "a {\n  color: red;\n}\n"
    );
    test!(
        style_interpolation_variable,
        "$a: foo;\na {\n  co#{$a}lor: red;\n}\n",
        "a {\n  cofoolor: red;\n}\n"
    );
    test!(
        style_val_interpolation_start,
        "a {\n  color: #{r}ed;\n}\n",
        "a {\n  color: red;\n}\n"
    );
    test!(
        style_val_interpolation_middle,
        "a {\n  color: r#{e}d;\n}\n",
        "a {\n  color: red;\n}\n"
    );
    test!(
        style_val_interpolation_end,
        "a {\n  color: re#{d};\n}\n",
        "a {\n  color: red;\n}\n"
    );
    test!(
        style_val_interpolation_variable,
        "$a: foo;\na {\n  color: r#{$a}ed;\n}\n",
        "a {\n  color: rfooed;\n}\n"
    );
    test!(
        style_whitespace,
        "a {\n     color      :       red    ;    \n}\n",
        "a {\n  color: red;\n}\n"
    );
}

#[cfg(test)]
mod test_misc {
    use super::*;
    test!(
        combines_hyphens,
        "a {\n  foo: bar - baz;\n}\n",
        "a {\n  foo: bar-baz;\n}\n"
    );
    test!(does_not_combine_hyphens, "a {\n  foo: bar -baz;\n}\n");
    test!(
        ident_starts_with_hyphen,
        "a {\n  foo: -webkit-bar-baz;\n}\n"
    );
    test!(ident_with_num, "el1 {\n  a: b;\n}\n");
    test!(keyword_important, "a {\n  height: 1 !important;\n}\n");
    test!(
        keyword_important_uppercase,
        "a {\n  height: 1 !IMPORTANT;\n}\n",
        "a {\n  height: 1 !important;\n}\n"
    );
    test!(
        keyword_important_not_at_end,
        "a {\n  height: !important 1;\n}\n"
    );
}

#[cfg(test)]
mod test_mixins {
    use super::*;
    test!(
        basic_mixin,
        "@mixin a {\n  color: red;\n}\n\nb {\n  @include a;\n}\n",
        "b {\n  color: red;\n}\n"
    );
    test!(
        mixin_two_styles,
        "@mixin a {\n  color: red;\n  color: blue;\n}\n\nb {\n  @include a;\n}\n",
        "b {\n  color: red;\n  color: blue;\n}\n"
    );
    test!(
        mixin_ruleset,
        "@mixin a {\n  b {\n    color: red;\n  }\n}\nb {\n  @include a;\n}\n",
        "b b {\n  color: red;\n}\n"
    );
    test!(
        mixin_two_rulesets,
        "@mixin a {\n  b {\n    color: red;\n  }\n  c {\n    color: blue;\n  }\n}\nd {\n  @include a;\n}\n",
        "d b {\n  color: red;\n}\nd c {\n  color: blue;\n}\n"
    );
    test!(
        mixin_ruleset_and_style,
        "@mixin a {\n  b {\n    color: red;\n  }\n  color: blue;\n}\nd {\n  @include a;\n}\n",
        "d {\n  color: blue;\n}\nd b {\n  color: red;\n}\n"
    );
    test!(
        mixin_style_and_ruleset,
        "@mixin a {\n  color: blue;\n  b {\n    color: red;\n}\n}\nd {\n  @include a;\n}\n",
        "d {\n  color: blue;\n}\nd b {\n  color: red;\n}\n"
    );
    test!(
        mixin_nested_rulesets,
        "@mixin a {\n  b {\n    c {\n      color: red;\n}\n}\n}\nd {\n  @include a;\n}\n",
        "d b c {\n  color: red;\n}\n"
    );
    test!(
        mixin_removes_empty_ruleset,
        "@mixin a {\n  color:red; b {\n}\n}\nd {\n  @include a;\n}\n",
        "d {\n  color: red;\n}\n"
    );
    test!(
        mixin_variable_scope_one_ruleset,
        "@mixin a {\n  $a: blue;\nb {\n  $a: red;\n}  color: $a\n}\nd {\n  @include a;\n}\n",
        "d {\n  color: red;\n}\n"
    );
    test!(
        mixin_single_arg,
        "@mixin a($b) {\n  color: $b;\n}\nd {\n  @include a(red);\n}\n",
        "d {\n  color: red;\n}\n"
    );
    test!(
        mixin_two_args,
        "@mixin a($b, $c) {\n  color: $b;\n  color: $c\n}\nd {\n  @include a(red, blue);\n}\n",
        "d {\n  color: red;\n  color: blue;\n}\n"
    );
    test!(
        mixin_arg_trailing_comma,
        "@mixin a($b, $c,) {\n  color: $b;\n  color: $c\n}\nd {\n  @include a(red, blue);\n}\n",
        "d {\n  color: red;\n  color: blue;\n}\n"
    );
    test!(
        mixin_property_interpolation,
        "@mixin a($b) {\n  #{$b}: red;\n}\nd {\n  @include a(color);\n}\n",
        "d {\n  color: red;\n}\n"
    );
    test!(
        mixin_style_interpolation,
        "@mixin a($b) {\n  color: #{$b};\n}\nd {\n  @include a(red);\n}\n",
        "d {\n  color: red;\n}\n"
    );
}

#[cfg(test)]
mod test_imports {
    use super::*;
    use tempfile::Builder;
    use Write;

    macro_rules! test_import {
        ($func:ident, $input:literal => $output:literal | $( $name:literal($content:literal) ),*) => {
            #[test]
            fn $func() {
                $(
                    write!(Builder::new().prefix($name).tempfile().unwrap(), $content).unwrap();
                )*
                let mut buf = Vec::new();
                StyleSheet::new($input)
                .expect(concat!("failed to parse on ", $input))
                .print_as_css(&mut buf)
                .expect(concat!("failed to pretty print on ", $input));
                assert_eq!(
                    String::from($output),
                    String::from_utf8(buf).expect("produced invalid utf8")
                );
            }
        }
    }

    // redundant test to ensure that the import tests are working
    test_import!(basic, "a {\n  color: red;\n}\n" => "a {\n  color: red;\n}\n" | );

    test_import!(imports_variable, "@import \"foo\";\na {\n color: $a;\n}" => "a {\n  color: red;\n}\n" | "foo"("$a: red;"));
    test_import!(single_quotes_import, "@import 'foo';\na {\n color: $a;\n}" => "a {\n  color: red;\n}\n" | "foo"("$a: red;"));
    test_import!(finds_name_scss, "@import \"foo\";\na {\n color: $a;\n}" => "a {\n  color: red;\n}\n" | "foo.scss"("$a: red;"));
    test_import!(finds_underscore_name_scss, "@import \"foo\";\na {\n color: $a;\n}" => "a {\n  color: red;\n}\n" | "_foo.scss"("$a: red;"));
}
