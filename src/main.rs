#![warn(
    clippy::all,
    clippy::restriction,
    clippy::pedantic,
    clippy::nursery,
    // clippy::cargo
)]
#![deny(missing_debug_implementations)]
#![allow(
    dead_code,
    clippy::pub_enum_variant_names,
    clippy::implicit_return,
    clippy::use_self,
    clippy::missing_docs_in_private_items,
    clippy::todo,
    clippy::dbg_macro,
    clippy::unreachable,
    clippy::wildcard_enum_match_arm,
    clippy::option_expect_used,
    clippy::panic,
    clippy::unused_self,
    clippy::too_many_lines,
    clippy::integer_arithmetic,
    clippy::missing_errors_doc
)]
use std::collections::HashMap;
use std::fs;
use std::io;
use std::iter::Iterator;
use std::{
    fmt::{self, Display},
    iter::Peekable,
};

use crate::common::{Keyword, Pos, Symbol, Whitespace};
use crate::format::PrettyPrinter;
use crate::lexer::Lexer;
use crate::selector::Selector;
use crate::units::Unit;

mod color;
mod common;
mod error;
mod format;
mod lexer;
mod selector;
mod units;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TokenKind {
    Ident(String),
    Symbol(Symbol),
    Function(String, Vec<String>),
    AtRule(String),
    Keyword(Keyword),
    Number(String),
    Unit(Unit),
    Whitespace(Whitespace),
    Variable(String),
    Selector(Selector),
    Style(Vec<Token>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum StyleToken {
    Ident(String),
    Function(String, Vec<String>),
    Keyword(Keyword),
    Symbol(Symbol),
    Dimension(String, Unit),
}

impl Display for StyleToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StyleToken::Ident(s) => write!(f, "{}", s),
            StyleToken::Symbol(s) => write!(f, "{}", s),
            StyleToken::Function(name, args) => write!(f, "{}({})", name, args.join(", ")),
            StyleToken::Keyword(kw) => write!(f, "{}", kw),
            StyleToken::Dimension(val, unit) => write!(f, "{}{}", val, unit),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Token {
    pos: Pos,
    pub kind: TokenKind,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StyleSheet {
    rules: Vec<Stmt>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Style {
    property: String,
    value: Vec<StyleToken>,
}

impl Display for Style {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}: {};",
            self.property,
            self.value
                .iter()
                .map(|x| format!("{}", x))
                .collect::<Vec<String>>()
                .join(" ")
        )
    }
}

impl Style {
    fn from_tokens(raw: &[Token]) -> Result<Style, ()> {
        let mut iter = raw.iter();
        let property: String;
        loop {
            if let Some(tok) = iter.next() {
                match tok.kind {
                    TokenKind::Whitespace(_) => continue,
                    TokenKind::Ident(ref s) => {
                        property = s.clone();
                        break;
                    }
                    _ => todo!(),
                };
            } else {
                return Err(());
            }
        }

        while let Some(tok) = iter.next() {
            match tok.kind {
                TokenKind::Whitespace(_) => continue,
                TokenKind::Symbol(Symbol::Colon) => break,
                _ => todo!("found tokens before style value"),
            }
        }

        let mut value = Vec::new();

        while let Some(tok) = iter.next() {
            match tok.kind {
                TokenKind::Whitespace(_)
                | TokenKind::Symbol(Symbol::SingleQuote)
                | TokenKind::Symbol(Symbol::DoubleQuote) => continue,
                TokenKind::Ident(ref s) => value.push(StyleToken::Ident(s.clone())),
                TokenKind::Symbol(s) => value.push(StyleToken::Symbol(s)),
                TokenKind::Unit(u) => value.push(StyleToken::Ident(u.into())),
                TokenKind::Number(ref num) => {
                    if let Some(t) = iter.next() {
                        match &t.kind {
                            &TokenKind::Unit(unit) => {
                                value.push(StyleToken::Dimension(num.clone(), unit))
                            }
                            TokenKind::Ident(ref s) => {
                                value.push(StyleToken::Dimension(num.clone(), Unit::None));
                                value.push(StyleToken::Ident(s.clone()));
                            }
                            TokenKind::Whitespace(_) => {
                                value.push(StyleToken::Dimension(num.clone(), Unit::None))
                            }
                            _ => todo!(),
                        }
                    } else {
                        value.push(StyleToken::Dimension(num.clone(), Unit::None))
                    }
                }
                _ => todo!("style value not ident or dimension"),
            }
        }
        Ok(Style { property, value })
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Stmt {
    Style(Style),
    RuleSet(RuleSet),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RuleSet {
    selector: Selector,
    rules: Vec<Stmt>,
    // potential optimization: we don't *need* to own the selector
    super_selector: Option<Selector>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum Expr {
    Style(Style),
    Selector(Selector),
}

impl StyleSheet {
    #[must_use]
    pub fn new(input: &str) -> StyleSheet {
        StyleSheetParser {
            variables: HashMap::new(),
            lexer: Lexer::new(input).peekable(),
            rules: Vec::new(),
        }
        .parse_toplevel()
    }

    pub fn pretty_print<W: std::io::Write>(&self, buf: W) -> io::Result<()> {
        PrettyPrinter::new(buf).pretty_print(self)
    }
}

#[derive(Debug, Clone)]
struct StyleSheetParser<'a> {
    variables: HashMap<String, String>,
    lexer: Peekable<Lexer<'a>>,
    rules: Vec<Stmt>,
}

impl<'a> StyleSheetParser<'a> {
    fn parse_toplevel(&'a mut self) -> StyleSheet {
        let mut rules = Vec::new();
        while let Some(tok) = self.lexer.peek() {
            match tok.kind {
                TokenKind::Ident(_)
                | TokenKind::Selector(_)
                | TokenKind::Symbol(Symbol::Hash)
                | TokenKind::Symbol(Symbol::Colon)
                | TokenKind::Symbol(Symbol::Mul)
                | TokenKind::Symbol(Symbol::Period) => rules.extend(self.eat_rules(&None)),
                TokenKind::Whitespace(_) | TokenKind::Symbol(_) => {
                    self.lexer.next();
                    continue;
                }
                _ => todo!("unexpected toplevel token"),
            };
        }
        StyleSheet { rules }
    }

    fn eat_rules(&mut self, super_selector: &Option<Selector>) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while let Ok(tok) = self.eat_expr() {
            match tok {
                Expr::Style(s) => stmts.push(Stmt::Style(s)),
                Expr::Selector(s) => {
                    let rules = self.eat_rules(&Some(s.clone()));
                    stmts.push(Stmt::RuleSet(RuleSet {
                        super_selector: super_selector.clone(),
                        selector: s,
                        rules,
                    }));
                }
            }
        }
        stmts
    }

    fn eat_expr(&mut self) -> Result<Expr, ()> {
        let mut values = Vec::with_capacity(5);
        while let Some(tok) = self.lexer.next() {
            match tok.kind {
                TokenKind::Symbol(Symbol::SemiColon) | TokenKind::Symbol(Symbol::CloseBrace) => {
                    self.devour_whitespace();
                    return Ok(Expr::Style(Style::from_tokens(&values)?));
                }
                TokenKind::Symbol(Symbol::OpenBrace) => {
                    self.devour_whitespace();
                    return Ok(Expr::Selector(Selector::from_tokens(
                        values.iter().peekable(),
                    )));
                }
                _ => values.push(tok.clone()),
            };
        }
        Err(())
    }

    fn devour_whitespace(&mut self) {
        while let Some(tok) = self.lexer.peek() {
            match tok.kind {
                TokenKind::Whitespace(_) => {
                    self.lexer.next();
                }
                _ => break,
            }
        }
    }
}

fn main() -> io::Result<()> {
    let input = fs::read_to_string("input.scss")?;
    let mut stdout = std::io::stdout();
    let s = StyleSheet::new(&input);
    // dbg!(s);
    s.pretty_print(&mut stdout)?;
    // println!("{}", s);
    // drop(input);
    Ok(())
}
