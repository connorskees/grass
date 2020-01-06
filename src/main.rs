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
use crate::css::Css;
use crate::format::PrettyPrinter;
use crate::lexer::Lexer;
use crate::selector::Selector;
use crate::units::Unit;

mod color;
mod common;
mod css;
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
    // todo! preserve multi-line comments
    MultilineComment(String),
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Ident(s) | TokenKind::Number(s) | TokenKind::AtRule(s) => write!(f, "{}", s),
            TokenKind::Symbol(s) => write!(f, "{}", s),
            TokenKind::Unit(s) => write!(f, "{}", s),
            TokenKind::Whitespace(s) => write!(f, "{}", s),
            TokenKind::Selector(s) => write!(f, "{}", s),
            TokenKind::Function(name, args) => write!(f, "{}({})", name, args.join(", ")),
            TokenKind::Keyword(kw) => write!(f, "{}", kw),
            TokenKind::MultilineComment(s) => write!(f, "/*{}*/", s),
            TokenKind::Variable(s) => write!(f, "${}", s),
            TokenKind::Style(_) => panic!("TokenKind should not be used to format styles"),
        }
    }
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

struct StyleParser<'a> {
    tokens: &'a [Token],
    vars: &'a HashMap<String, Vec<Token>>,
}

impl Style {
    fn deref_variable(variable: &TokenKind, vars: &HashMap<String, Vec<Token>>) -> String {
        let mut val = String::with_capacity(15);
        let v = match variable {
            TokenKind::Variable(ref v) => vars.get(v).expect("todo! expected variable to exist"),
            _ => panic!("expected variable"),
        };
        for tok in v {
            match &tok.kind {
                TokenKind::Variable(_) => val.push_str(&Self::deref_variable(&tok.kind, vars)),
                _ => val.push_str(&tok.kind.to_string()),
            };
        }
        val
    }

    fn from_tokens(raw: &[Token], vars: &HashMap<String, Vec<Token>>) -> Result<Style, ()> {
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
                TokenKind::Variable(_) => {
                    value.push(StyleToken::Ident(Self::deref_variable(&tok.kind, vars)))
                }
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
    super_selector: Selector,
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum Expr {
    Style(Style),
    Selector(Selector),
    VariableDecl(String, Vec<Token>),
}

impl StyleSheet {
    #[must_use]
    pub fn new(input: &str) -> StyleSheet {
        StyleSheetParser {
            global_variables: HashMap::new(),
            lexer: Lexer::new(input).peekable(),
            rules: Vec::new(),
        }
        .parse_toplevel()
    }

    pub fn pretty_print<W: std::io::Write>(&self, buf: W) -> io::Result<()> {
        PrettyPrinter::new(buf).pretty_print(self)
    }

    pub fn pretty_print_selectors<W: std::io::Write>(&self, buf: W) -> io::Result<()> {
        PrettyPrinter::new(buf).pretty_print_preserve_super_selectors(self)
    }

    pub fn print_as_css<W: std::io::Write>(self, buf: &mut W) -> io::Result<()> {
        Css::from_stylesheet(self).pretty_print(buf)
    }
}

#[derive(Debug, Clone)]
struct StyleSheetParser<'a> {
    global_variables: HashMap<String, Vec<Token>>,
    lexer: Peekable<Lexer<'a>>,
    rules: Vec<Stmt>,
}

impl<'a> StyleSheetParser<'a> {
    fn parse_toplevel(&mut self) -> StyleSheet {
        let mut rules = Vec::new();
        while let Some(tok) = self.lexer.peek() {
            match tok.kind.clone() {
                TokenKind::Ident(_)
                | TokenKind::Selector(_)
                | TokenKind::Symbol(Symbol::Hash)
                | TokenKind::Symbol(Symbol::Colon)
                | TokenKind::Symbol(Symbol::Mul)
                | TokenKind::Symbol(Symbol::Period) => rules
                    .extend(self.eat_rules(&Selector::None, &mut self.global_variables.clone())),
                TokenKind::Whitespace(_) | TokenKind::Symbol(_) => {
                    self.lexer.next();
                    continue;
                }
                TokenKind::Variable(name) => {
                    self.lexer.next();
                    self.devour_whitespace();
                    if self
                        .lexer
                        .next()
                        .expect("expected something after variable")
                        .kind
                        != TokenKind::Symbol(Symbol::Colon)
                    {
                        panic!("unexpected variable use at toplevel")
                    }
                    let val = self.eat_variable_value();
                    self.global_variables.insert(name, val);
                }
                _ => todo!("unexpected toplevel token"),
            };
        }
        StyleSheet { rules }
    }

    fn eat_variable_value(&mut self) -> Vec<Token> {
        self.devour_whitespace();
        self.lexer
            .by_ref()
            .take_while(|x| x.kind != TokenKind::Symbol(Symbol::SemiColon))
            .collect::<Vec<Token>>()
    }

    fn eat_rules(
        &mut self,
        super_selector: &Selector,
        vars: &mut HashMap<String, Vec<Token>>,
    ) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while let Ok(tok) = self.eat_expr(vars) {
            match tok {
                Expr::Style(s) => stmts.push(Stmt::Style(s)),
                Expr::Selector(s) => {
                    let rules = self.eat_rules(&super_selector.clone().zip(s.clone()), vars);
                    stmts.push(Stmt::RuleSet(RuleSet {
                        super_selector: super_selector.clone(),
                        selector: s,
                        rules,
                    }));
                }
                Expr::VariableDecl(name, val) => {
                    vars.insert(name, val);
                }
            }
        }
        stmts
    }

    fn eat_expr(&mut self, vars: &HashMap<String, Vec<Token>>) -> Result<Expr, ()> {
        let mut values = Vec::with_capacity(5);
        while let Some(tok) = self.lexer.next() {
            dbg!(&tok.kind);
            match tok.kind {
                TokenKind::Symbol(Symbol::SemiColon) | TokenKind::Symbol(Symbol::CloseBrace) => {
                    self.devour_whitespace();
                    return Ok(Expr::Style(Style::from_tokens(&values, vars)?));
                }
                TokenKind::Symbol(Symbol::OpenBrace) => {
                    self.devour_whitespace();
                    return Ok(Expr::Selector(Selector::from_tokens(
                        values.iter().peekable(),
                    )));
                }
                TokenKind::Variable(name) => {
                    if self
                        .lexer
                        .next()
                        .expect("expected something after variable")
                        .kind
                        == TokenKind::Symbol(Symbol::Colon)
                    {
                        self.devour_whitespace();
                        return Ok(Expr::VariableDecl(name, self.eat_variable_value()));
                    } else {
                        values.push(Token {
                            kind: TokenKind::Variable(name),
                            pos: tok.pos,
                        });
                    }
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
    // s.pretty_print(&mut stdout)?;
    // s.pretty_print_selectors(&mut stdout)?;
    s.print_as_css(&mut stdout)?;
    // dbg!(Css::from_stylesheet(s));
    // println!("{}", s);
    // drop(input);
    Ok(())
}
