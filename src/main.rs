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
use std::fmt::{self, Display};
use std::fs;
use std::io;
use std::path::Path;
use std::iter::{Iterator, Peekable};

use crate::common::{AtRule, Keyword, Op, Pos, Symbol, Whitespace};
use crate::css::Css;
use crate::error::SassError;
use crate::format::PrettyPrinter;
use crate::lexer::Lexer;
use crate::selector::Selector;
use crate::style::Style;
use crate::units::Unit;

mod color;
mod common;
mod css;
mod error;
mod format;
mod lexer;
mod selector;
mod style;
mod units;

type SassResult<T> = Result<T, SassError>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TokenKind {
    Ident(String),
    Symbol(Symbol),
    Function(String, Vec<String>),
    AtRule(AtRule),
    Keyword(Keyword),
    Number(String),
    Unit(Unit),
    Whitespace(Whitespace),
    Variable(String),
    Selector(Selector),
    Style(Vec<Token>),
    Op(Op),
    // todo! preserve multi-line comments
    MultilineComment(String),
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Ident(s) | TokenKind::Number(s) => write!(f, "{}", s),
            TokenKind::Symbol(s) => write!(f, "{}", s),
            TokenKind::AtRule(s) => write!(f, "{}", s),
            TokenKind::Op(s) => write!(f, "{}", s),
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
pub struct Token {
    pos: Pos,
    pub kind: TokenKind,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StyleSheet {
    rules: Vec<Stmt>,
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
    pub fn new(input: &str) -> SassResult<StyleSheet> {
        StyleSheetParser {
            global_variables: HashMap::new(),
            lexer: Lexer::new(input).peekable(),
            rules: Vec::new(),
            scope: 0,
            file: String::from("stdin"),
        }
        .parse_toplevel()
    }

    pub fn from_path<P: AsRef<Path> + Into<String>>(p: P) -> SassResult<StyleSheet> {
        StyleSheetParser {
            global_variables: HashMap::new(),
            lexer: Lexer::new(&fs::read_to_string(p.as_ref())?).peekable(),
            rules: Vec::new(),
            scope: 0,
            file: p.into(),
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
    scope: u32,
    file: String,
}

impl<'a> StyleSheetParser<'a> {
    fn parse_toplevel(mut self) -> SassResult<StyleSheet> {
        let mut rules = Vec::new();
        while let Some(Token { kind, .. }) = self.lexer.peek() {
            match kind.clone() {
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
                    let Token { ref pos, .. } = self
                        .lexer
                        .next()
                        .expect("this cannot occur as we have already peeked");
                    self.devour_whitespace();
                    if  self
                        .lexer
                        .next()
                        .unwrap_or_else(|| self.error(pos, "expected value after variable"))
                        .kind
                        != TokenKind::Symbol(Symbol::Colon)
                    {
                        self.error(pos, "unexpected variable use at toplevel");
                    }
                    let val = self.eat_variable_value();
                    self.global_variables.insert(name, val);
                }
                TokenKind::AtRule(_) => self.eat_at_rule(),
                _ => todo!("unexpected toplevel token"),
            };
        }
        Ok(StyleSheet { rules })
    }

    fn eat_at_rule(&mut self) {
        if let Some(Token {
            kind: TokenKind::AtRule(ref rule),
            ref pos,
        }) = self.lexer.next()
        {
            match rule {
                AtRule::Error => {
                    self.devour_whitespace();
                    let message = self
                        .lexer
                        .by_ref()
                        .take_while(|x| x.kind != TokenKind::Symbol(Symbol::SemiColon))
                        .map(|x| x.kind.to_string())
                        .collect::<String>();
                    self.error(pos, &message);
                }
                _ => todo!("encountered unimplemented at rule"),
            }
        }
    }

    fn error(&self, pos: &Pos, message: &str) -> ! {
        eprintln!("Error: {}", message);
        eprintln!(
            "{} {}:{} scope on line {} at column {}",
            self.file,
            pos.line(),
            pos.column(),
            pos.line(),
            pos.column()
        );
        let padding = vec![' '; format!("{}", pos.line()).len() + 1].iter().collect::<String>();
        eprintln!("{}|", padding);
        eprint!("{} | ", pos.line());
        eprintln!("todo! get line to print as error");
        eprintln!("{}| {}^", padding, vec![' '; pos.column() as usize].iter().collect::<String>());
        eprintln!("{}|", padding);
        std::process::exit(1);
    }

    fn eat_variable_value(&mut self) -> Vec<Token> {
        self.devour_whitespace();
        let iter1 = self
            .lexer
            .by_ref()
            .take_while(|x| x.kind != TokenKind::Symbol(Symbol::SemiColon))
            .collect::<Vec<Token>>();
        let mut iter2 = Vec::with_capacity(iter1.len());
        for tok in iter1 {
            if let Token { kind: TokenKind::Variable(ref name), ref pos } = tok {
                iter2.extend(
                    self.global_variables
                        .get(name)
                        .unwrap_or_else(|| self.error(pos, "Undefined variable"))
                        .clone(),
                );
            } else {
                iter2.push(tok);
            }
        }
        iter2
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
                    self.scope += 1;
                    let rules = self.eat_rules(&super_selector.clone().zip(s.clone()), vars);
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
                        vars.insert(name.clone(), val.clone());
                        self.global_variables.insert(name, val);
                    } else {
                        vars.insert(name, val);
                    }
                }
            }
        }
        stmts
    }

    fn eat_expr(&mut self, vars: &HashMap<String, Vec<Token>>) -> Result<Expr, ()> {
        let mut values = Vec::with_capacity(5);
        while let Some(tok) = self.lexer.next() {
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
                    if let TokenKind::Symbol(Symbol::Colon) = self
                        .lexer
                        .peek()
                        .expect("expected something after variable")
                        .kind
                    {
                        self.lexer.next();
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

fn main() -> SassResult<()> {
    let mut stdout = std::io::stdout();
    let s = StyleSheet::from_path("input.scss")?;
    // dbg!(s);
    // s.pretty_print(&mut stdout)?;
    // s.pretty_print_selectors(&mut stdout)?;
    s.print_as_css(&mut stdout)?;
    // dbg!(Css::from_stylesheet(s));
    // println!("{}", s);
    // drop(input);
    Ok(())
}

#[cfg(test)]
mod test_css {
    use super::StyleSheet;
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

    test!(
        nesting_el_mul_el,
        "a, b {\n  a, b {\n  color: red\n}\n}\n",
        "a a, b a, a b, b b {\n  color: red;\n}\n"
    );
    test!(basic_style, "a {\n  color: red;\n}\n");
    test!(two_styles, "a {\n  color: red;\n  color: blue;\n}\n");
    test!(selector_mul, "a, b {\n  color: red;\n}\n");
}
