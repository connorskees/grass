//! # Convert from SCSS AST to CSS
use crate::{RuleSet, SassResult, Selector, Stmt, Style, StyleSheet};
use std::fmt;
use std::io::Write;

#[derive(Debug, Clone)]
pub enum Toplevel {
    RuleSet(Selector, Vec<BlockEntry>),
    MultilineComment(String),
    // AtRule(AtRule),
}

#[derive(Debug, Clone)]
pub enum BlockEntry {
    Style(Style),
    MultilineComment(String),
    // AtRule(AtRule),
}

impl fmt::Display for BlockEntry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BlockEntry::Style(s) => writeln!(f, "  {}", s),
            BlockEntry::MultilineComment(s) => writeln!(f, "  /*{}*/", s),
        }
    }
}

impl Toplevel {
    const fn new_rule(selector: Selector) -> Self {
        Toplevel::RuleSet(selector, Vec::new())
    }

    fn push_style(&mut self, s: Style) {
        if let Toplevel::RuleSet(_, entries) = self {
            entries.push(BlockEntry::Style(s));
        }
    }

    fn push_comment(&mut self, s: String) {
        if let Toplevel::RuleSet(_, entries) = self {
            entries.push(BlockEntry::MultilineComment(s));
        }
    }
}

#[derive(Debug, Clone)]
pub struct Css {
    blocks: Vec<Toplevel>,
}

impl Css {
    pub const fn new() -> Self {
        Css { blocks: Vec::new() }
    }

    pub fn from_stylesheet(s: StyleSheet) -> Self {
        Css::new().parse_stylesheet(s)
    }

    fn parse_stmt(&mut self, stmt: Stmt) -> Vec<Toplevel> {
        match stmt {
            Stmt::RuleSet(RuleSet {
                selector,
                super_selector,
                rules,
            }) => {
                let mut vals = vec![Toplevel::new_rule(super_selector.zip(&selector))];
                for rule in rules {
                    match rule {
                        Stmt::RuleSet(_) => vals.extend(self.parse_stmt(rule)),
                        Stmt::Style(s) => vals
                            .get_mut(0)
                            .expect("expected block to exist")
                            .push_style(s),
                        Stmt::MultilineComment(s) => vals
                            .get_mut(0)
                            .expect("expected block to exist")
                            .push_comment(s),
                    };
                }
                vals
            }
            Stmt::MultilineComment(s) => vec![Toplevel::MultilineComment(s)],
            Stmt::Style(_) => panic!("expected toplevel element, found style"),
        }
    }

    fn parse_stylesheet(mut self, s: StyleSheet) -> Css {
        for stmt in s.0 {
            let v = self.parse_stmt(stmt);
            self.blocks.extend(v);
        }
        self
    }

    pub fn pretty_print<W: Write>(self, buf: &mut W) -> SassResult<()> {
        for block in self.blocks {
            match block {
                Toplevel::RuleSet(selector, styles) => {
                    if styles.is_empty() {
                        continue;
                    }
                    writeln!(buf, "{} {{", selector)?;
                    for style in styles {
                        write!(buf, "{}", style)?;
                    }
                    writeln!(buf, "}}")?;
                }
                Toplevel::MultilineComment(s) => {
                    writeln!(buf, "/*{}*/", s)?;
                }
            }
        }
        Ok(())
    }
}
