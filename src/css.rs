//! # Convert from SCSS AST to CSS
use crate::{RuleSet, Selector, Stmt, Style, StyleSheet};
// use crate::common::AtRule;
use std::fmt;
use std::io;

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
    idx: usize,
}

impl Css {
    pub const fn new() -> Self {
        Css {
            blocks: Vec::new(),
            idx: 0,
        }
    }

    pub fn from_stylesheet(s: StyleSheet) -> Self {
        Css::new().parse_stylesheet(s)
    }

    fn parse_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Style(s) => self.blocks[self.idx - 1].push_style(s),
            Stmt::MultilineComment(s) => {
                if self.idx == 0 {
                    self.blocks.push(Toplevel::MultilineComment(s));
                } else {
                    self.blocks[self.idx - 1].push_comment(s)
                }
            }
            Stmt::RuleSet(RuleSet {
                selector,
                super_selector,
                rules,
            }) => {
                if self.idx == 0 {
                    self.idx = self.blocks.len() + 1;
                    self.blocks
                        .push(Toplevel::new_rule(super_selector.zip(selector)));
                    for rule in rules {
                        self.parse_stmt(rule);
                    }
                    self.idx = 0;
                } else {
                    self.idx += 1;
                    self.blocks
                        .push(Toplevel::new_rule(super_selector.zip(selector)));
                    for rule in rules {
                        self.parse_stmt(rule);
                    }
                    self.idx -= 1;
                }
            }
        }
    }

    fn parse_stylesheet(mut self, s: StyleSheet) -> Css {
        for stmt in s.rules {
            self.parse_stmt(stmt);
        }
        self
    }

    pub fn pretty_print<W: std::io::Write>(self, buf: &mut W) -> io::Result<()> {
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
