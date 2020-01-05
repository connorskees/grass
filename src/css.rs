//! # Convert from SCSS AST to CSS 
use std::io;
use crate::{Selector, Style, StyleSheet, Stmt, RuleSet};


#[derive(Debug, Clone)]
pub struct Block {
    selector: Selector,
    styles: Vec<Style>,
}

impl Block {
    fn new(selector: Selector) -> Self {
        Block {
            selector,
            styles: Vec::new(),
        }
    }

    fn push(&mut self, s: Style) {
        self.styles.push(s);
    }
}

#[derive(Debug, Clone)]
pub struct Css {
    blocks: Vec<Block>,
    idx: usize,
}

impl Css {
    pub fn new() -> Self {
        Css {
            blocks: Vec::new(),
            idx: 0,
        }
    }

    pub fn from_stylesheet(s: StyleSheet) -> Self {
        Css {
            blocks: Vec::new(),
            idx: 0,
        }
        .parse_stylesheet(s)
    }

    fn parse_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Style(s) => self.blocks[self.idx - 1].push(s),
            Stmt::RuleSet(RuleSet {
                selector,
                super_selector,
                rules,
            }) => {
                if self.idx == 0 {
                    self.idx = self.blocks.len() + 1;
                    self.blocks.push(Block::new(super_selector.zip(selector)));
                    for rule in rules {
                        self.parse_stmt(rule);
                    }
                    self.idx = 0;
                } else {
                    self.idx += 1;
                    self.blocks.push(Block::new(super_selector.zip(selector)));
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
            if block.styles.is_empty() {
                continue;
            }
            writeln!(buf, "{} {{", block.selector)?;
            for style in block.styles {
                writeln!(buf, "  {}", style)?;
            }
            writeln!(buf, "}}")?;
        }
        Ok(())
    }
}