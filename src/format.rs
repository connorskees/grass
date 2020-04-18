use std::fmt::Write;

use crate::atrule::AtRule;
use crate::error::SassResult;
use crate::{RuleSet, Stmt, StyleSheet};

pub(crate) struct PrettyPrinter<W: Write> {
    buf: W,
    scope: usize,
}

impl<W: Write> PrettyPrinter<W> {
    pub(crate) fn new(buf: W) -> Self {
        PrettyPrinter { buf, scope: 0 }
    }

    /// Pretty print `crate::Stmt`
    /// Throws away super selectors and variables
    fn pretty_print_stmt(&mut self, stmt: &Stmt) -> SassResult<()> {
        let padding = vec![' '; self.scope * 2].iter().collect::<String>();
        match stmt {
            Stmt::MultilineComment(s) => writeln!(self.buf, "{}/*{}*/", padding, s)?,
            Stmt::RuleSet(RuleSet {
                selector, rules, ..
            }) => {
                writeln!(self.buf, "{}{} {{", padding, selector)?;
                self.scope += 1;
                for rule in rules {
                    self.pretty_print_stmt(rule)?;
                }
                writeln!(self.buf, "{}}}", padding)?;
                self.scope -= 1;
            }
            Stmt::Style(s) => {
                writeln!(self.buf, "{}{}", padding, s.to_string()?)?;
            }
            Stmt::AtRule(r) => match r {
                AtRule::Unknown(..) => todo!("Display @rules properly"),
                _ => todo!(),
            },
        }
        Ok(())
    }

    /// Pretty print SCSS
    ///
    /// The result should be an exact copy of the SCSS input
    /// Empty rules are included
    pub fn pretty_print(&mut self, s: &StyleSheet) -> SassResult<()> {
        for rule in &s.0 {
            self.pretty_print_stmt(rule)?;
        }
        Ok(())
    }
}
