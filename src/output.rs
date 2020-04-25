//! # Convert from SCSS AST to CSS
use std::io::Write;

use crate::atrule::AtRule;
use crate::error::SassResult;
use crate::{RuleSet, Selector, Stmt, Style, StyleSheet};

#[derive(Debug, Clone)]
enum Toplevel {
    RuleSet(Selector, Vec<BlockEntry>),
    MultilineComment(String),
    AtRule(AtRule),
    Newline,
    Style(Box<Style>),
}

#[derive(Debug, Clone)]
enum BlockEntry {
    Style(Box<Style>),
    MultilineComment(String),
}

impl BlockEntry {
    pub fn to_string(&self) -> SassResult<String> {
        match self {
            BlockEntry::Style(s) => s.to_string(),
            BlockEntry::MultilineComment(s) => Ok(format!("/*{}*/", s)),
        }
    }
}

impl Toplevel {
    const fn new_rule(selector: Selector) -> Self {
        Toplevel::RuleSet(selector, Vec::new())
    }

    fn push_style(&mut self, mut s: Style) -> SassResult<()> {
        s = s.eval()?;
        if s.value.is_null(s.value.span)? {
            return Ok(());
        }
        if let Toplevel::RuleSet(_, entries) = self {
            entries.push(BlockEntry::Style(Box::new(s)));
        }
        Ok(())
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

    pub fn from_stylesheet(s: StyleSheet) -> SassResult<Self> {
        Css::new().parse_stylesheet(s)
    }

    fn parse_stmt(&mut self, stmt: Stmt) -> SassResult<Vec<Toplevel>> {
        Ok(match stmt {
            Stmt::RuleSet(RuleSet {
                selector,
                super_selector,
                rules,
            }) => {
                let selector = super_selector.zip(&selector).remove_placeholders();
                if selector.is_empty() {
                    return Ok(Vec::new());
                }
                let mut vals = vec![Toplevel::new_rule(selector)];
                for rule in rules {
                    match rule.node {
                        Stmt::RuleSet(_) => vals.extend(self.parse_stmt(rule.node)?),
                        Stmt::Style(s) => vals
                            .get_mut(0)
                            .expect("expected block to exist")
                            .push_style(*s)?,
                        Stmt::MultilineComment(s) => vals
                            .get_mut(0)
                            .expect("expected block to exist")
                            .push_comment(s),
                        Stmt::AtRule(AtRule::AtRoot(stmts)) => stmts
                            .into_iter()
                            .map(|r| Ok(vals.extend(self.parse_stmt(r.node)?)))
                            .collect::<SassResult<()>>()?,
                        Stmt::AtRule(r) => vals.push(Toplevel::AtRule(r)),
                    };
                }
                vals
            }
            Stmt::MultilineComment(s) => vec![Toplevel::MultilineComment(s)],
            Stmt::Style(s) => vec![Toplevel::Style(s)],
            Stmt::AtRule(r) => vec![Toplevel::AtRule(r)],
        })
    }

    fn parse_stylesheet(mut self, s: StyleSheet) -> SassResult<Css> {
        let mut is_first = true;
        for stmt in s.0 {
            let v = self.parse_stmt(stmt.node)?;
            // this is how we print newlines between unrelated styles
            // it could probably be refactored
            if !v.is_empty() {
                if let Toplevel::MultilineComment(..) = v[0] {
                } else if is_first {
                    is_first = false;
                } else {
                    self.blocks.push(Toplevel::Newline);
                }
                self.blocks.extend(v);
            }
        }
        Ok(self)
    }

    pub fn pretty_print(self) -> SassResult<String> {
        let mut string = Vec::new();
        self._inner_pretty_print(&mut string, 0)?;
        if string.iter().any(|s| !s.is_ascii()) {
            return Ok(format!(
                "@charset \"UTF-8\";\n{}",
                String::from_utf8(string)?
            ));
        }
        Ok(String::from_utf8(string)?)
    }

    fn _inner_pretty_print(self, buf: &mut Vec<u8>, nesting: usize) -> SassResult<()> {
        let mut has_written = false;
        let padding = vec![' '; nesting * 2].iter().collect::<String>();
        for block in self.blocks {
            match block {
                Toplevel::RuleSet(selector, styles) => {
                    if styles.is_empty() {
                        continue;
                    }
                    has_written = true;
                    writeln!(buf, "{}{} {{", padding, selector)?;
                    for style in styles {
                        writeln!(buf, "{}  {}", padding, style.to_string()?)?;
                    }
                    writeln!(buf, "{}}}", padding)?;
                }
                Toplevel::MultilineComment(s) => {
                    has_written = true;
                    writeln!(buf, "{}/*{}*/", padding, s)?;
                }
                Toplevel::AtRule(r) => match r {
                    AtRule::Unknown(u) => {
                        if u.body.is_empty() {
                            continue;
                        }
                        if u.params.is_empty() {
                            writeln!(buf, "{}@{} {{", padding, u.name)?;
                        } else {
                            writeln!(buf, "{}@{} {} {{", padding, u.name, u.params)?;
                        }
                        Css::from_stylesheet(StyleSheet::from_stmts(u.body))?
                            ._inner_pretty_print(buf, nesting + 1)?;
                        writeln!(buf, "{}}}", padding)?;
                    }
                    _ => todo!("at-rule other than unknown at toplevel: {:?}", r),
                },
                Toplevel::Style(s) => {
                    writeln!(buf, "{}{}", padding, s.to_string()?)?;
                }
                Toplevel::Newline => {
                    if has_written {
                        writeln!(buf)?
                    }
                }
            }
        }
        Ok(())
    }
}
