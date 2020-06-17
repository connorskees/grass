//! # Convert from SCSS AST to CSS
use std::io::Write;

use codemap::CodeMap;

use crate::{error::SassResult, parse::Stmt, selector::Selector, style::Style};

#[derive(Debug, Clone)]
enum Toplevel {
    RuleSet(Selector, Vec<BlockEntry>),
    MultilineComment(String),
    UnknownAtRule {
        name: String,
        params: String,
        body: Vec<Stmt>,
    },
    Media {
        params: String,
        body: Vec<Stmt>,
    },
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

    pub(crate) fn from_stmts(s: Vec<Stmt>) -> SassResult<Self> {
        Css::new().parse_stylesheet(s)
    }

    fn parse_stmt(&mut self, stmt: Stmt) -> SassResult<Vec<Toplevel>> {
        Ok(match stmt {
            Stmt::RuleSet {
                selector,
                super_selector,
                body,
            } => {
                let selector = selector
                    .resolve_parent_selectors(&super_selector, true)
                    .remove_placeholders();
                if selector.is_empty() {
                    return Ok(Vec::new());
                }
                let mut vals = vec![Toplevel::new_rule(selector)];
                for rule in body {
                    match rule {
                        Stmt::RuleSet { .. } => vals.extend(self.parse_stmt(rule)?),
                        Stmt::Style(s) => vals.get_mut(0).unwrap().push_style(*s)?,
                        Stmt::Comment(s) => vals.get_mut(0).unwrap().push_comment(s),
                        Stmt::Media { params, body, .. } => {
                            vals.push(Toplevel::Media { params, body })
                        }
                        Stmt::UnknownAtRule {
                            params, body, name, ..
                        } => vals.push(Toplevel::UnknownAtRule { params, body, name }),
                        Stmt::Return(..) => unreachable!(),
                        Stmt::AtRoot { body } => body
                            .into_iter()
                            .map(|r| Ok(vals.extend(self.parse_stmt(r)?)))
                            .collect::<SassResult<()>>()?,
                    };
                }
                vals
            }
            Stmt::Comment(s) => vec![Toplevel::MultilineComment(s)],
            Stmt::Style(s) => vec![Toplevel::Style(s)],
            Stmt::Media { params, body, .. } => vec![Toplevel::Media { params, body }],
            Stmt::UnknownAtRule {
                params, name, body, ..
            } => vec![Toplevel::UnknownAtRule { params, name, body }],
            Stmt::Return(..) => unreachable!("@return: {:?}", stmt),
            Stmt::AtRoot { .. } => unreachable!("@at-root: {:?}", stmt),
        })
    }

    fn parse_stylesheet(mut self, stmts: Vec<Stmt>) -> SassResult<Css> {
        let mut is_first = true;
        for stmt in stmts {
            let v = self.parse_stmt(stmt)?;
            // this is how we print newlines between unrelated styles
            // it could probably be refactored
            if !v.is_empty() {
                if let Some(Toplevel::MultilineComment(..)) = v.get(0) {
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

    pub fn pretty_print(self, map: &CodeMap) -> SassResult<String> {
        let mut string = Vec::new();
        self._inner_pretty_print(&mut string, map, 0)?;
        if string.iter().any(|s| !s.is_ascii()) {
            return Ok(format!("@charset \"UTF-8\";\n{}", unsafe {
                String::from_utf8_unchecked(string)
            }));
        }
        Ok(unsafe { String::from_utf8_unchecked(string) })
    }

    fn _inner_pretty_print(
        self,
        buf: &mut Vec<u8>,
        map: &CodeMap,
        nesting: usize,
    ) -> SassResult<()> {
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
                Toplevel::UnknownAtRule { params, name, body } => {
                    if params.is_empty() {
                        write!(buf, "{}@{}", padding, name)?;
                    } else {
                        write!(buf, "{}@{} {}", padding, name, params)?;
                    }

                    if body.is_empty() {
                        writeln!(buf, ";")?;
                        continue;
                    } else {
                        writeln!(buf, " {{")?;
                    }

                    Css::from_stmts(body)?._inner_pretty_print(buf, map, nesting + 1)?;
                    writeln!(buf, "{}}}", padding)?;
                }
                Toplevel::Media { params, body } => {
                    if body.is_empty() {
                        continue;
                    }
                    writeln!(buf, "{}@media {} {{", padding, params)?;
                    Css::from_stmts(body)?._inner_pretty_print(buf, map, nesting + 1)?;
                    writeln!(buf, "{}}}", padding)?;
                }
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
