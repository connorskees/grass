//! # Convert from SCSS AST to CSS
use std::io::Write;

use codemap::CodeMap;

use crate::{error::SassResult, parse::Stmt, selector::Extender, selector::Selector, style::Style};

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
    Supports {
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
pub(crate) struct Css {
    blocks: Vec<Toplevel>,
}

impl Css {
    pub const fn new() -> Self {
        Css { blocks: Vec::new() }
    }

    pub(crate) fn from_stmts(s: Vec<Stmt>, extender: &mut Extender) -> SassResult<Self> {
        Css::new().parse_stylesheet(s, extender)
    }

    fn parse_stmt(&mut self, stmt: Stmt, extender: &mut Extender) -> SassResult<Vec<Toplevel>> {
        Ok(match stmt {
            Stmt::RuleSet { selector, body } => {
                if body.is_empty() {
                    return Ok(Vec::new());
                }
                let selector = selector.remove_placeholders();
                if selector.is_empty() {
                    return Ok(Vec::new());
                }
                let mut vals = vec![Toplevel::new_rule(selector)];
                for rule in body {
                    match rule {
                        Stmt::RuleSet { .. } => vals.extend(self.parse_stmt(rule, extender)?),
                        Stmt::Style(s) => vals.get_mut(0).unwrap().push_style(*s)?,
                        Stmt::Comment(s) => vals.get_mut(0).unwrap().push_comment(s),
                        Stmt::Media { params, body, .. } => {
                            vals.push(Toplevel::Media { params, body })
                        }
                        Stmt::Supports { params, body, .. } => {
                            vals.push(Toplevel::Supports { params, body })
                        }
                        Stmt::UnknownAtRule {
                            params, body, name, ..
                        } => vals.push(Toplevel::UnknownAtRule { params, body, name }),
                        Stmt::Return(..) => unreachable!(),
                        Stmt::AtRoot { body } => body
                            .into_iter()
                            .map(|r| Ok(vals.extend(self.parse_stmt(r, extender)?)))
                            .collect::<SassResult<()>>()?,
                    };
                }
                vals
            }
            Stmt::Comment(s) => vec![Toplevel::MultilineComment(s)],
            Stmt::Style(s) => vec![Toplevel::Style(s)],
            Stmt::Media { params, body, .. } => vec![Toplevel::Media { params, body }],
            Stmt::Supports { params, body, .. } => vec![Toplevel::Supports { params, body }],
            Stmt::UnknownAtRule {
                params, name, body, ..
            } => vec![Toplevel::UnknownAtRule { params, name, body }],
            Stmt::Return(..) => unreachable!("@return: {:?}", stmt),
            Stmt::AtRoot { .. } => unreachable!("@at-root: {:?}", stmt),
        })
    }

    fn parse_stylesheet(mut self, stmts: Vec<Stmt>, extender: &mut Extender) -> SassResult<Css> {
        let mut is_first = true;
        for stmt in stmts {
            let v = self.parse_stmt(stmt, extender)?;
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

    pub fn pretty_print(self, map: &CodeMap, extender: &mut Extender) -> SassResult<String> {
        let mut string = Vec::new();
        self._inner_pretty_print(&mut string, map, extender, 0)?;
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
        extender: &mut Extender,
        nesting: usize,
    ) -> SassResult<()> {
        let mut has_written = false;
        let padding = vec![' '; nesting * 2].iter().collect::<String>();
        let mut should_emit_newline = false;
        for block in self.blocks {
            match block {
                Toplevel::RuleSet(selector, styles) => {
                    if styles.is_empty() {
                        continue;
                    }
                    has_written = true;
                    if should_emit_newline {
                        should_emit_newline = false;
                        writeln!(buf)?;
                    }
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
                    if should_emit_newline {
                        should_emit_newline = false;
                        writeln!(buf)?;
                    }

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

                    Css::from_stmts(body, extender)?._inner_pretty_print(
                        buf,
                        map,
                        extender,
                        nesting + 1,
                    )?;
                    writeln!(buf, "{}}}", padding)?;
                }
                Toplevel::Supports { params, body } => {
                    if should_emit_newline {
                        should_emit_newline = false;
                        writeln!(buf)?;
                    }

                    if params.is_empty() {
                        write!(buf, "{}@supports", padding)?;
                    } else {
                        write!(buf, "{}@supports {}", padding, params)?;
                    }

                    if body.is_empty() {
                        writeln!(buf, ";")?;
                        continue;
                    } else {
                        writeln!(buf, " {{")?;
                    }

                    Css::from_stmts(body, extender)?._inner_pretty_print(
                        buf,
                        map,
                        extender,
                        nesting + 1,
                    )?;
                    writeln!(buf, "{}}}", padding)?;
                }
                Toplevel::Media { params, body } => {
                    if body.is_empty() {
                        continue;
                    }
                    if should_emit_newline {
                        should_emit_newline = false;
                        writeln!(buf)?;
                    }
                    writeln!(buf, "{}@media {} {{", padding, params)?;
                    Css::from_stmts(body, extender)?._inner_pretty_print(
                        buf,
                        map,
                        extender,
                        nesting + 1,
                    )?;
                    writeln!(buf, "{}}}", padding)?;
                }
                Toplevel::Style(s) => {
                    writeln!(buf, "{}{}", padding, s.to_string()?)?;
                }
                Toplevel::Newline => {
                    if has_written {
                        should_emit_newline = true;
                    }
                    continue;
                }
            }
        }
        Ok(())
    }
}
