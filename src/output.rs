//! # Convert from SCSS AST to CSS
use std::{io::Write, mem};

use codemap::CodeMap;

use crate::{
    atrule::{
        keyframes::{Keyframes, KeyframesRuleSet, KeyframesSelector},
        media::MediaRule,
        SupportsRule, UnknownAtRule,
    },
    error::SassResult,
    parse::Stmt,
    selector::{ComplexSelector, ComplexSelectorComponent, Selector},
    style::Style,
    OutputStyle,
};

#[derive(Debug, Clone)]
struct ToplevelUnknownAtRule {
    name: String,
    params: String,
    body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
enum Toplevel {
    RuleSet {
        selector: Selector,
        body: Vec<BlockEntry>,
        is_group_end: bool,
    },
    MultilineComment(String),
    UnknownAtRule(Box<ToplevelUnknownAtRule>),
    Keyframes(Box<Keyframes>),
    KeyframesRuleSet(Vec<KeyframesSelector>, Vec<BlockEntry>),
    Media {
        query: String,
        body: Vec<Stmt>,
        inside_rule: bool,
        is_group_end: bool,
    },
    Supports {
        params: String,
        body: Vec<Stmt>,
        inside_rule: bool,
        is_group_end: bool,
    },
    // todo: do we actually need a toplevel style variant?
    Style(Style),
    Import(String),
    Empty,
}

impl Toplevel {
    pub fn is_invisible(&self) -> bool {
        match self {
            Toplevel::RuleSet { selector, body, .. } => selector.is_empty() || body.is_empty(),
            Toplevel::Media { body, .. } => body.is_empty(),
            Toplevel::Empty => true,
            _ => false,
        }
    }

    pub fn is_group_end(&self) -> bool {
        match self {
            Toplevel::RuleSet { is_group_end, .. } => *is_group_end,
            Toplevel::Media {
                inside_rule,
                is_group_end,
                ..
            }
            | Toplevel::Supports {
                inside_rule,
                is_group_end,
                ..
            } => *inside_rule && *is_group_end,
            _ => false,
        }
    }
}

fn set_group_end(group: &mut [Toplevel]) {
    match group.last_mut() {
        Some(Toplevel::RuleSet { is_group_end, .. })
        | Some(Toplevel::Supports { is_group_end, .. })
        | Some(Toplevel::Media { is_group_end, .. }) => {
            *is_group_end = true;
        }
        _ => {}
    }
}

#[derive(Debug, Clone)]
enum BlockEntry {
    Style(Style),
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
    const fn new_rule(selector: Selector, is_group_end: bool) -> Self {
        Toplevel::RuleSet {
            selector,
            is_group_end,
            body: Vec::new(),
        }
    }

    fn new_keyframes_rule(selector: Vec<KeyframesSelector>) -> Self {
        Toplevel::KeyframesRuleSet(selector, Vec::new())
    }

    fn push_style(&mut self, s: Style) {
        if s.value.is_null() {
            return;
        }

        if let Toplevel::RuleSet { body, .. } | Toplevel::KeyframesRuleSet(_, body) = self {
            body.push(BlockEntry::Style(s));
        } else {
            panic!();
        }
    }

    fn push_comment(&mut self, s: String) {
        if let Toplevel::RuleSet { body, .. } | Toplevel::KeyframesRuleSet(_, body) = self {
            body.push(BlockEntry::MultilineComment(s));
        } else {
            panic!();
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Css {
    blocks: Vec<Toplevel>,
    at_rule_context: AtRuleContext,
    allows_charset: bool,
    plain_imports: Vec<Toplevel>,
}

impl Css {
    pub const fn new(at_rule_context: AtRuleContext, allows_charset: bool) -> Self {
        Css {
            blocks: Vec::new(),
            at_rule_context,
            allows_charset,
            plain_imports: Vec::new(),
        }
    }

    pub(crate) fn from_stmts(
        s: Vec<Stmt>,
        at_rule_context: AtRuleContext,
        allows_charset: bool,
    ) -> SassResult<Self> {
        Css::new(at_rule_context, allows_charset).parse_stylesheet(s)
    }

    fn parse_stmt(&mut self, stmt: Stmt) -> SassResult<Vec<Toplevel>> {
        Ok(match stmt {
            Stmt::RuleSet { selector, body } => {
                if body.is_empty() {
                    return Ok(vec![Toplevel::Empty]);
                }

                let selector = selector.into_selector().remove_placeholders();

                if selector.is_empty() {
                    return Ok(vec![Toplevel::Empty]);
                }

                let mut vals = vec![Toplevel::new_rule(selector, false)];

                for rule in body {
                    match rule {
                        Stmt::RuleSet { .. } => vals.extend(self.parse_stmt(rule)?),
                        Stmt::Style(s) => vals.first_mut().unwrap().push_style(s),
                        Stmt::Comment(s) => vals.first_mut().unwrap().push_comment(s),
                        Stmt::Media(m) => {
                            let MediaRule { query, body, .. } = *m;
                            vals.push(Toplevel::Media {
                                query,
                                body,
                                inside_rule: true,
                                is_group_end: false,
                            });
                        }
                        Stmt::Supports(s) => {
                            let SupportsRule { params, body } = *s;
                            vals.push(Toplevel::Supports {
                                params,
                                body,
                                inside_rule: true,
                                is_group_end: false,
                            });
                        }
                        Stmt::UnknownAtRule(u) => {
                            let UnknownAtRule {
                                params, body, name, ..
                            } = *u;
                            vals.push(Toplevel::UnknownAtRule(Box::new(ToplevelUnknownAtRule {
                                name,
                                params,
                                body,
                            })));
                        }
                        Stmt::Return(..) => unreachable!(),
                        Stmt::AtRoot { body } => {
                            body.into_iter().try_for_each(|r| -> SassResult<()> {
                                let mut stmts = self.parse_stmt(r)?;

                                set_group_end(&mut stmts);

                                vals.append(&mut stmts);
                                Ok(())
                            })?;
                        }
                        Stmt::Keyframes(k) => {
                            let Keyframes { rule, name, body } = *k;
                            vals.push(Toplevel::Keyframes(Box::new(Keyframes {
                                rule,
                                name,
                                body,
                            })));
                        }
                        k @ Stmt::KeyframesRuleSet(..) => {
                            unreachable!("@keyframes ruleset {:?}", k);
                        }
                        Stmt::Import(s) => self.plain_imports.push(Toplevel::Import(s)),
                    };
                }
                vals
            }
            Stmt::Comment(s) => vec![Toplevel::MultilineComment(s)],
            Stmt::Import(s) => {
                self.plain_imports.push(Toplevel::Import(s));
                Vec::new()
            }
            Stmt::Style(s) => vec![Toplevel::Style(s)],
            Stmt::Media(m) => {
                let MediaRule { query, body, .. } = *m;
                vec![Toplevel::Media {
                    query,
                    body,
                    inside_rule: false,
                    is_group_end: false,
                }]
            }
            Stmt::Supports(s) => {
                let SupportsRule { params, body } = *s;
                vec![Toplevel::Supports {
                    params,
                    body,
                    inside_rule: false,
                    is_group_end: false,
                }]
            }
            Stmt::UnknownAtRule(u) => {
                let UnknownAtRule {
                    params, body, name, ..
                } = *u;
                vec![Toplevel::UnknownAtRule(Box::new(ToplevelUnknownAtRule {
                    name,
                    params,
                    body,
                }))]
            }
            Stmt::Return(..) => unreachable!("@return: {:?}", stmt),
            Stmt::AtRoot { .. } => unreachable!("@at-root: {:?}", stmt),
            Stmt::Keyframes(k) => vec![Toplevel::Keyframes(k)],
            Stmt::KeyframesRuleSet(k) => {
                let KeyframesRuleSet { body, selector } = *k;
                if body.is_empty() {
                    return Ok(Vec::new());
                }
                let mut vals = vec![Toplevel::new_keyframes_rule(selector)];
                for rule in body {
                    match rule {
                        Stmt::Style(s) => vals.first_mut().unwrap().push_style(s),
                        Stmt::KeyframesRuleSet(..) => vals.extend(self.parse_stmt(rule)?),
                        _ => todo!(),
                    }
                }
                vals
            }
        })
    }

    fn parse_stylesheet(mut self, stmts: Vec<Stmt>) -> SassResult<Css> {
        for stmt in stmts {
            let mut v = self.parse_stmt(stmt)?;

            set_group_end(&mut v);

            self.blocks.extend(v);
        }

        // move plain imports to top of file
        self.plain_imports.append(&mut self.blocks);
        mem::swap(&mut self.plain_imports, &mut self.blocks);

        Ok(self)
    }

    pub fn pretty_print(self, map: &CodeMap, style: OutputStyle) -> SassResult<String> {
        let mut buf = Vec::new();
        let allows_charset = self.allows_charset;
        match style {
            OutputStyle::Compressed => {
                CompressedFormatter::default().write_css(&mut buf, self, map)?;
            }
            OutputStyle::Expanded => {
                ExpandedFormatter::default().write_css(&mut buf, self, map)?;

                if !buf.is_empty() {
                    writeln!(buf)?;
                }
            }
        }

        // TODO: check for this before writing
        let show_charset = allows_charset && buf.iter().any(|s| !s.is_ascii());
        let out = unsafe { String::from_utf8_unchecked(buf) };
        Ok(if show_charset {
            match style {
                OutputStyle::Compressed => format!("\u{FEFF}{}", out),
                OutputStyle::Expanded => format!("@charset \"UTF-8\";\n{}", out),
            }
        } else {
            out
        })
    }
}

trait Formatter {
    fn write_css(&mut self, buf: &mut Vec<u8>, css: Css, map: &CodeMap) -> SassResult<()>;
}

#[derive(Debug, Default)]
struct CompressedFormatter {}

impl Formatter for CompressedFormatter {
    fn write_css(&mut self, buf: &mut Vec<u8>, css: Css, map: &CodeMap) -> SassResult<()> {
        for block in css.blocks {
            match block {
                Toplevel::RuleSet { selector, body, .. } => {
                    if body.is_empty() {
                        continue;
                    }

                    let mut complexes = selector.0.components.iter().filter(|c| !c.is_invisible());
                    if let Some(complex) = complexes.next() {
                        self.write_complex(buf, complex)?;
                    }
                    for complex in complexes {
                        write!(buf, ",")?;
                        self.write_complex(buf, complex)?;
                    }

                    write!(buf, "{{")?;
                    self.write_block_entry(buf, &body)?;
                    write!(buf, "}}")?;
                }
                Toplevel::KeyframesRuleSet(selectors, styles) => {
                    if styles.is_empty() {
                        continue;
                    }

                    let mut selectors = selectors.iter();
                    if let Some(selector) = selectors.next() {
                        write!(buf, "{}", selector)?;
                    }
                    for selector in selectors {
                        write!(buf, ",{}", selector)?;
                    }

                    write!(buf, "{{")?;
                    self.write_block_entry(buf, &styles)?;
                    write!(buf, "}}")?;
                }
                Toplevel::Empty | Toplevel::MultilineComment(..) => continue,
                Toplevel::Import(s) => {
                    write!(buf, "@import {};", s)?;
                }
                Toplevel::UnknownAtRule(u) => {
                    let ToplevelUnknownAtRule { params, name, body } = *u;

                    if params.is_empty() {
                        write!(buf, "@{}", name)?;
                    } else {
                        write!(buf, "@{} {}", name, params)?;
                    }

                    if body.is_empty() {
                        write!(buf, ";")?;
                        continue;
                    }

                    write!(buf, "{{")?;
                    let css = Css::from_stmts(body, AtRuleContext::Unknown, css.allows_charset)?;
                    self.write_css(buf, css, map)?;
                    write!(buf, "}}")?;
                }
                Toplevel::Keyframes(k) => {
                    let Keyframes { rule, name, body } = *k;

                    write!(buf, "@{}", rule)?;

                    if !name.is_empty() {
                        write!(buf, " {}", name)?;
                    }

                    if body.is_empty() {
                        write!(buf, "{{}}")?;
                        continue;
                    }

                    write!(buf, "{{")?;
                    let css = Css::from_stmts(body, AtRuleContext::Keyframes, css.allows_charset)?;
                    self.write_css(buf, css, map)?;
                    write!(buf, "}}")?;
                }
                Toplevel::Supports { params, body, .. } => {
                    if params.is_empty() {
                        write!(buf, "@supports")?;
                    } else {
                        write!(buf, "@supports {}", params)?;
                    }

                    if body.is_empty() {
                        write!(buf, ";")?;
                        continue;
                    }

                    write!(buf, "{{")?;
                    let css = Css::from_stmts(body, AtRuleContext::Supports, css.allows_charset)?;
                    self.write_css(buf, css, map)?;
                    write!(buf, "}}")?;
                }
                Toplevel::Media { query, body, .. } => {
                    if body.is_empty() {
                        continue;
                    }

                    write!(buf, "@media {}{{", query)?;
                    let css = Css::from_stmts(body, AtRuleContext::Media, css.allows_charset)?;
                    self.write_css(buf, css, map)?;
                    write!(buf, "}}")?;
                }
                Toplevel::Style(style) => {
                    let value = style.value.node.to_css_string(style.value.span)?;
                    write!(buf, "{}:{};", style.property, value)?;
                }
            }
        }
        Ok(())
    }
}

// this could be a trait implemented on value itself
#[allow(clippy::unused_self)]
impl CompressedFormatter {
    fn write_complex(&self, buf: &mut Vec<u8>, complex: &ComplexSelector) -> SassResult<()> {
        let mut was_compound = false;
        for component in &complex.components {
            match component {
                ComplexSelectorComponent::Compound(c) if was_compound => write!(buf, " {}", c)?,
                ComplexSelectorComponent::Compound(c) => write!(buf, "{}", c)?,
                ComplexSelectorComponent::Combinator(c) => write!(buf, "{}", c)?,
            }
            was_compound = matches!(component, ComplexSelectorComponent::Compound(_));
        }
        Ok(())
    }

    fn write_block_entry(&self, buf: &mut Vec<u8>, styles: &[BlockEntry]) -> SassResult<()> {
        let mut styles = styles.iter();

        for style in &mut styles {
            match style {
                BlockEntry::Style(s) => {
                    let value = s.value.node.to_css_string(s.value.span)?;
                    write!(buf, "{}:{}", s.property, value)?;
                    break;
                }
                BlockEntry::MultilineComment(..) => continue,
            }
        }

        for style in styles {
            match style {
                BlockEntry::Style(s) => {
                    let value = s.value.node.to_css_string(s.value.span)?;

                    write!(buf, ";{}:{}", s.property, value)?;
                }
                BlockEntry::MultilineComment(..) => continue,
            }
        }
        Ok(())
    }
}

#[derive(Debug, Default)]
struct ExpandedFormatter {
    nesting: usize,
}

#[derive(Clone, Copy)]
struct Previous {
    is_group_end: bool,
}

/// What kind of @-rule are we currently inside
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AtRuleContext {
    Media,
    Supports,
    Keyframes,
    Unknown,
    None,
}

impl Formatter for ExpandedFormatter {
    fn write_css(&mut self, buf: &mut Vec<u8>, css: Css, map: &CodeMap) -> SassResult<()> {
        let padding = "  ".repeat(self.nesting);
        self.nesting += 1;

        let mut prev: Option<Previous> = None;

        for block in css.blocks {
            if block.is_invisible() {
                continue;
            }

            let is_group_end = block.is_group_end();

            if let Some(prev) = prev {
                writeln!(buf)?;

                if (prev.is_group_end && css.at_rule_context == AtRuleContext::None)
                    || css.at_rule_context == AtRuleContext::Supports
                {
                    writeln!(buf)?;
                }
            }

            match block {
                Toplevel::Empty => continue,
                Toplevel::RuleSet { selector, body, .. } => {
                    writeln!(buf, "{}{} {{", padding, selector)?;

                    for style in body {
                        writeln!(buf, "{}  {}", padding, style.to_string()?)?;
                    }

                    write!(buf, "{}}}", padding)?;
                }
                Toplevel::KeyframesRuleSet(selector, body) => {
                    if body.is_empty() {
                        continue;
                    }

                    writeln!(
                        buf,
                        "{}{} {{",
                        padding,
                        selector
                            .into_iter()
                            .map(|s| s.to_string())
                            .collect::<Vec<String>>()
                            .join(", ")
                    )?;
                    for style in body {
                        writeln!(buf, "{}  {}", padding, style.to_string()?)?;
                    }
                    write!(buf, "{}}}", padding)?;
                }
                Toplevel::MultilineComment(s) => {
                    write!(buf, "{}/*{}*/", padding, s)?;
                }
                Toplevel::Import(s) => {
                    write!(buf, "{}@import {};", padding, s)?;
                }
                Toplevel::UnknownAtRule(u) => {
                    let ToplevelUnknownAtRule { params, name, body } = *u;

                    if params.is_empty() {
                        write!(buf, "{}@{}", padding, name)?;
                    } else {
                        write!(buf, "{}@{} {}", padding, name, params)?;
                    }

                    if body.is_empty() {
                        write!(buf, ";")?;
                        prev = Some(Previous { is_group_end });
                        continue;
                    }

                    writeln!(buf, " {{")?;
                    let css = Css::from_stmts(body, AtRuleContext::Unknown, css.allows_charset)?;
                    self.write_css(buf, css, map)?;
                    write!(buf, "\n{}}}", padding)?;
                }
                Toplevel::Keyframes(k) => {
                    let Keyframes { rule, name, body } = *k;

                    write!(buf, "{}@{}", padding, rule)?;

                    if !name.is_empty() {
                        write!(buf, " {}", name)?;
                    }

                    if body.is_empty() {
                        write!(buf, " {{}}")?;
                        prev = Some(Previous { is_group_end });
                        continue;
                    }

                    writeln!(buf, " {{")?;
                    let css = Css::from_stmts(body, AtRuleContext::Keyframes, css.allows_charset)?;
                    self.write_css(buf, css, map)?;
                    write!(buf, "\n{}}}", padding)?;
                }
                Toplevel::Supports {
                    params,
                    body,
                    inside_rule,
                    ..
                } => {
                    if params.is_empty() {
                        write!(buf, "{}@supports", padding)?;
                    } else {
                        write!(buf, "{}@supports {}", padding, params)?;
                    }

                    if body.is_empty() {
                        write!(buf, ";")?;
                        prev = Some(Previous { is_group_end });
                        continue;
                    }

                    writeln!(buf, " {{")?;
                    let css = Css::from_stmts(
                        body,
                        if inside_rule {
                            AtRuleContext::Supports
                        } else {
                            AtRuleContext::None
                        },
                        css.allows_charset,
                    )?;
                    self.write_css(buf, css, map)?;
                    write!(buf, "\n{}}}", padding)?;
                }
                Toplevel::Media {
                    query,
                    body,
                    inside_rule,
                    ..
                } => {
                    writeln!(buf, "{}@media {} {{", padding, query)?;
                    let css = Css::from_stmts(
                        body,
                        if inside_rule {
                            AtRuleContext::Media
                        } else {
                            AtRuleContext::None
                        },
                        css.allows_charset,
                    )?;
                    self.write_css(buf, css, map)?;
                    write!(buf, "\n{}}}", padding)?;
                }
                Toplevel::Style(s) => {
                    write!(buf, "{}{}", padding, s.to_string()?)?;
                }
            }

            prev = Some(Previous { is_group_end });
        }

        self.nesting -= 1;

        Ok(())
    }
}
