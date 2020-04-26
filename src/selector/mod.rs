use std::fmt::{self, Display, Write};

use peekmore::{PeekMore, PeekMoreIterator};

use crate::common::{Brackets, ListSeparator, QuoteKind};
use crate::error::SassResult;
use crate::scope::Scope;
use crate::utils::{
    devour_whitespace, eat_comment, eat_ident_no_interpolation, parse_interpolation,
    read_until_closing_paren, read_until_newline, IsWhitespace,
};
use crate::value::Value;
use crate::Token;

use attribute::Attribute;

mod attribute;

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Selector(Vec<SelectorPart>);

#[derive(Clone, Debug, Eq, PartialEq)]
struct SelectorPart {
    pub inner: Vec<SelectorKind>,
    pub is_invisible: bool,
    pub has_newline: bool,
    pub contains_super_selector: bool,
}

impl SelectorPart {
    pub fn into_value(&self) -> Value {
        let mut kinds = Vec::new();
        let mut this_kind = Vec::new();
        for kind in &self.inner {
            match kind {
                SelectorKind::Whitespace => {
                    if !this_kind.is_empty() {
                        kinds.push(SelectorPart {
                            inner: std::mem::take(&mut this_kind),
                            is_invisible: false,
                            has_newline: false,
                            contains_super_selector: false,
                        });
                    }
                }
                v => this_kind.push(v.clone()),
            }
        }
        if !this_kind.is_empty() {
            kinds.push(SelectorPart {
                inner: std::mem::take(&mut this_kind),
                is_invisible: false,
                has_newline: false,
                contains_super_selector: false,
            });
        }
        Value::List(
            kinds
                .iter()
                .map(|s| Value::Ident(s.to_string(), QuoteKind::None))
                .collect(),
            ListSeparator::Space,
            Brackets::None,
        )
    }
}

impl Display for SelectorPart {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut iter = self.inner.iter().peekmore();
        devour_whitespace(&mut iter);
        while let Some(s) = iter.next() {
            write!(f, "{}", s)?;
            if devour_whitespace(&mut iter) {
                match iter.peek() {
                    Some(SelectorKind::Universal)
                    | Some(SelectorKind::Following)
                    | Some(SelectorKind::ImmediateChild)
                    | Some(SelectorKind::Preceding) => {
                        f.write_char(' ')?;
                        write!(f, "{}", iter.next().unwrap())?;
                        devour_whitespace(&mut iter);
                        if iter.peek().is_some() {
                            f.write_char(' ')?;
                        }
                    }
                    Some(..) => {
                        f.write_char(' ')?;
                    }
                    None => break,
                }
            }
        }
        Ok(())
    }
}

impl Display for Selector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (idx, part) in self.0.iter().enumerate() {
            write!(f, "{}", part)?;
            if idx + 1 < self.0.len() {
                f.write_char(',')?;
                if part.has_newline {
                    f.write_char('\n')?;
                } else {
                    f.write_char(' ')?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum SelectorKind {
    /// Any string
    ///
    ///  `button`
    Element(String),

    /// An id selector
    ///
    /// `#`
    Id(String),

    /// A class selector
    ///
    /// `.`
    Class(String),

    /// A universal selector
    ///
    /// `*`
    Universal,

    /// Select all immediate children
    ///
    /// `>`
    ImmediateChild,

    /// Select all elements immediately following
    ///
    /// `+`
    Following,

    /// Select elements preceeded by
    ///
    /// `~`
    Preceding,

    /// Select elements with attribute
    ///
    /// `[lang|=en]`
    Attribute(Attribute),

    Super,

    /// Pseudo selector
    ///
    /// `:hover`
    Pseudo(String),

    /// Pseudo element selector
    ///
    /// `::before`
    PseudoElement(String),

    /// Pseudo selector with additional parens
    ///
    /// `:any(h1, h2, h3, h4, h5, h6)`
    PseudoParen(String, Selector),

    /// Placeholder selector
    ///
    /// `%`
    Placeholder(String),

    /// Denotes whitespace between two selectors
    Whitespace,
}

impl IsWhitespace for &SelectorKind {
    fn is_whitespace(&self) -> bool {
        self == &&SelectorKind::Whitespace
    }
}

impl Display for SelectorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SelectorKind::Element(s) => write!(f, "{}", s),
            SelectorKind::Id(s) => write!(f, "#{}", s),
            SelectorKind::Class(s) => write!(f, ".{}", s),
            SelectorKind::Universal => write!(f, "*"),
            SelectorKind::ImmediateChild => write!(f, ">"),
            SelectorKind::Following => write!(f, "+"),
            SelectorKind::Preceding => write!(f, "~"),
            SelectorKind::Attribute(attr) => write!(f, "{}", attr),
            SelectorKind::Pseudo(s) => write!(f, ":{}", s),
            SelectorKind::PseudoElement(s) => write!(f, "::{}", s),
            SelectorKind::PseudoParen(s, val) => write!(f, ":{}({})", s, val),
            SelectorKind::Placeholder(s) => write!(f, "%{}", s),
            SelectorKind::Super => unreachable!("& selector should not be emitted"),
            SelectorKind::Whitespace => f.write_char(' '),
        }
    }
}

fn is_selector_name_char(c: char) -> bool {
    c.is_ascii_alphanumeric()
        || c == '_'
        || c == '\\'
        || (!c.is_ascii() && !c.is_control())
        || c == '-'
}

impl Selector {
    pub fn from_tokens<I: Iterator<Item = Token>>(
        toks: &mut PeekMoreIterator<I>,
        scope: &Scope,
        super_selector: &Selector,
    ) -> SassResult<Selector> {
        let mut string = String::new();
        let mut span = if let Some(tok) = toks.peek() {
            tok.pos()
        } else {
            return Ok(Selector::new());
        };
        while let Some(tok) = toks.next() {
            span = span.merge(tok.pos());
            match tok.kind {
                '#' => {
                    if toks.peek().is_some() && toks.peek().unwrap().kind == '{' {
                        toks.next();
                        string.push_str(
                            &parse_interpolation(toks, scope, super_selector)?
                                .to_css_string(span)?,
                        );
                    } else {
                        string.push('#');
                    }
                }
                ',' => {
                    while let Some(c) = string.pop() {
                        if c == ' ' || c == ',' {
                            continue;
                        }
                        string.push(c);
                        string.push(',');
                        break;
                    }
                }
                '/' => {
                    if toks.peek().is_none() {
                        return Err(("Expected selector.", tok.pos()).into());
                    } else if '*' == toks.peek().unwrap().kind {
                        toks.next();
                        eat_comment(toks, &Scope::new(), &Selector::new())?;
                    } else if '/' == toks.peek().unwrap().kind {
                        read_until_newline(toks);
                        devour_whitespace(toks);
                    } else {
                        return Err(("Expected selector.", tok.pos()).into());
                    }
                    string.push(' ');
                }
                c => string.push(c),
            }
        }

        while let Some(c) = string.pop() {
            if c == ' ' || c == ',' || c == '\t' {
                continue;
            }
            string.push(c);
            break;
        }

        let mut inner = Vec::new();
        let mut is_invisible = false;
        let mut has_newline = false;
        let mut contains_super_selector = false;
        let mut parts = Vec::new();

        let mut sel_toks = Vec::new();

        let mut current_pos = 0;
        sel_toks.extend(string.chars().map(|x| {
            let len = x.len_utf8() as u64;
            let tok = Token::new(span.subspan(current_pos, current_pos + len), x);
            current_pos += len;
            tok
        }));

        let mut iter = sel_toks.into_iter().peekmore();

        while let Some(tok) = iter.peek() {
            inner.push(match tok.kind {
                _ if is_selector_name_char(tok.kind) => {
                    inner.push(SelectorKind::Element(
                        eat_ident_no_interpolation(&mut iter, false)?.node,
                    ));
                    continue;
                }
                '&' => {
                    contains_super_selector = true;
                    SelectorKind::Super
                }
                '.' => {
                    iter.next();
                    inner.push(SelectorKind::Class(
                        eat_ident_no_interpolation(&mut iter, false)?.node,
                    ));
                    continue;
                }
                '#' => {
                    iter.next();
                    inner.push(SelectorKind::Id(
                        eat_ident_no_interpolation(&mut iter, false)?.node,
                    ));
                    continue;
                }
                '%' => {
                    iter.next();
                    is_invisible = true;
                    inner.push(SelectorKind::Placeholder(
                        eat_ident_no_interpolation(&mut iter, false)?.node,
                    ));
                    continue;
                }
                '>' => SelectorKind::ImmediateChild,
                '+' => SelectorKind::Following,
                '~' => SelectorKind::Preceding,
                '*' => SelectorKind::Universal,
                ',' => {
                    iter.next();
                    if iter.peek().is_some() && iter.peek().unwrap().kind == '\n' {
                        has_newline = true;
                    }
                    if !inner.is_empty() {
                        parts.push(SelectorPart {
                            inner: inner.clone(),
                            is_invisible,
                            has_newline,
                            contains_super_selector,
                        });
                        inner.clear();
                    }
                    is_invisible = false;
                    has_newline = false;
                    contains_super_selector = false;
                    devour_whitespace(&mut iter);
                    continue;
                }
                '[' => {
                    let span = iter.next().unwrap().pos();
                    inner.push(Attribute::from_tokens(
                        &mut iter,
                        scope,
                        super_selector,
                        span,
                    )?);
                    continue;
                }
                ':' => {
                    iter.next();
                    let sel = Self::consume_pseudo_selector(&mut iter, scope, super_selector)?;
                    match &sel {
                        SelectorKind::PseudoParen(_, s) => {
                            if s.contains_super_selector() {
                                contains_super_selector = true;
                            }
                        }
                        _ => {}
                    }
                    inner.push(sel);
                    continue;
                }
                c if c.is_whitespace() => {
                    if devour_whitespace(&mut iter) {
                        inner.push(SelectorKind::Whitespace);
                    }
                    continue;
                }
                _ => return Err(("expected selector.", tok.pos()).into()),
            });
            iter.next();
        }

        if !inner.is_empty() {
            parts.push(SelectorPart {
                inner,
                is_invisible,
                has_newline,
                contains_super_selector,
            });
        }

        Ok(Selector(parts))
    }

    fn consume_pseudo_selector<I: Iterator<Item = Token>>(
        toks: &mut PeekMoreIterator<I>,
        scope: &Scope,
        super_selector: &Selector,
    ) -> SassResult<SelectorKind> {
        let is_pseudo_element = if toks.peek().unwrap().kind == ':' {
            toks.next();
            true
        } else {
            false
        };
        if is_selector_name_char(toks.peek().unwrap().kind) {
            let name = eat_ident_no_interpolation(toks, false)?.node;
            Ok(
                if toks.peek().is_some() && toks.peek().unwrap().kind == '(' {
                    toks.next();
                    let mut inner_toks = read_until_closing_paren(toks);
                    inner_toks.pop();
                    let inner = Selector::from_tokens(
                        &mut inner_toks.into_iter().peekmore(),
                        scope,
                        super_selector,
                    )?;
                    SelectorKind::PseudoParen(name, inner)
                } else if is_pseudo_element {
                    SelectorKind::PseudoElement(name)
                } else {
                    SelectorKind::Pseudo(name)
                },
            )
        } else {
            todo!()
        }
    }

    pub fn replace(super_selector: &Selector, this: Selector) -> Selector {
        if super_selector.0.is_empty() || this.0.is_empty() {
            return this;
        }
        let mut parts = Vec::with_capacity(super_selector.0.len());
        for (idx, part) in super_selector.clone().0.into_iter().enumerate() {
            let mut found_inner = false;
            for part2 in this.clone().0 {
                if !part2.contains_super_selector {
                    if idx == 0 {
                        parts.push(part2);
                    }
                    continue;
                }
                let mut kinds = Vec::new();
                for kind in part2.clone().inner {
                    match kind {
                        SelectorKind::Super => kinds.extend(part.inner.clone()),
                        SelectorKind::PseudoParen(name, inner) => {
                            if inner.contains_super_selector() {
                                found_inner = true;
                                kinds.push(SelectorKind::PseudoParen(
                                    name,
                                    Selector::replace(super_selector, inner),
                                ))
                            } else {
                                kinds.push(SelectorKind::PseudoParen(name, inner));
                            }
                        }
                        _ => kinds.push(kind),
                    }
                }
                parts.push(SelectorPart {
                    inner: kinds,
                    is_invisible: part2.is_invisible,
                    has_newline: part2.has_newline,
                    contains_super_selector: false,
                });
            }
            if found_inner {
                break;
            }
        }
        Selector(parts)
    }

    pub fn zip(&self, other: &Selector) -> Selector {
        if self.0.is_empty() {
            return Selector(other.0.clone());
        } else if other.0.is_empty() {
            return self.clone();
        }
        let mut rules = Vec::with_capacity(self.0.len());
        for sel1 in self.clone().0 {
            let mut found_inner = false;
            for sel2 in other.clone().0 {
                let mut this_selector: Vec<SelectorKind> = Vec::with_capacity(other.0.len());
                let mut found_super = false;

                for sel in sel2.inner {
                    match sel {
                        SelectorKind::Super => {
                            this_selector.extend(sel1.inner.clone());
                            found_super = true;
                        }
                        SelectorKind::PseudoParen(s, inner_selector) => {
                            if inner_selector.contains_super_selector() {
                                found_super = true;
                                found_inner = true;
                                this_selector.push(SelectorKind::PseudoParen(
                                    s,
                                    Selector::replace(self, inner_selector),
                                ))
                            } else {
                                this_selector.push(SelectorKind::PseudoParen(s, inner_selector));
                            }
                        }
                        _ => this_selector.push(sel),
                    }
                }

                if !found_super {
                    let mut x = std::mem::take(&mut this_selector);
                    let mut y = sel1.clone().inner;
                    y.push(SelectorKind::Whitespace);
                    y.append(&mut x);
                    this_selector = y;
                }
                rules.push(SelectorPart {
                    inner: this_selector,
                    is_invisible: sel1.is_invisible || sel2.is_invisible,
                    has_newline: (sel1.has_newline || sel2.has_newline) && !found_inner,
                    contains_super_selector: false,
                });
            }
            if found_inner {
                break;
            }
        }
        Selector(rules)
    }

    pub fn remove_placeholders(self) -> Selector {
        Selector(
            self.0
                .into_iter()
                .filter_map(|s| {
                    if s.is_invisible {
                        None
                    } else {
                        let mut inner = Vec::new();
                        let mut last_was_whitespace = false;
                        let len = s.inner.len();
                        for kind in s.inner {
                            match kind {
                                SelectorKind::PseudoParen(name, inner_selector) => {
                                    let inner_empty = inner_selector.is_empty();
                                    let removed_placeholders = inner_selector.remove_placeholders();
                                    if removed_placeholders.is_empty() && !inner_empty {
                                        if name.to_ascii_lowercase().as_str() == "not" {
                                            if last_was_whitespace || len == 1 {
                                                inner.push(SelectorKind::Universal);
                                            } else {
                                                continue;
                                            }
                                        } else {
                                            return None;
                                        }
                                    } else {
                                        inner.push(SelectorKind::PseudoParen(
                                            name,
                                            removed_placeholders,
                                        ));
                                    }
                                }
                                SelectorKind::Whitespace => {
                                    last_was_whitespace = true;
                                    inner.push(kind);
                                    continue;
                                }
                                _ => inner.push(kind),
                            }
                            last_was_whitespace = false;
                        }
                        Some(SelectorPart {
                            inner,
                            is_invisible: false,
                            has_newline: s.has_newline,
                            contains_super_selector: s.contains_super_selector,
                        })
                    }
                })
                .collect(),
        )
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub const fn new() -> Selector {
        Selector(Vec::new())
    }

    pub fn contains_super_selector(&self) -> bool {
        self.0.iter().any(|s| s.contains_super_selector)
    }

    pub fn into_value(&self) -> Value {
        Value::List(
            self.0.iter().map(SelectorPart::into_value).collect(),
            ListSeparator::Comma,
            Brackets::None,
        )
    }
}
