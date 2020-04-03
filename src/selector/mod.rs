use std::fmt::{self, Display, Write};
use std::iter::Peekable;
use std::string::ToString;

use crate::error::SassResult;
use crate::lexer::Lexer;
use crate::scope::Scope;
use crate::utils::{
    devour_whitespace, eat_comment, eat_ident_no_interpolation, parse_interpolation,
    read_until_closing_paren, read_until_newline, IsWhitespace,
};
use crate::Token;

use attribute::Attribute;

mod attribute;

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Selector(Vec<SelectorPart>);

impl Selector {
    pub const fn new() -> Selector {
        Selector(Vec::new())
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct SelectorPart {
    pub inner: Vec<SelectorKind>,
    pub is_invisible: bool,
    pub has_newline: bool,
}

impl Display for SelectorPart {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        dbg!(&self);
        let mut iter = self.inner.iter().peekable();
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
            SelectorKind::Super => todo!(),
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
        toks: &mut Peekable<I>,
        scope: &Scope,
        super_selector: &Selector,
    ) -> SassResult<Selector> {
        let mut string = String::new();
        while let Some(tok) = toks.next() {
            match tok.kind {
                '#' => {
                    if toks.peek().is_some() && toks.peek().unwrap().kind == '{' {
                        toks.next();
                        string.push_str(
                            &parse_interpolation(toks, scope, super_selector)?.to_string(),
                        );
                    } else {
                        string.push('#');
                    }
                }
                ',' => {
                    while let Some(c) = string.pop() {
                        if c == ' ' || c == ',' {
                            continue;
                        } else {
                            string.push(c);
                            string.push(',');
                            break;
                        }
                    }
                }
                '/' => {
                    if toks.peek().is_none() {
                        return Err("Expected selector.".into());
                    } else if '*' == toks.peek().unwrap().kind {
                        toks.next();
                        eat_comment(toks, &Scope::new(), &Selector::new())?;
                    } else if '/' == toks.peek().unwrap().kind {
                        read_until_newline(toks);
                        devour_whitespace(toks);
                    } else {
                        return Err("Expected selector.".into());
                    }
                    string.push(' ');
                }
                c => string.push(c),
            }
        }

        while let Some(c) = string.pop() {
            if c == ' ' || c == ',' || c == '\t' {
                continue;
            } else {
                string.push(c);
                string.push(',');
                break;
            }
        }

        let mut inner = Vec::new();
        let mut is_invisible = false;
        let mut has_newline = false;
        let mut parts = Vec::new();

        // HACK: we re-lex here to get access to generic helper functions that
        // operate on `Token`s. Ideally, we would in the future not have
        // to do this, or at the very least retain the span information.
        let mut iter = Lexer::new(&string).peekable();

        while let Some(tok) = iter.peek() {
            inner.push(match tok.kind {
                _ if is_selector_name_char(tok.kind) => {
                    inner.push(SelectorKind::Element(eat_ident_no_interpolation(
                        &mut iter,
                    )?));
                    continue;
                }
                '&' => SelectorKind::Super,
                '.' => {
                    iter.next();
                    inner.push(SelectorKind::Class(eat_ident_no_interpolation(&mut iter)?));
                    continue;
                }
                '#' => {
                    iter.next();
                    inner.push(SelectorKind::Id(eat_ident_no_interpolation(&mut iter)?));
                    continue;
                }
                '%' => {
                    iter.next();
                    is_invisible = true;
                    inner.push(SelectorKind::Placeholder(eat_ident_no_interpolation(
                        &mut iter,
                    )?));
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
                        });
                        inner.clear();
                    }
                    is_invisible = false;
                    has_newline = false;
                    devour_whitespace(&mut iter);
                    continue;
                }
                '[' => {
                    iter.next();
                    inner.push(Attribute::from_tokens(&mut iter, scope, super_selector)?);
                    continue;
                }
                ':' => {
                    iter.next();
                    inner.push(Self::consume_pseudo_selector(
                        &mut iter,
                        scope,
                        super_selector,
                    )?);
                    continue;
                }
                c if c.is_whitespace() => {
                    if devour_whitespace(&mut iter) {
                        inner.push(SelectorKind::Whitespace);
                    }
                    continue;
                }
                _ => todo!(),
            });
            iter.next();
        }

        if !inner.is_empty() {
            parts.push(SelectorPart {
                inner,
                is_invisible,
                has_newline,
            });
        }

        Ok(Selector(parts))
    }

    fn consume_pseudo_selector<I: Iterator<Item = Token>>(
        toks: &mut Peekable<I>,
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
            let name = eat_ident_no_interpolation(toks)?;
            Ok(
                if toks.peek().is_some() && toks.peek().unwrap().kind == '(' {
                    toks.next();
                    let mut inner_toks = read_until_closing_paren(toks);
                    inner_toks.pop();
                    let inner = Selector::from_tokens(
                        &mut inner_toks.into_iter().peekable(),
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

    pub fn zip(&self, other: &Selector) -> Selector {
        if self.0.is_empty() {
            return Selector(other.0.clone());
        } else if other.0.is_empty() {
            return self.clone();
        }
        let mut rules = Vec::with_capacity(self.0.len());
        for sel1 in self.clone().0 {
            for sel2 in other.clone().0 {
                let mut this_selector: Vec<SelectorKind> = Vec::with_capacity(other.0.len());
                let mut found_super = false;

                for sel in sel2.inner {
                    if sel == SelectorKind::Super {
                        this_selector.extend(sel1.clone().inner);
                        found_super = true;
                    } else {
                        this_selector.push(sel.clone());
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
                    has_newline: sel1.has_newline || sel2.has_newline,
                });
            }
        }
        Selector(rules)
    }

    pub fn remove_placeholders(self) -> Selector {
        Selector(self.0.into_iter().filter(|s| !s.is_invisible).collect())
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}
