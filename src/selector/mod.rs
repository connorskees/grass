use std::fmt::{self, Display, Write};
use std::iter::Peekable;
use std::string::ToString;

use crate::error::SassResult;
use crate::lexer::Lexer;
use crate::scope::Scope;
use crate::utils::{
    devour_whitespace, devour_whitespace_or_comment, eat_ident, eat_ident_no_interpolation,
    parse_interpolation, IsWhitespace,
};
use crate::Token;

use attribute::Attribute;

mod attribute;

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Selector(pub Vec<SelectorKind>);

impl Selector {
    pub const fn new() -> Selector {
        Selector(Vec::new())
    }
}

impl Display for Selector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut iter = self.0.iter().peekable();

        while let Some(s) = iter.next() {
            match s {
                SelectorKind::Whitespace => continue,
                SelectorKind::Attribute(_)
                | SelectorKind::Pseudo(_)
                | SelectorKind::PseudoElement(_)
                | SelectorKind::PseudoParen(..)
                | SelectorKind::Class
                | SelectorKind::Id
                | SelectorKind::Universal
                | SelectorKind::InterpolatedSuper
                | SelectorKind::Element(_) => {
                    write!(f, "{}", s)?;
                    if devour_whitespace(&mut iter) {
                        match iter.peek() {
                            Some(SelectorKind::Attribute(_))
                            | Some(SelectorKind::Pseudo(_))
                            | Some(SelectorKind::PseudoElement(_))
                            | Some(SelectorKind::PseudoParen(..))
                            | Some(SelectorKind::Class)
                            | Some(SelectorKind::Id)
                            | Some(SelectorKind::Universal)
                            | Some(SelectorKind::InterpolatedSuper)
                            | Some(SelectorKind::Element(_)) => {
                                write!(f, " ")?;
                            }
                            _ => {}
                        }
                    }
                }
                SelectorKind::Multiple => {
                    devour_whitespace(&mut iter);
                    while let Some(sel) = iter.peek() {
                        if sel != &&SelectorKind::Multiple {
                            write!(f, ",")?;
                            if sel == &&SelectorKind::Newline {
                                iter.next();
                                f.write_char('\n')?;
                            } else {
                                f.write_char(' ')?;
                            }
                            break;
                        }
                        iter.next();
                        devour_whitespace(&mut iter);
                    }
                    while let Some(sel) = iter.peek() {
                        if sel != &&SelectorKind::Multiple
                            && sel != &&SelectorKind::Newline
                            && !sel.is_whitespace()
                        {
                            break;
                        }
                        iter.next();
                    }
                }
                _ => write!(f, "{}", s)?,
            }
        }
        write!(f, "")
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum SelectorKind {
    /// An element selector: `button`
    Element(String),
    /// An id selector: `#footer`
    Id,
    /// A single class selector: `.button-active`
    Class,
    /// A universal selector: `*`
    Universal,
    /// Multiple unrelated selectors: `button, .active`
    Multiple,
    /// Newline (significant if after `SelectorKind::Multiple`)
    Newline,
    /// Select all immediate children: `ul > li`
    ImmediateChild,
    /// Select all elements immediately following: `div + p`
    Following,
    /// Select elements preceeded by: `p ~ ul`
    Preceding,
    /// Select elements with attribute: `html[lang|=en]`
    Attribute(Attribute),
    /// Pseudo selector: `:hover`
    Pseudo(String),
    /// Pseudo element selector: `::before`
    PseudoElement(String),
    /// Pseudo selector with additional parens: `:any(h1, h2, h3, h4, h5, h6)`
    PseudoParen(String, String),
    /// Use the super selector: `&.red`
    Super,
    /// Super selector in an interpolated context: `a #{&}`
    InterpolatedSuper,
    /// Placeholder selector: `%alert`
    Placeholder,
    Whitespace,
}

impl IsWhitespace for SelectorKind {
    fn is_whitespace(&self) -> bool {
        self == &Self::Whitespace
    }
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
            SelectorKind::Id => write!(f, "#"),
            SelectorKind::Class => write!(f, "."),
            SelectorKind::Universal => write!(f, "*"),
            SelectorKind::Whitespace => write!(f, " "),
            SelectorKind::Multiple => write!(f, ", "),
            SelectorKind::Newline => writeln!(f),
            SelectorKind::ImmediateChild => write!(f, " > "),
            SelectorKind::Following => write!(f, " + "),
            SelectorKind::Preceding => write!(f, " ~ "),
            SelectorKind::Attribute(attr) => write!(f, "{}", attr),
            SelectorKind::Pseudo(s) => write!(f, ":{}", s),
            SelectorKind::PseudoElement(s) => write!(f, "::{}", s),
            SelectorKind::PseudoParen(s, val) => write!(f, ":{}({})", s, val),
            SelectorKind::Super | SelectorKind::InterpolatedSuper => write!(f, ""),
            SelectorKind::Placeholder => write!(f, "%"),
        }
    }
}

struct SelectorParser<'a> {
    scope: &'a Scope,
    super_selector: &'a Selector,
    selectors: Vec<SelectorKind>,
    is_interpolated: bool,
}

impl<'a> SelectorParser<'a> {
    const fn new(scope: &'a Scope, super_selector: &'a Selector) -> SelectorParser<'a> {
        SelectorParser {
            scope,
            super_selector,
            selectors: Vec::new(),
            is_interpolated: false,
        }
    }

    fn all_selectors<I: Iterator<Item = Token>>(
        mut self,
        tokens: &'a mut Peekable<I>,
    ) -> SassResult<Selector> {
        self.tokens_to_selectors(tokens)?;
        // remove trailing whitespace
        while let Some(x) = self.selectors.pop() {
            if x != SelectorKind::Whitespace {
                self.selectors.push(x);
                break;
            }
        }
        Ok(Selector(self.selectors))
    }

    fn consume_pseudo_selector<I: Iterator<Item = Token>>(
        &mut self,
        tokens: &'_ mut Peekable<I>,
    ) -> SassResult<()> {
        if let Some(tok) = tokens.next() {
            match tok.kind {
                v @ 'a'..='z' | v @ 'A'..='Z' | v @ '-' | v @ '_' => {
                    let s = format!(
                        "{}{}",
                        v,
                        eat_ident(tokens, self.scope, self.super_selector)?
                    );
                    if let Some(Token { kind: '(', .. }) = tokens.peek() {
                        tokens.next();
                        devour_whitespace(tokens);
                        let mut toks = String::new();
                        while let Some(Token { kind, .. }) = tokens.peek() {
                            if kind == &')' {
                                tokens.next();
                                break;
                            }
                            let tok = tokens.next().unwrap();
                            toks.push_str(&tok.kind.to_string());
                            if devour_whitespace(tokens) {
                                toks.push(' ');
                            }
                        }
                        self.selectors
                            .push(SelectorKind::PseudoParen(s, toks.trim_end().to_owned()))
                    } else {
                        self.selectors.push(SelectorKind::Pseudo(s))
                    }
                }
                ':' => {
                    let s = eat_ident(tokens, self.scope, self.super_selector)?;
                    self.selectors.push(SelectorKind::PseudoElement(s))
                }
                _ => return Err("Expected identifier.".into()),
            }
        }
        Ok(())
    }

    fn tokens_to_selectors<I: Iterator<Item = Token>>(
        &mut self,
        tokens: &'_ mut Peekable<I>,
    ) -> SassResult<()> {
        while tokens.peek().is_some() {
            self.consume_selector(tokens)?;
        }
        Ok(())
    }

    fn consume_selector<I: Iterator<Item = Token>>(
        &mut self,
        tokens: &'_ mut Peekable<I>,
    ) -> SassResult<()> {
        if devour_whitespace_or_comment(tokens)? {
            if let Some(Token { kind: ',', .. }) = tokens.peek() {
                tokens.next();
                self.selectors.push(SelectorKind::Multiple);
                return Ok(());
            }
            self.selectors.push(SelectorKind::Whitespace);
            return Ok(());
        }
        if let Some(Token { kind, .. }) = tokens.peek() {
            match kind {
                'a'..='z' | 'A'..='Z' | '-' | '_' | '0'..='9' | '\\' => {
                    let s = eat_ident_no_interpolation(tokens)?;
                    self.selectors.push(SelectorKind::Element(s))
                }
                '.' => {
                    tokens.next();
                    self.selectors.push(SelectorKind::Class)
                }
                '#' => {
                    tokens.next();
                    if tokens.peek().is_some() && tokens.peek().unwrap().kind == '{' {
                        tokens.next();
                        self.is_interpolated = true;
                        self.tokens_to_selectors(
                            &mut Lexer::new(
                                &parse_interpolation(tokens, self.scope, self.super_selector)?
                                    .to_string(),
                            )
                            .peekable(),
                        )?;
                        self.is_interpolated = false;
                    } else {
                        self.selectors.push(SelectorKind::Id)
                    }
                }
                ':' => {
                    tokens.next();
                    self.consume_pseudo_selector(tokens)?
                }
                ',' => {
                    tokens.next();
                    self.selectors.push(SelectorKind::Multiple);
                    if tokens.peek().unwrap().kind == '\n' {
                        self.selectors.push(SelectorKind::Newline);
                        devour_whitespace(tokens);
                    }
                }
                '>' => {
                    tokens.next();
                    self.selectors.push(SelectorKind::ImmediateChild)
                }
                '+' => {
                    tokens.next();
                    self.selectors.push(SelectorKind::Following)
                }
                '~' => {
                    tokens.next();
                    self.selectors.push(SelectorKind::Preceding)
                }
                '*' => {
                    tokens.next();
                    self.selectors.push(SelectorKind::Universal)
                }
                '%' => {
                    tokens.next();
                    self.selectors.push(SelectorKind::Placeholder)
                }
                '&' => self.selectors.push(if self.is_interpolated {
                    tokens.next();
                    SelectorKind::InterpolatedSuper
                } else {
                    tokens.next();
                    SelectorKind::Super
                }),
                '[' => {
                    tokens.next();
                    self.selectors.push(Attribute::from_tokens(
                        tokens,
                        self.scope,
                        self.super_selector,
                    )?)
                }
                c if c.is_control() => {
                    return Err("expected selector.".into());
                }
                '`' => return Err("expected selector.".into()),
                _ => todo!("unimplemented selector"),
            };
        }
        Ok(())
    }
}

impl Selector {
    pub fn from_tokens<'a, I: Iterator<Item = Token>>(
        tokens: &'a mut Peekable<I>,
        scope: &'a Scope,
        super_selector: &'a Selector,
    ) -> SassResult<Selector> {
        SelectorParser::new(scope, super_selector).all_selectors(tokens)
    }

    pub fn zip(&self, other: &Selector) -> Selector {
        if self.0.is_empty() {
            return Selector(other.0.clone());
        } else if other.0.is_empty() {
            return self.clone();
        }
        let mut rules: Vec<SelectorKind> = Vec::with_capacity(self.0.len() + other.0.len());
        let sel1_split: Vec<&[SelectorKind]> =
            self.0.split(|sel| sel == &SelectorKind::Multiple).collect();
        let sel2_split: Vec<&[SelectorKind]> = other
            .0
            .split(|sel| sel == &SelectorKind::Multiple)
            .collect();
        let len1 = sel1_split.len();
        let len2 = sel2_split.len();
        for (idx, sel1) in sel1_split.into_iter().enumerate() {
            for (idx2, sel2) in sel2_split.iter().enumerate() {
                let mut this_selector: Vec<SelectorKind> = Vec::with_capacity(other.0.len());
                let mut found_super = false;

                for sel in *sel2 {
                    if sel == &SelectorKind::Super {
                        this_selector.extend(sel1.to_vec());
                        found_super = true;
                    } else if sel == &SelectorKind::InterpolatedSuper {
                        this_selector.extend(sel1.to_vec());
                    } else {
                        this_selector.push(sel.clone());
                    }
                }

                if !found_super {
                    rules.extend(sel1.to_vec());
                    rules.push(SelectorKind::Whitespace);
                }
                rules.extend(this_selector);

                if !(idx + 1 == len1 && idx2 + 1 == len2) {
                    rules.push(SelectorKind::Multiple);
                }
            }
        }
        Selector(rules)
    }

    pub fn remove_placeholders(self) -> Selector {
        let mut selectors = Vec::with_capacity(self.0.len());
        let mut temp_sels = Vec::new();
        let mut found_placeholder = false;
        for sel in self.0 {
            match sel {
                SelectorKind::Placeholder => found_placeholder = true,
                SelectorKind::Multiple => {
                    temp_sels.push(SelectorKind::Multiple);
                    if !found_placeholder {
                        selectors.extend(temp_sels.clone());
                    }
                    temp_sels.clear();
                    found_placeholder = false;
                }
                _ => temp_sels.push(sel),
            }
        }
        if !found_placeholder {
            selectors.extend(temp_sels);
        }
        Selector(selectors)
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}
