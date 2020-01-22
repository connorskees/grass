use crate::common::{Scope, Symbol};
use crate::utils::{devour_whitespace, eat_interpolation, IsWhitespace};
use crate::{Token, TokenKind};
use std::fmt::{self, Display};
use std::iter::Peekable;
use std::string::ToString;
use std::vec::IntoIter;

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Selector(pub Vec<SelectorKind>);

impl Display for Selector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut iter = self.0.iter().peekable();

        while let Some(s) = iter.next() {
            match s {
                SelectorKind::Whitespace => continue,
                SelectorKind::Attribute(_)
                | SelectorKind::Pseudo(_)
                | SelectorKind::PseudoParen(..)
                | SelectorKind::Class
                | SelectorKind::Id
                | SelectorKind::Universal
                | SelectorKind::Element(_) => {
                    write!(f, "{}", s)?;
                    if devour_whitespace(&mut iter) {
                        match iter.peek() {
                            Some(SelectorKind::Attribute(_))
                            | Some(SelectorKind::Pseudo(_))
                            | Some(SelectorKind::PseudoParen(..))
                            | Some(SelectorKind::Class)
                            | Some(SelectorKind::Id)
                            | Some(SelectorKind::Universal)
                            | Some(SelectorKind::Element(_)) => {
                                write!(f, " ")?;
                            }
                            _ => {}
                        }
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
    /// Pseudo selector with additional parens: `:any(h1, h2, h3, h4, h5, h6)`
    PseudoParen(String, Vec<TokenKind>),
    /// Use the super selector: `&.red`
    Super,
    /// Used to signify no selector (when there is no super_selector of a rule)
    None,
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
            SelectorKind::ImmediateChild => write!(f, " > "),
            SelectorKind::Following => write!(f, " + "),
            SelectorKind::Preceding => write!(f, " ~ "),
            SelectorKind::Attribute(attr) => write!(f, "{}", attr),
            SelectorKind::Pseudo(s) => write!(f, ":{}", s),
            SelectorKind::PseudoParen(s, toks) => write!(
                f,
                ":{}({})",
                s,
                toks.iter().map(ToString::to_string).collect::<String>()
            ),
            SelectorKind::Super | SelectorKind::None => write!(f, ""),
        }
    }
}

#[cfg(test)]
mod test_selector_display {
    use super::*;
    use SelectorKind::*;
    macro_rules! test_selector_display {

        ($func:ident, Selector($tok:tt), $output:literal) => {
            #[test]
            fn $func() {
                    assert_eq!(format!("{}", Selector(vec!$tok)), $output);
            }
        }
    }

    test_selector_display!(el, Selector((Element("a".to_string()))), "a");
    test_selector_display!(
        keeps_one_whitespace,
        Selector((
            Element("a".to_string()),
            Whitespace,
            Element("b".to_string()),
        )),
        "a b"
    );
    test_selector_display!(
        keeps_one_whitespace_with_two,
        Selector((
            Element("a".to_string()),
            Whitespace,
            Whitespace,
            Element("c".to_string()),
            Whitespace,
        )),
        "a c"
    );
    test_selector_display!(
        keeps_one_whitespace_with_three_els,
        Selector((
            Element("a".to_string()),
            Whitespace,
            Element("a".to_string()),
            Whitespace,
            Element("c".to_string()),
        )),
        "a a c"
    );
}

struct SelectorParser<'a> {
    scope: &'a Scope,
    selectors: Vec<SelectorKind>,
}

impl<'a> SelectorParser<'a> {
    const fn new(scope: &'a Scope) -> SelectorParser<'a> {
        SelectorParser {
            scope,
            selectors: Vec::new(),
        }
    }

    fn all_selectors(mut self, tokens: &'a mut Peekable<IntoIter<Token>>) -> Selector {
        self.tokens_to_selectors(tokens);
        // remove trailing whitespace
        while let Some(x) = self.selectors.pop() {
            if x != SelectorKind::Whitespace {
                self.selectors.push(x);
                break;
            }
        }
        Selector(self.selectors)
    }

    fn consume_pseudo_selector(&mut self, tokens: &'_ mut Peekable<IntoIter<Token>>) {
        if let Some(Token {
            kind: TokenKind::Ident(s),
            ..
        }) = tokens.next()
        {
            if let Some(Token {
                kind: TokenKind::Symbol(Symbol::OpenParen),
                ..
            }) = tokens.peek()
            {
                tokens.next();
                let mut toks = Vec::new();
                while let Some(Token { kind, .. }) = tokens.peek() {
                    if kind == &TokenKind::Symbol(Symbol::CloseParen) {
                        break;
                    }
                    let tok = tokens.next().unwrap();
                    toks.push(tok.kind);
                }
                tokens.next();
                self.selectors.push(SelectorKind::PseudoParen(s, toks))
            } else {
                self.selectors.push(SelectorKind::Pseudo(s))
            }
        } else {
            todo!("expected ident after `:` in selector")
        }
    }

    fn tokens_to_selectors(&mut self, tokens: &'_ mut Peekable<IntoIter<Token>>) {
        while tokens.peek().is_some() {
            self.consume_selector(tokens)
        }
    }

    fn consume_selector(&mut self, tokens: &'_ mut Peekable<IntoIter<Token>>) {
        if devour_whitespace(tokens) {
            if let Some(Token {
                kind: TokenKind::Symbol(Symbol::Comma),
                ..
            }) = tokens.peek()
            {
                tokens.next();
                self.selectors.push(SelectorKind::Multiple);
                return;
            }
            self.selectors.push(SelectorKind::Whitespace);
            return;
        }
        if let Some(Token { kind, .. }) = tokens.next() {
            match kind {
                TokenKind::Ident(ident) => self.selectors.push(SelectorKind::Element(ident)),
                TokenKind::Symbol(Symbol::Period) => self.selectors.push(SelectorKind::Class),
                TokenKind::Symbol(Symbol::Hash) => self.selectors.push(SelectorKind::Id),
                TokenKind::Symbol(Symbol::Colon) => self.consume_pseudo_selector(tokens),
                TokenKind::Symbol(Symbol::Comma) => self.selectors.push(SelectorKind::Multiple),
                TokenKind::Symbol(Symbol::Gt) => self.selectors.push(SelectorKind::ImmediateChild),
                TokenKind::Symbol(Symbol::Plus) => self.selectors.push(SelectorKind::Following),
                TokenKind::Symbol(Symbol::Tilde) => self.selectors.push(SelectorKind::Preceding),
                TokenKind::Symbol(Symbol::Mul) => self.selectors.push(SelectorKind::Universal),
                TokenKind::Symbol(Symbol::BitAnd) => self.selectors.push(SelectorKind::Super),
                TokenKind::Interpolation => self.tokens_to_selectors(
                    &mut eat_interpolation(tokens, self.scope).into_iter().peekable(),
                ),
                TokenKind::Attribute(attr) => self.selectors.push(SelectorKind::Attribute(attr)),
                _ => todo!("unimplemented selector"),
            };
        }
    }
}

impl Selector {
    pub fn from_tokens<'a>(
        tokens: &'a mut Peekable<IntoIter<Token>>,
        scope: &'a Scope,
    ) -> Selector {
        SelectorParser::new(scope).all_selectors(tokens)
    }

    pub fn zip(&self, other: &Selector) -> Selector {
        if self.0.is_empty() {
            return Selector(other.0.clone());
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
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Attribute {
    pub attr: String,
    pub value: String,
    pub case_sensitive: bool,
    pub kind: AttributeKind,
}

impl Display for Attribute {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.case_sensitive {
            match self.kind {
                AttributeKind::Any => write!(f, "[{}]", self.attr),
                AttributeKind::Equals => write!(f, "[{}={}]", self.attr, self.value),
                AttributeKind::InList => write!(f, "[{}~={}]", self.attr, self.value),
                AttributeKind::BeginsWithHyphenOrExact => {
                    write!(f, "[{}|={}]", self.attr, self.value)
                }
                AttributeKind::StartsWith => write!(f, "[{}^={}]", self.attr, self.value),
                AttributeKind::EndsWith => write!(f, "[{}$={}]", self.attr, self.value),
                AttributeKind::Contains => write!(f, "[{}*={}]", self.attr, self.value),
            }
        } else {
            match self.kind {
                AttributeKind::Any => write!(f, "[{} i]", self.attr),
                AttributeKind::Equals => write!(f, "[{}={} i]", self.attr, self.value),
                AttributeKind::InList => write!(f, "[{}~={} i]", self.attr, self.value),
                AttributeKind::BeginsWithHyphenOrExact => {
                    write!(f, "[{}|={} i]", self.attr, self.value)
                }
                AttributeKind::StartsWith => write!(f, "[{}^={} i]", self.attr, self.value),
                AttributeKind::EndsWith => write!(f, "[{}$={} i]", self.attr, self.value),
                AttributeKind::Contains => write!(f, "[{}*={} i]", self.attr, self.value),
            }
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum AttributeKind {
    /// [attr]
    /// Represents elements with an attribute name of `attr`
    Any,
    /// [attr=value]
    /// Represents elements with an attribute name of `attr` whose value is exactly `value`
    Equals,
    /// [attr~=value]
    /// Represents elements with an attribute name of `attr` whose value is a whitespace-separated list of words, one of which is exactly `value`
    InList,
    /// [attr|=value]
    /// Represents elements with an attribute name of `attr` whose value can be exactly value or can begin with `value` immediately followed by a hyphen (`-`)
    BeginsWithHyphenOrExact,
    /// [attr^=value]
    StartsWith,
    /// [attr$=value]
    EndsWith,
    /// [attr*=value]
    /// Represents elements with an attribute name of `attr` whose value contains at least one occurrence of `value` within the string
    Contains,
}
