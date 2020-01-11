use crate::common::Symbol;
use crate::{Token, TokenKind};
use std::fmt::{self, Display};
use std::iter::Peekable;
use std::slice::Iter;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Selector(pub Vec<SelectorKind>);

fn devour_whitespace(i: &mut Peekable<Iter<SelectorKind>>) -> bool {
    let mut found_whitespace = false;
    while let Some(SelectorKind::Whitespace) = i.peek() {
        i.next();
        found_whitespace = true;
    }
    found_whitespace
}

impl Display for Selector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut iter = self.0.iter().peekable();

        while let Some(s) = iter.next() {
            match s {
                SelectorKind::Whitespace => continue,
                SelectorKind::Attribute(_)
                | SelectorKind::Pseudo(_)
                | SelectorKind::Class
                | SelectorKind::Id
                | SelectorKind::Universal
                | SelectorKind::Element(_) => {
                    write!(f, "{}", s)?;
                    if devour_whitespace(&mut iter) {
                        match iter.peek() {
                            Some(SelectorKind::Attribute(_))
                            | Some(SelectorKind::Pseudo(_))
                            | Some(SelectorKind::Class)
                            | Some(SelectorKind::Id)
                            | Some(SelectorKind::Universal)
                            | Some(SelectorKind::Element(_)) => {
                                write!(f, " {}", iter.next().expect("already peeked here"))?;
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
pub enum SelectorKind {
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
    /// Use the super selector: `&.red`
    // Super,
    /// Used to signify no selector (when there is no super_selector of a rule)
    None,
    Whitespace,
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
            // SelectorKind::Super => write!(f, "{}"),
            SelectorKind::None => write!(f, ""),
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
}

struct SelectorParser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
    super_selector: &'a Selector,
}

impl<'a> SelectorParser<'a> {
    const fn new(
        tokens: Peekable<Iter<'a, Token>>,
        super_selector: &'a Selector,
    ) -> SelectorParser<'a> {
        SelectorParser {
            tokens,
            super_selector,
        }
    }

    fn all_selectors(&mut self) -> Selector {
        let mut v = Vec::new();
        while let Some(s) = self.consume_selector() {
            v.push(s);
        }
        Selector(v)
    }

    fn consume_selector(&mut self) -> Option<SelectorKind> {
        if self.devour_whitespace() {
            return Some(SelectorKind::Whitespace);
        }
        if let Some(Token { kind, .. }) = self.tokens.next() {
            return Some(match &kind {
                TokenKind::Ident(tok) => SelectorKind::Element(tok.clone()),
                TokenKind::Symbol(Symbol::Period) => SelectorKind::Class,
                TokenKind::Symbol(Symbol::Hash) => SelectorKind::Id,
                TokenKind::Symbol(Symbol::Colon) => {
                    if let Some(Token {
                        kind: TokenKind::Ident(s),
                        ..
                    }) = self.tokens.next()
                    {
                        SelectorKind::Pseudo(s.clone())
                    } else {
                        todo!("expected ident after `:` in selector")
                    }
                }
                TokenKind::Symbol(Symbol::Comma) => SelectorKind::Multiple,
                TokenKind::Symbol(Symbol::Gt) => SelectorKind::ImmediateChild,
                TokenKind::Symbol(Symbol::Plus) => SelectorKind::Following,
                TokenKind::Symbol(Symbol::Tilde) => SelectorKind::Preceding,
                TokenKind::Symbol(Symbol::Mul) => SelectorKind::Universal,
                // TokenKind::Symbol(Symbol::BitAnd) => SelectorKind::Super,
                TokenKind::Attribute(attr) => SelectorKind::Attribute(attr.clone()),
                _ => todo!("unimplemented selector"),
            });
        }
        None
    }

    fn devour_whitespace(&mut self) -> bool {
        let mut found_whitespace = false;
        while let Some(tok) = self.tokens.peek() {
            match tok.kind {
                TokenKind::Whitespace(_) => {
                    self.tokens.next();
                    found_whitespace = true;
                }
                _ => break,
            }
        }
        found_whitespace
    }
}

impl Selector {
    pub fn from_tokens<'a>(
        tokens: Peekable<Iter<'a, Token>>,
        super_selector: &'a Selector,
    ) -> Selector {
        SelectorParser::new(tokens, super_selector).all_selectors()
    }

    pub fn zip(self, other: Selector) -> Selector {
        if self.0.is_empty() {
            return Selector(other.0);
        }
        let mut rules: Vec<SelectorKind> = Vec::with_capacity(self.0.len());
        let sel1_split: Vec<Vec<SelectorKind>> = self
            .0
            .split(|sel| sel == &SelectorKind::Multiple)
            .map(|x| x.to_vec())
            .collect();
        let sel2_split: Vec<Vec<SelectorKind>> = other
            .0
            .split(|sel| sel == &SelectorKind::Multiple)
            .map(|x| x.to_vec())
            .collect();
        for (idx, sel1) in sel1_split.iter().enumerate() {
            for (idx2, sel2) in sel2_split.iter().enumerate() {
                rules.extend(sel1.iter().cloned());
                rules.push(SelectorKind::Whitespace);
                rules.extend(sel2.iter().cloned());
                if !(idx + 1 == sel1_split.len() && idx2 + 1 == sel2_split.len()) {
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
