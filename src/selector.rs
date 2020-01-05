use crate::common::Symbol;
use crate::{Token, TokenKind};
use std::iter::Peekable;
use std::{
    fmt::{self, Display},
    slice::Iter,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Selector {
    /// An element selector: `button`
    Element(String),
    /// An id selector: `#footer`
    Id(String),
    /// A single class selector: `.button-active`
    Class(String),
    /// A universal selector: `*`
    Universal,
    /// A simple child selector: `ul li`
    Descendant(Box<Selector>, Box<Selector>),
    /// And selector: `button.active`
    And(Box<Selector>, Box<Selector>),
    /// Multiple unrelated selectors: `button, .active`
    Multiple(Box<Selector>, Box<Selector>),
    /// Select all immediate children: `ul > li`
    ImmediateChild(Box<Selector>, Box<Selector>),
    /// Select all elements immediately following: `div + p`
    Following(Box<Selector>, Box<Selector>),
    /// Select elements preceeded by: `p ~ ul`
    Preceding(Box<Selector>, Box<Selector>),
    /// Select elements with attribute: `html[lang|=en]`
    Attribute(Attribute),
    /// Pseudo selector: `:hover`
    Pseudo(String),
}

impl Display for Selector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Selector::Element(s) => write!(f, "{}", s),
            Selector::Id(s) => write!(f, "#{}", s),
            Selector::Class(s) => write!(f, ".{}", s),
            Selector::Universal => write!(f, "*"),
            Selector::Descendant(lhs, rhs) => write!(f, "{} {}", lhs, rhs),
            Selector::And(lhs, rhs) => write!(f, "{}{}", lhs, rhs),
            Selector::Multiple(lhs, rhs) => write!(f, "{}, {}", lhs, rhs),
            Selector::ImmediateChild(lhs, rhs) => write!(f, "{} > {}", lhs, rhs),
            Selector::Following(lhs, rhs) => write!(f, "{} + {}", lhs, rhs),
            Selector::Preceding(lhs, rhs) => write!(f, "{} ~ {}", lhs, rhs),
            Selector::Attribute(attr) => write!(f, "{}", attr),
            Selector::Pseudo(s) => write!(f, ":{}", s),
        }
    }
}

struct SelectorParser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
}

impl<'a> SelectorParser<'a> {
    const fn new(tokens: Peekable<Iter<'a, Token>>) -> SelectorParser<'a> {
        SelectorParser { tokens }
    }

    fn all_selectors(&mut self) -> Selector {
        self.devour_whitespace();
        let left = self
            .consume_selector()
            .expect("expected left handed selector");
        let whitespace: bool = self.devour_whitespace();
        match self.tokens.peek() {
            Some(tok) => match tok.kind {
                TokenKind::Ident(_) => {
                    return Selector::Descendant(Box::new(left), Box::new(self.all_selectors()))
                }
                TokenKind::Symbol(Symbol::Plus) => {
                    self.tokens.next();
                    self.devour_whitespace();
                    return Selector::Following(Box::new(left), Box::new(self.all_selectors()));
                }
                TokenKind::Symbol(Symbol::Tilde) => {
                    self.tokens.next();
                    self.devour_whitespace();
                    return Selector::Preceding(Box::new(left), Box::new(self.all_selectors()));
                }
                TokenKind::Symbol(Symbol::Comma) => {
                    self.tokens.next();
                    self.devour_whitespace();
                    return Selector::Multiple(Box::new(left), Box::new(self.all_selectors()));
                }
                TokenKind::Symbol(Symbol::Gt) => {
                    self.tokens.next();
                    self.devour_whitespace();
                    return Selector::ImmediateChild(
                        Box::new(left),
                        Box::new(self.all_selectors()),
                    );
                }
                TokenKind::Symbol(Symbol::Colon)
                | TokenKind::Symbol(Symbol::Period)
                | TokenKind::Symbol(Symbol::Mul)
                | TokenKind::Selector(_)
                | TokenKind::Symbol(Symbol::Hash) => {
                    if whitespace {
                        return Selector::Descendant(
                            Box::new(left),
                            Box::new(self.all_selectors()),
                        );
                    } else {
                        return Selector::And(Box::new(left), Box::new(self.all_selectors()));
                    }
                }
                TokenKind::Symbol(Symbol::Lt) => {}
                _ => todo!(),
            },
            None => return left,
        }
        todo!()
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

    fn consume_selector(&mut self) -> Option<Selector> {
        if let Some(tok) = self.tokens.next() {
            let selector = match tok.kind {
                TokenKind::Symbol(Symbol::Period) => match self
                    .tokens
                    .next()
                    .expect("expected ident after `.`")
                    .clone()
                    .kind
                {
                    TokenKind::Ident(tok) => Selector::Class(tok),
                    _ => todo!("there should normally be an ident after `.`"),
                },
                TokenKind::Symbol(Symbol::Mul) => Selector::Universal,
                TokenKind::Symbol(Symbol::Hash) => match self
                    .tokens
                    .next()
                    .expect("expected ident after `#`")
                    .clone()
                    .kind
                {
                    TokenKind::Ident(tok) => Selector::Id(tok),
                    _ => todo!("there should normally be an ident after `#`"),
                },
                TokenKind::Symbol(Symbol::Colon) => {
                    match self
                        .tokens
                        .next()
                        .expect("expected ident after `:`")
                        .clone()
                        .kind
                    {
                        TokenKind::Ident(tok) => Selector::Pseudo(tok),
                        _ => todo!("there should normally be an ident after `:`"),
                    }
                }
                TokenKind::Ident(ref tok) => Selector::Element(tok.clone()),
                TokenKind::Selector(ref sel) => sel.clone(),
                _ => todo!(),
            };
            Some(selector)
        } else {
            None
        }
    }
}

impl Selector {
    pub fn from_tokens(tokens: Peekable<Iter<Token>>) -> Selector {
        SelectorParser::new(tokens).all_selectors()
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
