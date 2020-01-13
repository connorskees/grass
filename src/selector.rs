use crate::common::{Scope, Symbol};
use crate::utils::{devour_whitespace, IsWhitespace};
use crate::{Token, TokenKind};
use std::fmt::{self, Display};
use std::iter::Peekable;
use std::slice::Iter;
use std::string::ToString;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Selector(pub Vec<SelectorKind>);

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
    /// Pseudo selector with additional parens: `:any(h1, h2, h3, h4, h5, h6)`
    PseudoParen(String, Vec<TokenKind>),
    /// Use the super selector: `&.red`
    Super,
    /// Used to signify no selector (when there is no super_selector of a rule)
    None,
    Whitespace,
    /// Intemediate value to simplify usage with identifier interpolation
    Several(Vec<SelectorKind>),
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
            SelectorKind::Several(_) => {
                panic!("SelectorKind::Several should not be rendered using Display")
            }
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
    tokens: Peekable<Iter<'a, Token>>,
    super_selector: &'a Selector,
    scope: &'a Scope,
}

/// Methods to handle dealing with interpolation
impl<'a> SelectorParser<'a> {
    fn consume_interpolation(&mut self) -> SelectorKind {
        let mut v = Vec::new();
        let toks = self
            .tokens
            .by_ref()
            .take_while(|x| x.kind != TokenKind::Symbol(Symbol::CloseCurlyBrace))
            .cloned()
            .collect::<Vec<Token>>(); //.iter().peekable();
        let mut toks = toks.iter().peekable();
        while let Some(Token { kind, .. }) = toks.peek() {
            if let TokenKind::Variable(_) = kind {
                toks.next();
                let these_toks = self.deref_variable(kind);
                let mut these_toks = these_toks.iter().peekable();
                while let Some(s) = self.selector_from_token_stream(&mut these_toks) {
                    v.push(s);
                }
            } else if let Some(s) = self.selector_from_token_stream(&mut toks) {
                v.push(s);
            } else {
                return SelectorKind::Several(v);
            }
        }
        SelectorKind::Several(v)
    }

    fn selector_from_token_stream(
        &mut self,
        toks: &mut Peekable<Iter<'_, Token>>,
    ) -> Option<SelectorKind> {
        if devour_whitespace(toks) {
            if let Some(&&Token {
                kind: TokenKind::Symbol(Symbol::Comma),
                ..
            }) = toks.peek()
            {
                toks.next();
                return Some(SelectorKind::Multiple);
            }
            return Some(SelectorKind::Whitespace);
        }
        if let Some(Token { kind, .. }) = toks.next() {
            return Some(match &kind {
                TokenKind::Ident(tok) => SelectorKind::Element(tok.clone()),
                TokenKind::Symbol(Symbol::Period) => SelectorKind::Class,
                TokenKind::Symbol(Symbol::Hash) => SelectorKind::Id,
                TokenKind::Symbol(Symbol::Colon) => return self.consume_pseudo_selector(),
                TokenKind::Symbol(Symbol::Comma) => SelectorKind::Multiple,
                TokenKind::Symbol(Symbol::Gt) => SelectorKind::ImmediateChild,
                TokenKind::Symbol(Symbol::Plus) => SelectorKind::Following,
                TokenKind::Symbol(Symbol::Tilde) => SelectorKind::Preceding,
                TokenKind::Symbol(Symbol::Mul) => SelectorKind::Universal,
                TokenKind::Symbol(Symbol::BitAnd) => SelectorKind::Super,
                TokenKind::Interpolation => self.consume_interpolation(),
                TokenKind::Attribute(attr) => SelectorKind::Attribute(attr.clone()),
                _ => todo!("unimplemented selector"),
            });
        }
        None
    }

    fn deref_variable(&mut self, variable: &TokenKind) -> Vec<Token> {
        let mut val = Vec::with_capacity(25);
        let v = match variable {
            TokenKind::Variable(ref v) => self
                .scope
                .vars
                .get(v)
                .expect("todo! expected variable to exist"),
            _ => todo!("expected variable"),
        }
        .iter()
        .peekable();
        for tok in v {
            match &tok.kind {
                TokenKind::Variable(_) => val.extend(self.deref_variable(&tok.kind)),
                _ => val.push(tok.clone()),
            };
        }
        val
    }
}

impl<'a> SelectorParser<'a> {
    const fn new(
        tokens: Peekable<Iter<'a, Token>>,
        super_selector: &'a Selector,
        scope: &'a Scope,
    ) -> SelectorParser<'a> {
        SelectorParser {
            tokens,
            super_selector,
            scope,
        }
    }

    fn all_selectors(&mut self) -> Selector {
        let mut v = Vec::with_capacity(self.tokens.len());
        while let Some(s) = self.consume_selector() {
            if let SelectorKind::Several(sels) = s {
                v.extend(sels);
            } else {
                v.push(s);
            }
        }
        while let Some(x) = v.pop() {
            if x != SelectorKind::Whitespace {
                v.push(x);
                break;
            }
        }
        Selector(v)
    }

    fn consume_pseudo_selector(&mut self) -> Option<SelectorKind> {
        if let Some(Token {
            kind: TokenKind::Ident(s),
            ..
        }) = self.tokens.next()
        {
            if let Some(Token {
                kind: TokenKind::Symbol(Symbol::OpenParen),
                ..
            }) = self.tokens.peek()
            {
                self.tokens.next();
                let mut toks = Vec::new();
                while let Some(Token { kind, .. }) = self.tokens.peek() {
                    if kind == &TokenKind::Symbol(Symbol::CloseParen) {
                        break;
                    }
                    let tok = self.tokens.next().unwrap();
                    toks.push(tok.kind.clone());
                }
                self.tokens.next();
                Some(SelectorKind::PseudoParen(s.clone(), toks))
            } else {
                Some(SelectorKind::Pseudo(s.clone()))
            }
        } else {
            todo!("expected ident after `:` in selector")
        }
    }

    fn consume_selector(&mut self) -> Option<SelectorKind> {
        if devour_whitespace(&mut self.tokens) {
            if let Some(&&Token {
                kind: TokenKind::Symbol(Symbol::Comma),
                ..
            }) = self.tokens.peek()
            {
                self.tokens.next();
                return Some(SelectorKind::Multiple);
            }
            return Some(SelectorKind::Whitespace);
        }
        if let Some(Token { kind, .. }) = self.tokens.next() {
            return Some(match &kind {
                TokenKind::Ident(tok) => SelectorKind::Element(tok.clone()),
                TokenKind::Symbol(Symbol::Period) => SelectorKind::Class,
                TokenKind::Symbol(Symbol::Hash) => SelectorKind::Id,
                TokenKind::Symbol(Symbol::Colon) => return self.consume_pseudo_selector(),
                TokenKind::Symbol(Symbol::Comma) => SelectorKind::Multiple,
                TokenKind::Symbol(Symbol::Gt) => SelectorKind::ImmediateChild,
                TokenKind::Symbol(Symbol::Plus) => SelectorKind::Following,
                TokenKind::Symbol(Symbol::Tilde) => SelectorKind::Preceding,
                TokenKind::Symbol(Symbol::Mul) => SelectorKind::Universal,
                TokenKind::Symbol(Symbol::BitAnd) => SelectorKind::Super,
                TokenKind::Interpolation => self.consume_interpolation(),
                TokenKind::Attribute(attr) => SelectorKind::Attribute(attr.clone()),
                _ => todo!("unimplemented selector"),
            });
        }
        None
    }
}

impl Selector {
    pub fn from_tokens<'a>(
        tokens: Peekable<Iter<'a, Token>>,
        super_selector: &'a Selector,
        scope: &'a Scope,
    ) -> Selector {
        SelectorParser::new(tokens, super_selector, scope).all_selectors()
    }

    pub fn zip(self, other: Selector) -> Selector {
        if self.0.is_empty() {
            return Selector(other.0);
        }
        let mut rules: Vec<SelectorKind> = Vec::with_capacity(self.0.len() + other.0.len());
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
                let mut this_selector = Vec::with_capacity(other.0.len());
                let mut found_super = false;

                for sel in sel2 {
                    if sel == &SelectorKind::Super {
                        this_selector.extend(sel1.iter().cloned());
                        found_super = true;
                    } else {
                        this_selector.push(sel.clone());
                    }
                }

                if !found_super {
                    rules.extend(sel1.iter().cloned());
                    rules.push(SelectorKind::Whitespace);
                }
                rules.extend(this_selector);

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
