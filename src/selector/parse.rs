use peekmore::PeekMoreIterator;

use codemap::Span;

use super::{
    Attribute, Combinator, ComplexSelector, ComplexSelectorComponent, CompoundSelector, Namespace,
    Pseudo, QualifiedName, SelectorList, SimpleSelector,
};
use crate::error::SassResult;
use crate::scope::Scope;
use crate::selector::Selector;
use crate::utils::{
    devour_whitespace, eat_ident, is_name, is_name_start, read_until_closing_paren,
};
use crate::Token;

#[derive(PartialEq)]
enum DevouredWhitespace {
    /// Some whitespace was found
    Whitespace,
    /// A newline and potentially other whitespace was found
    Newline,
    /// No whitespace was found
    None,
}

impl DevouredWhitespace {
    fn found_whitespace(&mut self) {
        if self == &Self::None {
            *self = Self::Whitespace;
        }
    }

    fn found_newline(&mut self) {
        *self = Self::Newline;
    }
}

/// Pseudo-class selectors that take unadorned selectors as arguments.
const SELECTOR_PSEUDO_CLASSES: [&'static str; 7] = [
    "not",
    "matches",
    "current",
    "any",
    "has",
    "host",
    "host-context",
];

/// Pseudo-element selectors that take unadorned selectors as arguments.
const SELECTOR_PSEUDO_ELEMENTS: [&'static str; 1] = ["slotted"];

pub(crate) struct SelectorParser<'a, I: Iterator<Item = Token>> {
    /// Whether this parser allows the parent selector `&`.
    allows_parent: bool,

    /// Whether this parser allows placeholder selectors beginning with `%`.
    allows_placeholder: bool,

    toks: &'a mut PeekMoreIterator<I>,

    scope: &'a Scope,

    super_selector: &'a Selector,

    span: Span,
}

impl<'a, I: Iterator<Item = Token>> SelectorParser<'a, I> {
    pub fn new(
        toks: &'a mut PeekMoreIterator<I>,
        scope: &'a Scope,
        super_selector: &'a Selector,
        allows_parent: bool,
        allows_placeholder: bool,
        span: Span,
    ) -> Self {
        Self {
            toks,
            scope,
            super_selector,
            allows_parent,
            allows_placeholder,
            span,
        }
    }

    pub fn parse(mut self) -> SassResult<SelectorList> {
        let tmp = self.parse_selector_list()?;
        if self.toks.peek().is_some() {
            return Err(("expected selector.", self.span).into());
        }
        Ok(tmp)
    }

    fn parse_selector_list(&mut self) -> SassResult<SelectorList> {
        let mut components = vec![self.parse_complex_selector(false)?];

        devour_whitespace(self.toks);

        let mut line_break = false;

        while let Some(Token { kind: ',', .. }) = self.toks.peek() {
            self.toks.next();
            line_break = self.eat_whitespace() == DevouredWhitespace::Newline || line_break;
            match self.toks.peek() {
                Some(Token { kind: ',', .. }) => continue,
                Some(..) => {}
                None => break,
            }
            components.push(self.parse_complex_selector(line_break)?);

            line_break = false;
        }

        return Ok(SelectorList { components });
    }

    fn eat_whitespace(&mut self) -> DevouredWhitespace {
        let mut whitespace_devoured = DevouredWhitespace::None;
        while let Some(tok) = self.toks.peek() {
            match tok.kind {
                ' ' | '\t' => whitespace_devoured.found_whitespace(),
                '\n' => whitespace_devoured.found_newline(),
                _ => break,
            }
            self.toks.next();
        }

        whitespace_devoured
    }

    /// Consumes a complex selector.
    ///
    /// If `line_break` is `true`, that indicates that there was a line break
    /// before this selector.
    fn parse_complex_selector(&mut self, line_break: bool) -> SassResult<ComplexSelector> {
        let mut components = Vec::new();

        // todo: or patterns
        loop {
            devour_whitespace(self.toks);

            // todo: can we do while let Some(..) = self.toks.peek() ?
            match self.toks.peek() {
                Some(Token { kind: '+', .. }) => {
                    self.toks.next();
                    components.push(ComplexSelectorComponent::Combinator(
                        Combinator::NextSibling,
                    ));
                }
                Some(Token { kind: '>', .. }) => {
                    self.toks.next();
                    components.push(ComplexSelectorComponent::Combinator(Combinator::Child))
                }
                Some(Token { kind: '~', .. }) => {
                    self.toks.next();
                    components.push(ComplexSelectorComponent::Combinator(
                        Combinator::FollowingSibling,
                    ));
                }
                Some(Token { kind: '[', .. })
                | Some(Token { kind: '.', .. })
                | Some(Token { kind: '#', .. })
                | Some(Token { kind: '%', .. })
                | Some(Token { kind: ':', .. })
                // todo: ampersand?
                | Some(Token { kind: '&', .. })
                | Some(Token { kind: '*', .. })
                | Some(Token { kind: '|', .. }) => {
                    components.push(ComplexSelectorComponent::Compound(
                        self.parse_compound_selector()?,
                    ));
                    if let Some(Token { kind: '&', .. }) = self.toks.peek() {
                        return Err(("\"&\" may only used at the beginning of a compound selector.", self.span).into());
                    }
                }
                Some(..) => {
                    if !self.looking_at_identifier() {
                        break;
                    }
                    components.push(ComplexSelectorComponent::Compound(
                        self.parse_compound_selector()?,
                    ));
                    if let Some(Token { kind: '&', .. }) = self.toks.peek() {
                        return Err(("\"&\" may only used at the beginning of a compound selector.", self.span).into());
                    }
                }
                None => break,
            }
        }

        if components.is_empty() {
            return Err(("expected selector.", self.span).into());
        }

        Ok(ComplexSelector {
            components,
            line_break,
        })
    }

    fn parse_compound_selector(&mut self) -> SassResult<CompoundSelector> {
        let mut components = vec![self.parse_simple_selector(true)?];

        while let Some(Token { kind, .. }) = self.toks.peek() {
            if !is_simple_selector_start(*kind) {
                break;
            }

            components.push(self.parse_simple_selector(false)?);
        }

        Ok(CompoundSelector { components })
    }

    /// Returns whether the scanner is immediately before a plain CSS identifier.
    ///
    // todo: foward arg
    /// If `forward` is passed, this looks that many characters forward instead.
    ///
    /// This is based on [the CSS algorithm][], but it assumes all backslashes
    /// start escapes.
    ///
    /// [the CSS algorithm]: https://drafts.csswg.org/css-syntax-3/#would-start-an-identifier
    fn looking_at_identifier(&mut self) -> bool {
        match self.toks.peek() {
            Some(Token { kind, .. }) if is_name_start(*kind) || kind == &'\\' => return true,
            Some(Token { kind: '-', .. }) => {}
            Some(..) | None => return false,
        }

        match self.toks.peek_forward(1) {
            Some(Token { kind, .. }) if is_name_start(*kind) || kind == &'-' || kind == &'\\' => {
                self.toks.reset_view();
                true
            }
            Some(..) | None => {
                self.toks.reset_view();
                false
            }
        }
    }

    fn looking_at_identifier_body(&mut self) -> bool {
        matches!(self.toks.peek(), Some(t) if is_name(t.kind) || t.kind == '\\')
    }

    /// Consumes a simple selector.
    fn parse_simple_selector(&mut self, allow_parent: bool) -> SassResult<SimpleSelector> {
        match self.toks.peek() {
            Some(Token { kind: '[', .. }) => self.parse_attribute_selector(),
            Some(Token { kind: '.', .. }) => self.parse_class_selector(),
            Some(Token { kind: '#', .. }) => self.parse_id_selector(),
            Some(Token { kind: '%', .. }) => {
                if !self.allows_placeholder {
                    return Err(("Placeholder selectors aren't allowed here.", self.span).into());
                }
                self.parse_placeholder_selector()
            }
            Some(Token { kind: ':', .. }) => self.parse_pseudo_selector(),
            Some(Token { kind: '&', .. }) => {
                if !allow_parent && !self.allows_parent {
                    return Err(("Parent selectors aren't allowed here.", self.span).into());
                }
                self.parse_parent_selector()
            }
            _ => self.parse_type_or_universal_selector(),
        }
    }

    fn parse_attribute_selector(&mut self) -> SassResult<SimpleSelector> {
        let start = self.toks.next().unwrap().pos;
        Ok(SimpleSelector::Attribute(Attribute::from_tokens(
            self.toks,
            self.scope,
            self.super_selector,
            start,
        )?))
    }

    fn parse_class_selector(&mut self) -> SassResult<SimpleSelector> {
        let span_before = self.toks.next().unwrap().pos;
        Ok(SimpleSelector::Class(
            eat_ident(self.toks, self.scope, self.super_selector, span_before)?.node,
        ))
    }

    fn parse_id_selector(&mut self) -> SassResult<SimpleSelector> {
        let span_before = self.toks.next().unwrap().pos;
        Ok(SimpleSelector::Id(
            eat_ident(self.toks, self.scope, self.super_selector, span_before)?.node,
        ))
    }

    fn parse_pseudo_selector(&mut self) -> SassResult<SimpleSelector> {
        let span_before = self.toks.next().unwrap().pos;
        let element = match self.toks.peek() {
            Some(Token { kind: ':', .. }) => {
                self.toks.next();
                true
            }
            _ => false,
        };

        let name = eat_ident(self.toks, self.scope, self.super_selector, span_before)?;

        match self.toks.peek() {
            Some(Token { kind: '(', .. }) => self.toks.next(),
            _ => {
                return Ok(SimpleSelector::Pseudo(Pseudo {
                    // todo: we can store the reference to this
                    normalized_name: unvendor(&name.node).to_string(),
                    is_class: !element && !is_fake_pseudo_element(&name),
                    name: name.node,
                    selector: None,
                    is_syntactic_class: !element,
                    argument: None,
                }));
            }
        };

        devour_whitespace(self.toks);

        let unvendored = unvendor(&name);

        let mut argument: Option<String> = None;
        let mut selector: Option<SelectorList> = None;

        if element {
            // todo: lowercase?
            if SELECTOR_PSEUDO_ELEMENTS.contains(&unvendored) {
                selector = Some(self.parse_selector_list()?);
                devour_whitespace(self.toks);
                assert!(matches!(self.toks.next(), Some(Token { kind: ')', .. })));
            } else {
                argument = Some(self.declaration_value(true)?);
            }
        } else if SELECTOR_PSEUDO_CLASSES.contains(&unvendored) {
            selector = Some(self.parse_selector_list()?);
            devour_whitespace(self.toks);
            assert!(matches!(self.toks.next(), Some(Token { kind: ')', .. })));
        } else if unvendored == "nth-child" || unvendored == "nth-last-child" {
            let mut this_arg = self.parse_a_n_plus_b()?;
            let found_whitespace = devour_whitespace(self.toks);
            match (found_whitespace, self.toks.peek()) {
                (_, Some(Token { kind: ')', .. })) => {}
                (true, _) => {
                    expect_identifier("of")?;
                    this_arg.push_str("of");
                    devour_whitespace(self.toks);
                    selector = Some(self.parse_selector_list()?);
                }
                _ => {}
            }
            argument = Some(this_arg);
        } else {
            argument = Some(self.declaration_value(true)?.trim_end().to_string());
        }

        // todo: closing paren is consumed
        // if let Some(Token { kind: ')', .. }) = dbg!(self.toks.next()) {
        // } else {
        //     todo!("expected \")\".");
        // }

        Ok(SimpleSelector::Pseudo(Pseudo {
            normalized_name: unvendor(&name.node).to_string(),
            is_class: !element && !is_fake_pseudo_element(&name),
            name: name.node,
            selector,
            // todo: we can store the reference to this
            is_syntactic_class: !element,
            argument,
        }))
    }

    fn parse_parent_selector(&mut self) -> SassResult<SimpleSelector> {
        let span_before = self.toks.next().unwrap().pos;
        let suffix = if self.looking_at_identifier_body() {
            Some(eat_ident(self.toks, self.scope, self.super_selector, span_before)?.node)
        } else {
            None
        };
        Ok(SimpleSelector::Parent(suffix))
    }

    fn parse_placeholder_selector(&mut self) -> SassResult<SimpleSelector> {
        let span_before = self.toks.next().unwrap().pos;
        Ok(SimpleSelector::Placeholder(
            eat_ident(self.toks, self.scope, self.super_selector, span_before)?.node,
        ))
    }

    /// Consumes a type selector or a universal selector.
    ///
    /// These are combined because either one could start with `*`.
    fn parse_type_or_universal_selector(&mut self) -> SassResult<SimpleSelector> {
        let span_before = self.toks.peek().unwrap().pos;

        match self.toks.peek().cloned() {
            Some(Token { kind: '*', pos }) => {
                self.toks.next();
                if let Some(Token { kind: '|', .. }) = self.toks.peek() {
                    self.toks.next();
                    if let Some(Token { kind: '*', .. }) = self.toks.peek() {
                        self.toks.next();
                        return Ok(SimpleSelector::Universal(Namespace::Asterisk));
                    } else {
                        return Ok(SimpleSelector::Type(QualifiedName {
                            ident: eat_ident(self.toks, self.scope, self.super_selector, pos)?.node,
                            namespace: Namespace::Asterisk,
                        }));
                    }
                } else {
                    return Ok(SimpleSelector::Universal(Namespace::None));
                }
            }
            Some(Token { kind: '|', pos }) => {
                self.toks.next();
                match self.toks.peek() {
                    Some(Token { kind: '*', .. }) => {
                        self.toks.next();
                        return Ok(SimpleSelector::Universal(Namespace::Empty));
                    }
                    _ => {
                        return Ok(SimpleSelector::Type(QualifiedName {
                            ident: eat_ident(self.toks, self.scope, self.super_selector, pos)?.node,
                            namespace: Namespace::Empty,
                        }));
                    }
                }
            }
            _ => {}
        }

        let name_or_namespace =
            eat_ident(self.toks, self.scope, self.super_selector, span_before)?.node;

        Ok(match self.toks.peek() {
            Some(Token { kind: '|', .. }) => {
                self.toks.next();
                if let Some(Token { kind: '*', .. }) = self.toks.peek() {
                    self.toks.next();
                    SimpleSelector::Universal(Namespace::Other(name_or_namespace))
                } else {
                    SimpleSelector::Type(QualifiedName {
                        ident: eat_ident(self.toks, self.scope, self.super_selector, span_before)?
                            .node,
                        namespace: Namespace::Other(name_or_namespace),
                    })
                }
            }
            Some(..) | None => SimpleSelector::Type(QualifiedName {
                ident: name_or_namespace,
                namespace: Namespace::None,
            }),
        })
    }

    /// Consumes an [`An+B` production][An+B] and returns its text.
    ///
    /// [An+B]: https://drafts.csswg.org/css-syntax-3/#anb-microsyntax
    fn parse_a_n_plus_b(&mut self) -> SassResult<String> {
        let mut buf = String::new();

        match self.toks.peek() {
            Some(Token { kind: 'e', .. }) | Some(Token { kind: 'E', .. }) => todo!("even"),
            Some(Token { kind: 'o', .. }) | Some(Token { kind: 'O', .. }) => todo!("odd"),
            Some(t @ Token { kind: '+', .. }) | Some(t @ Token { kind: '-', .. }) => {
                buf.push(t.kind);
                self.toks.next();
            }
            _ => {}
        }

        match self.toks.peek() {
            Some(t) if t.kind.is_ascii_digit() => {
                while let Some(t) = self.toks.peek() {
                    if !t.kind.is_ascii_digit() {
                        break;
                    }
                    devour_whitespace(self.toks);
                    if let Some(t) = self.toks.next() {
                        if t.kind != 'n' && t.kind != 'N' {
                            return Ok(buf);
                        }
                    }
                }
            }
            Some(t) => {
                if t.kind == 'n' || t.kind == 'N' {
                    self.toks.next();
                } else {
                    todo!()
                }
            }
            None => todo!(),
        }

        buf.push('n');

        devour_whitespace(self.toks);

        if let Some(Token { kind: '+', .. }) | Some(Token { kind: '-', .. }) = self.toks.peek() {
            buf.push(self.toks.next().unwrap().kind);
            devour_whitespace(self.toks);
            match self.toks.peek() {
                Some(t) if !t.kind.is_ascii_digit() => {
                    return Err(("Expected a number.", self.span).into())
                }
                None => return Err(("Expected a number.", self.span).into()),
                _ => {}
            }

            while let Some(t) = self.toks.peek() {
                if !t.kind.is_ascii_digit() {
                    break;
                }
                buf.push(self.toks.next().unwrap().kind);
            }
        }
        Ok(buf)
    }

    fn declaration_value(&mut self, allow_empty: bool) -> SassResult<String> {
        // todo: this consumes the closing paren
        let mut tmp = read_until_closing_paren(self.toks)?;
        if let Some(Token { kind: ')', .. }) = tmp.pop() {
        } else {
            return Err(("expected \")\".", self.span).into());
        }
        Ok(tmp.into_iter().map(|t| t.kind).collect::<String>())
    }
}

/// Returns whether `c` can start a simple selector other than a type
/// selector.
fn is_simple_selector_start(c: char) -> bool {
    matches!(c, '*' | '[' | '.' | '#' | '%' | ':')
}

/// Returns `name` without a vendor prefix.
///
/// If `name` has no vendor prefix, it's returned as-is.
fn unvendor(name: &str) -> &str {
    let bytes = name.as_bytes();

    if bytes.len() < 2 {
        return name;
    }

    if bytes[0usize] != b'-' || bytes[1usize] == b'-' {
        return name;
    }

    for i in 2..bytes.len() {
        if bytes.get(i) == Some(&b'-') {
            return &name[i + 1..];
        }
    }

    name
}

/// Returns whether `name` is the name of a pseudo-element that can be written
/// with pseudo-class syntax (`:before`, `:after`, `:first-line`, or
/// `:first-letter`)
fn is_fake_pseudo_element(name: &str) -> bool {
    match name.as_bytes()[0] {
        b'a' | b'A' => name.to_ascii_lowercase() == "after",
        b'b' | b'B' => name.to_ascii_lowercase() == "before",
        b'f' | b'F' => match name.to_ascii_lowercase().as_str() {
            "first-line" | "first-letter" => true,
            _ => false,
        },
        _ => false,
    }
}

fn expect_identifier(s: &str) -> SassResult<()> {
    todo!()
}
