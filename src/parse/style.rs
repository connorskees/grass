use codemap::Spanned;

use crate::{
    error::SassResult,
    style::Style,
    utils::{is_name, is_name_start},
    value::Value,
    Token,
};

use super::common::SelectorOrStyle;

use super::Parser;

impl<'a> Parser<'a> {
    /// Determines whether the parser is looking at a style or a selector
    ///
    /// When parsing the children of a style rule, property declarations,
    /// namespaced variable declarations, and nested style rules can all begin
    /// with bare identifiers. In order to know which statement type to produce,
    /// we need to disambiguate them. We use the following criteria:
    ///
    /// * If the entity starts with an identifier followed by a period and a
    ///   dollar sign, it's a variable declaration. This is the simplest case,
    ///   because `.$` is used in and only in variable declarations.
    ///
    /// * If the entity doesn't start with an identifier followed by a colon,
    ///   it's a selector. There are some additional mostly-unimportant cases
    ///   here to support various declaration hacks.
    ///
    /// * If the colon is followed by another colon, it's a selector.
    ///
    /// * Otherwise, if the colon is followed by anything other than
    ///   interpolation or a character that's valid as the beginning of an
    ///   identifier, it's a declaration.
    ///
    /// * If the colon is followed by interpolation or a valid identifier, try
    ///   parsing it as a declaration value. If this fails, backtrack and parse
    ///   it as a selector.
    ///
    /// * If the declaration value is valid but is followed by "{", backtrack and
    ///   parse it as a selector anyway. This ensures that ".foo:bar {" is always
    ///   parsed as a selector and never as a property with nested properties
    ///   beneath it.
    // todo: potentially we read the property to a string already since properties
    // are more common than selectors? this seems to be annihilating our performance
    fn parse_style_value_when_no_space_after_semicolon(&mut self) -> Option<Vec<Token>> {
        let mut toks = Vec::new();
        while let Some(tok) = self.toks.peek() {
            match tok.kind {
                ';' | '}' => {
                    self.toks.reset_view();
                    break;
                }
                '{' => {
                    self.toks.reset_view();
                    return None;
                }
                '(' => {
                    toks.push(*tok);
                    self.toks.peek_forward(1);
                    let mut scope = 0;
                    while let Some(tok) = self.toks.peek() {
                        match tok.kind {
                            ')' => {
                                if scope == 0 {
                                    toks.push(*tok);
                                    self.toks.peek_forward(1);
                                    break;
                                } else {
                                    scope -= 1;
                                    toks.push(*tok);
                                    self.toks.peek_forward(1);
                                }
                            }
                            '(' => {
                                toks.push(*tok);
                                self.toks.peek_forward(1);
                                scope += 1;
                            }
                            _ => {
                                toks.push(*tok);
                                self.toks.peek_forward(1);
                            }
                        }
                    }
                }
                _ => {
                    toks.push(*tok);
                    self.toks.peek_forward(1);
                }
            }
        }
        Some(toks)
    }

    pub(super) fn is_selector_or_style(&mut self) -> SassResult<SelectorOrStyle> {
        if let Some(first_char) = self.toks.peek() {
            if first_char.kind == '#' {
                if !matches!(self.toks.peek_forward(1), Some(Token { kind: '{', .. })) {
                    self.toks.reset_view();
                    return Ok(SelectorOrStyle::Selector(String::new()));
                }
                self.toks.reset_view();
            } else if !is_name_start(first_char.kind) && first_char.kind != '-' {
                return Ok(SelectorOrStyle::Selector(String::new()));
            }
        }

        let mut property = self.parse_identifier()?.node;
        let whitespace_after_property = self.whitespace();

        if let Some(Token { kind: ':', .. }) = self.toks.peek() {
            self.toks.next();
            if let Some(Token { kind, .. }) = self.toks.peek() {
                return Ok(match kind {
                    ':' => {
                        if whitespace_after_property {
                            property.push(' ');
                        }
                        property.push(':');
                        SelectorOrStyle::Selector(property)
                    }
                    c if is_name(*c) => {
                        if let Some(toks) = self.parse_style_value_when_no_space_after_semicolon() {
                            let len = toks.len();
                            if let Ok(val) = self.parse_value_from_vec(toks) {
                                self.toks.take(len).for_each(drop);
                                return Ok(SelectorOrStyle::Style(property, Some(Box::new(val))));
                            }
                        }

                        if whitespace_after_property {
                            property.push(' ');
                        }
                        property.push(':');
                        return Ok(SelectorOrStyle::Selector(property));
                    }
                    _ => SelectorOrStyle::Style(property, None),
                });
            }
        } else {
            if whitespace_after_property {
                property.push(' ');
            }
            return Ok(SelectorOrStyle::Selector(property));
        }
        Err(("expected \"{\".", self.span_before).into())
    }

    fn parse_property(&mut self, mut super_property: String) -> SassResult<String> {
        let property = self.parse_identifier()?;
        self.whitespace_or_comment();
        if let Some(Token { kind: ':', .. }) = self.toks.peek() {
            self.toks.next();
            self.whitespace_or_comment();
        } else {
            return Err(("Expected \":\".", property.span).into());
        }

        if super_property.is_empty() {
            Ok(property.node)
        } else {
            super_property.reserve(1 + property.node.len());
            super_property.push('-');
            super_property.push_str(&property.node);
            Ok(super_property)
        }
    }

    fn parse_style_value(&mut self) -> SassResult<Spanned<Value>> {
        self.parse_value()
    }

    pub(super) fn parse_style_group(&mut self, super_property: String) -> SassResult<Vec<Style>> {
        let mut styles = Vec::new();
        self.whitespace();
        while let Some(tok) = self.toks.peek().cloned() {
            match tok.kind {
                '{' => {
                    self.toks.next();
                    self.whitespace();
                    loop {
                        let property = self.parse_property(super_property.clone())?;
                        if let Some(tok) = self.toks.peek() {
                            if tok.kind == '{' {
                                styles.append(&mut self.parse_style_group(property)?);
                                self.whitespace();
                                if let Some(tok) = self.toks.peek() {
                                    if tok.kind == '}' {
                                        self.toks.next();
                                        self.whitespace();
                                        return Ok(styles);
                                    } else {
                                        continue;
                                    }
                                }
                                continue;
                            }
                        }
                        let value = self.parse_style_value()?;
                        match self.toks.peek() {
                            Some(Token { kind: '}', .. }) => {
                                styles.push(Style { property, value });
                            }
                            Some(Token { kind: ';', .. }) => {
                                self.toks.next();
                                self.whitespace();
                                styles.push(Style { property, value });
                            }
                            Some(Token { kind: '{', .. }) => {
                                styles.push(Style {
                                    property: property.clone(),
                                    value,
                                });
                                styles.append(&mut self.parse_style_group(property)?);
                            }
                            Some(..) | None => {
                                self.whitespace();
                                styles.push(Style { property, value });
                            }
                        }
                        if let Some(tok) = self.toks.peek() {
                            match tok.kind {
                                '}' => {
                                    self.toks.next();
                                    self.whitespace();
                                    return Ok(styles);
                                }
                                _ => continue,
                            }
                        }
                    }
                }
                _ => {
                    let value = self.parse_style_value()?;
                    let t = self
                        .toks
                        .peek()
                        .ok_or(("expected more input.", value.span))?;
                    match t.kind {
                        ';' => {
                            self.toks.next();
                            self.whitespace();
                        }
                        '{' => {
                            let mut v = vec![Style {
                                property: super_property.clone(),
                                value,
                            }];
                            v.append(&mut self.parse_style_group(super_property)?);
                            return Ok(v);
                        }
                        _ => {}
                    }
                    return Ok(vec![Style {
                        property: super_property,
                        value,
                    }]);
                }
            }
        }
        Ok(styles)
    }
}
