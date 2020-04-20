use peekmore::PeekMoreIterator;

use codemap::Spanned;

use crate::error::SassResult;
use crate::scope::Scope;
use crate::selector::Selector;
use crate::utils::{
    devour_whitespace, devour_whitespace_or_comment, eat_ident,
    read_until_semicolon_or_open_or_closing_curly_brace,
};
use crate::value::Value;
use crate::{Expr, Token};

/// A style: `color: red`
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Style {
    pub property: String,
    pub value: Spanned<Value>,
}

impl Style {
    pub fn parse_property<I: Iterator<Item = Token>>(
        toks: &mut PeekMoreIterator<I>,
        scope: &Scope,
        super_selector: &Selector,
        super_property: String,
    ) -> SassResult<String> {
        StyleParser::new(scope, super_selector).parse_property(toks, super_property)
    }

    pub fn to_string(&self) -> SassResult<String> {
        Ok(format!(
            "{}: {};",
            self.property,
            self.value.node.to_css_string(self.value.span)?
        ))
    }

    pub(crate) fn eval(self) -> SassResult<Self> {
        Ok(Style {
            property: self.property,
            value: Spanned {
                span: self.value.span,
                node: self.value.node.eval(self.value.span)?.node,
            },
        })
    }

    pub fn parse_value<I: Iterator<Item = Token>>(
        toks: &mut PeekMoreIterator<I>,
        scope: &Scope,
        super_selector: &Selector,
    ) -> SassResult<Spanned<Value>> {
        StyleParser::new(scope, super_selector).parse_style_value(toks, scope)
    }

    pub fn from_tokens<I: Iterator<Item = Token>>(
        toks: &mut PeekMoreIterator<I>,
        scope: &Scope,
        super_selector: &Selector,
        super_property: String,
    ) -> SassResult<Expr> {
        StyleParser::new(scope, super_selector).eat_style_group(toks, super_property, scope)
    }
}

struct StyleParser<'a> {
    scope: &'a Scope,
    super_selector: &'a Selector,
}

impl<'a> StyleParser<'a> {
    const fn new(scope: &'a Scope, super_selector: &'a Selector) -> Self {
        StyleParser {
            scope,
            super_selector,
        }
    }

    pub(crate) fn parse_style_value<I: Iterator<Item = Token>>(
        &self,
        toks: &mut PeekMoreIterator<I>,
        scope: &Scope,
    ) -> SassResult<Spanned<Value>> {
        devour_whitespace(toks);
        Value::from_vec(
            read_until_semicolon_or_open_or_closing_curly_brace(toks),
            scope,
            self.super_selector,
        )
    }

    pub(crate) fn eat_style_group<I: Iterator<Item = Token>>(
        &self,
        toks: &mut PeekMoreIterator<I>,
        super_property: String,
        scope: &Scope,
    ) -> SassResult<Expr> {
        let mut styles = Vec::new();
        devour_whitespace(toks);
        while let Some(tok) = toks.peek() {
            match tok.kind {
                '{' => {
                    toks.next();
                    devour_whitespace(toks);
                    loop {
                        let property = self.parse_property(toks, super_property.clone())?;
                        if let Some(tok) = toks.peek() {
                            if tok.kind == '{' {
                                match self.eat_style_group(toks, property, scope)? {
                                    Expr::Styles(s) => styles.extend(s),
                                    Expr::Style(s) => styles.push(*s),
                                    _ => unreachable!(),
                                }
                                devour_whitespace(toks);
                                if let Some(tok) = toks.peek() {
                                    if tok.kind == '}' {
                                        toks.next();
                                        devour_whitespace(toks);
                                        return Ok(Expr::Styles(styles));
                                    } else {
                                        continue;
                                    }
                                }
                                continue;
                            }
                        }
                        let value = self.parse_style_value(toks, scope)?;
                        match toks.peek().unwrap().kind {
                            '}' => {
                                styles.push(Style { property, value });
                            }
                            ';' => {
                                toks.next();
                                devour_whitespace(toks);
                                styles.push(Style { property, value });
                            }
                            '{' => {
                                styles.push(Style {
                                    property: property.clone(),
                                    value,
                                });
                                match self.eat_style_group(toks, property, scope)? {
                                    Expr::Style(s) => styles.push(*s),
                                    Expr::Styles(s) => styles.extend(s),
                                    _ => unreachable!(),
                                }
                            }
                            _ => {
                                devour_whitespace(toks);
                                styles.push(Style { property, value });
                            }
                        }
                        if let Some(tok) = toks.peek() {
                            match tok.kind {
                                '}' => {
                                    toks.next();
                                    devour_whitespace(toks);
                                    return Ok(Expr::Styles(styles));
                                }
                                _ => continue,
                            }
                        }
                    }
                }
                _ => {
                    let value = self.parse_style_value(toks, scope)?;
                    let t = toks.peek().ok_or(("expected more input.", value.span))?;
                    match t.kind {
                        '}' => {}
                        ';' => {
                            toks.next();
                            devour_whitespace(toks);
                        }
                        '{' => {
                            let mut v = vec![Style {
                                property: super_property.clone(),
                                value,
                            }];
                            match self.eat_style_group(toks, super_property, scope)? {
                                Expr::Style(s) => v.push(*s),
                                Expr::Styles(s) => v.extend(s),
                                _ => unreachable!(),
                            }
                            return Ok(Expr::Styles(v));
                        }
                        _ => {}
                    }
                    return Ok(Expr::Style(Box::new(Style {
                        property: super_property,
                        value,
                    })));
                }
            }
        }
        Ok(Expr::Styles(styles))
    }

    pub(crate) fn parse_property<I: Iterator<Item = Token>>(
        &self,
        toks: &mut PeekMoreIterator<I>,
        mut super_property: String,
    ) -> SassResult<String> {
        devour_whitespace(toks);
        let property = eat_ident(toks, self.scope, self.super_selector)?.node;
        devour_whitespace_or_comment(toks)?;
        if toks.peek().is_some() && toks.peek().unwrap().kind == ':' {
            toks.next();
            devour_whitespace_or_comment(toks)?;
        }

        if super_property.is_empty() {
            Ok(property)
        } else {
            super_property.reserve(1 + property.len());
            super_property.push('-');
            super_property.push_str(&property);
            Ok(super_property)
        }
    }
}
