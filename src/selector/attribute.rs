use std::fmt::{self, Display, Write};

use peekmore::PeekMoreIterator;

use codemap::Span;

use super::{Namespace, QualifiedName, Selector};
use crate::common::QuoteKind;
use crate::error::SassResult;
use crate::scope::Scope;
use crate::utils::{devour_whitespace, eat_ident, is_ident, parse_quoted_string};
use crate::value::Value;
use crate::Token;

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Attribute {
    attr: QualifiedName,
    value: String,
    modifier: Option<char>,
    op: AttributeOp,
    span: Span,
}

fn attribute_name<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    scope: &Scope,
    super_selector: &Selector,
    start: Span,
) -> SassResult<QualifiedName> {
    let next = toks.peek().ok_or(("Expected identifier.", start))?;
    if next.kind == '*' {
        let pos = next.pos;
        toks.next();
        if toks.peek().ok_or(("expected \"|\".", pos))?.kind != '|' {
            return Err(("expected \"|\".", pos).into());
        }

        let span_before = toks.next().unwrap().pos();

        let ident = eat_ident(toks, scope, super_selector, span_before)?.node;
        return Ok(QualifiedName {
            ident,
            namespace: Namespace::Asterisk,
        });
    }
    let span_before = next.pos;
    let name_or_namespace = eat_ident(toks, scope, super_selector, span_before)?;
    match toks.peek() {
        Some(v) if v.kind != '|' => {
            return Ok(QualifiedName {
                ident: name_or_namespace.node,
                namespace: Namespace::None,
            });
        }
        Some(..) => {}
        None => return Err(("expected more input.", name_or_namespace.span).into()),
    }
    match toks.peek_forward(1) {
        Some(v) if v.kind == '=' => {
            toks.reset_view();
            return Ok(QualifiedName {
                ident: name_or_namespace.node,
                namespace: Namespace::None,
            });
        }
        Some(..) => {
            toks.reset_view();
        }
        None => return Err(("expected more input.", name_or_namespace.span).into()),
    }
    let span_before = toks.next().unwrap().pos();
    let ident = eat_ident(toks, scope, super_selector, span_before)?.node;
    Ok(QualifiedName {
        ident,
        namespace: Namespace::Other(name_or_namespace.node),
    })
}

fn attribute_operator<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    start: Span,
) -> SassResult<AttributeOp> {
    let op = match toks.next().ok_or(("Expected \"]\".", start))?.kind {
        '=' => return Ok(AttributeOp::Equals),
        '~' => AttributeOp::Include,
        '|' => AttributeOp::Dash,
        '^' => AttributeOp::Prefix,
        '$' => AttributeOp::Suffix,
        '*' => AttributeOp::Contains,
        _ => return Err(("Expected \"]\".", start).into()),
    };
    if toks.next().ok_or(("expected \"=\".", start))?.kind != '=' {
        return Err(("expected \"=\".", start).into());
    }
    Ok(op)
}
impl Attribute {
    pub fn from_tokens<I: Iterator<Item = Token>>(
        toks: &mut PeekMoreIterator<I>,
        scope: &Scope,
        super_selector: &Selector,
        start: Span,
    ) -> SassResult<Attribute> {
        devour_whitespace(toks);
        let attr = attribute_name(toks, scope, super_selector, start)?;
        devour_whitespace(toks);
        if toks.peek().ok_or(("expected more input.", start))?.kind == ']' {
            toks.next();
            return Ok(Attribute {
                attr,
                value: String::new(),
                modifier: None,
                op: AttributeOp::Any,
                span: start,
            });
        }

        let op = attribute_operator(toks, start)?;
        devour_whitespace(toks);

        let peek = toks.peek().ok_or(("expected more input.", start))?;
        let span_before = peek.pos;
        let value = match peek.kind {
            q @ '\'' | q @ '"' => {
                toks.next();
                match parse_quoted_string(toks, scope, q, super_selector, span_before)?.node {
                    Value::String(s, ..) => s,
                    _ => unreachable!(),
                }
            }
            _ => eat_ident(toks, scope, super_selector, span_before)?.node,
        };
        devour_whitespace(toks);

        let peek = toks.peek().ok_or(("expected more input.", start))?;

        let modifier = match peek.kind {
            c if c.is_alphabetic() => Some(c),
            _ => None,
        };

        let pos = peek.pos();

        if modifier.is_some() {
            toks.next();
            devour_whitespace(toks);
        }

        if toks.peek().ok_or(("expected \"]\".", pos))?.kind != ']' {
            return Err(("expected \"]\".", pos).into());
        }

        toks.next();

        Ok(Attribute {
            op,
            attr,
            value,
            modifier,
            span: start,
        })
    }
}

impl Display for Attribute {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_char('[')?;
        write!(f, "{}", self.attr)?;

        if self.op != AttributeOp::Any {
            f.write_str(self.op.into())?;
            if is_ident(&self.value) && !self.value.starts_with("--") {
                f.write_str(&self.value)?;

                if self.modifier.is_some() {
                    f.write_char(' ')?;
                }
            } else {
                // todo: remove unwrap by not doing this in display
                // or having special emitter for quoted strings?
                // (also avoids the clone because we can consume/modify self)
                f.write_str(
                    &Value::String(self.value.clone(), QuoteKind::Quoted)
                        .to_css_string(self.span)
                        .unwrap(),
                )?;
                // todo: this space is not emitted when `compressed` output
                if self.modifier.is_some() {
                    f.write_char(' ')?;
                }
            }

            if let Some(c) = self.modifier {
                f.write_char(c)?;
            }
        }

        f.write_char(']')?;

        Ok(())
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum AttributeOp {
    /// \[attr\]
    ///
    /// Represents elements with an attribute name of `attr`
    Any,

    /// [attr=value]
    ///
    /// Represents elements with an attribute name of `attr`
    /// whose value is exactly `value`
    Equals,

    /// [attr~=value]
    ///
    /// Represents elements with an attribute name of `attr`
    /// whose value is a whitespace-separated list of words,
    /// one of which is exactly `value`
    Include,

    /// [attr|=value]
    ///
    /// Represents elements with an attribute name of `attr`
    /// whose value can be exactly value or can begin with
    /// `value` immediately followed by a hyphen (`-`)
    Dash,

    /// [attr^=value]
    Prefix,

    /// [attr$=value]
    Suffix,

    /// [attr*=value]
    ///
    /// Represents elements with an attribute name of `attr`
    /// whose value contains at least one occurrence of
    /// `value` within the string
    Contains,
}

impl Into<&'static str> for AttributeOp {
    fn into(self) -> &'static str {
        match self {
            Self::Any => "",
            Self::Equals => "=",
            Self::Include => "~=",
            Self::Dash => "|=",
            Self::Prefix => "^=",
            Self::Suffix => "$=",
            Self::Contains => "*=",
        }
    }
}
