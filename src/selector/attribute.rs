use std::{
    fmt::{self, Display, Write},
    hash::{Hash, Hasher},
};

use codemap::Span;

use crate::{common::QuoteKind, error::SassResult, parse::Parser, utils::is_ident, value::Value};

use super::{Namespace, QualifiedName};

#[derive(Clone, Debug)]
pub(crate) struct Attribute {
    attr: QualifiedName,
    value: String,
    modifier: Option<char>,
    op: AttributeOp,
    span: Span,
}

impl PartialEq for Attribute {
    fn eq(&self, other: &Self) -> bool {
        self.attr == other.attr
            && self.value == other.value
            && self.modifier == other.modifier
            && self.op == other.op
    }
}

impl Eq for Attribute {}

impl Hash for Attribute {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.attr.hash(state);
        self.value.hash(state);
        self.modifier.hash(state);
        self.op.hash(state);
    }
}

fn attribute_name(parser: &mut Parser<'_>, start: Span) -> SassResult<QualifiedName> {
    let next = parser.toks.peek().ok_or(("Expected identifier.", start))?;
    if next.kind == '*' {
        let pos = next.pos;
        parser.toks.next();
        if parser.toks.peek().ok_or(("expected \"|\".", pos))?.kind != '|' {
            return Err(("expected \"|\".", pos).into());
        }

        parser.span_before = parser.toks.next().unwrap().pos();

        let ident = parser.parse_identifier()?.node;
        return Ok(QualifiedName {
            ident,
            namespace: Namespace::Asterisk,
        });
    }
    parser.span_before = next.pos;
    let name_or_namespace = parser.parse_identifier()?;
    match parser.toks.peek() {
        Some(v) if v.kind != '|' => {
            return Ok(QualifiedName {
                ident: name_or_namespace.node,
                namespace: Namespace::None,
            });
        }
        Some(..) => {}
        None => return Err(("expected more input.", name_or_namespace.span).into()),
    }
    match parser.toks.peek_forward(1) {
        Some(v) if v.kind == '=' => {
            parser.toks.reset_cursor();
            return Ok(QualifiedName {
                ident: name_or_namespace.node,
                namespace: Namespace::None,
            });
        }
        Some(..) => {
            parser.toks.reset_cursor();
        }
        None => return Err(("expected more input.", name_or_namespace.span).into()),
    }
    parser.span_before = parser.toks.next().unwrap().pos();
    let ident = parser.parse_identifier()?.node;
    Ok(QualifiedName {
        ident,
        namespace: Namespace::Other(name_or_namespace.node),
    })
}

fn attribute_operator(parser: &mut Parser<'_>) -> SassResult<AttributeOp> {
    let start = parser.span_before;
    let op = match parser.toks.next().ok_or(("Expected \"]\".", start))?.kind {
        '=' => return Ok(AttributeOp::Equals),
        '~' => AttributeOp::Include,
        '|' => AttributeOp::Dash,
        '^' => AttributeOp::Prefix,
        '$' => AttributeOp::Suffix,
        '*' => AttributeOp::Contains,
        _ => return Err(("Expected \"]\".", start).into()),
    };
    if parser.toks.next().ok_or(("expected \"=\".", start))?.kind != '=' {
        return Err(("expected \"=\".", start).into());
    }
    Ok(op)
}
impl Attribute {
    pub fn from_tokens(parser: &mut Parser<'_>) -> SassResult<Attribute> {
        let start = parser.span_before;
        parser.whitespace();
        let attr = attribute_name(parser, start)?;
        parser.whitespace();
        if parser
            .toks
            .peek()
            .ok_or(("expected more input.", start))?
            .kind
            == ']'
        {
            parser.toks.next();
            return Ok(Attribute {
                attr,
                value: String::new(),
                modifier: None,
                op: AttributeOp::Any,
                span: start,
            });
        }

        parser.span_before = start;
        let op = attribute_operator(parser)?;
        parser.whitespace();

        let peek = parser.toks.peek().ok_or(("expected more input.", start))?;
        parser.span_before = peek.pos;
        let value = match peek.kind {
            q @ '\'' | q @ '"' => {
                parser.toks.next();
                match parser.parse_quoted_string(q)?.node {
                    Value::String(s, ..) => s,
                    _ => unreachable!(),
                }
            }
            _ => parser.parse_identifier()?.node,
        };
        parser.whitespace();

        let peek = parser.toks.peek().ok_or(("expected more input.", start))?;

        let modifier = match peek.kind {
            c if c.is_alphabetic() => Some(c),
            _ => None,
        };

        let pos = peek.pos();

        if modifier.is_some() {
            parser.toks.next();
            parser.whitespace();
        }

        if parser.toks.peek().ok_or(("expected \"]\".", pos))?.kind != ']' {
            return Err(("expected \"]\".", pos).into());
        }

        parser.toks.next();

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

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
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
