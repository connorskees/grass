use std::borrow::Cow;
use std::iter::Iterator;

use codemap::{Span, Spanned};

use crate::color::Color;
use crate::common::{Brackets, ListSeparator, Op, QuoteKind};
use crate::error::SassResult;
use crate::interner::InternedString;
use crate::unit::Unit;
use crate::utils::hex_char_for;

use css_function::is_special_function;
pub(crate) use map::SassMap;
pub(crate) use number::Number;
pub(crate) use sass_function::SassFunction;

mod css_function;
mod map;
mod number;
mod ops;
mod parse;
mod sass_function;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Value {
    Important,
    True,
    False,
    Null,
    Dimension(Number, Unit),
    List(Vec<Value>, ListSeparator, Brackets),
    Color(Box<Color>),
    UnaryOp(Op, Box<Value>),
    BinaryOp(Box<Value>, Op, Box<Value>),
    Paren(Box<Value>),
    Ident(InternedString, QuoteKind),
    Map(SassMap),
    ArgList(Vec<Spanned<Value>>),
    /// Returned by `get-function()`
    Function(SassFunction),
}

fn visit_quoted_string(buf: &mut String, force_double_quote: bool, string: &str) -> SassResult<()> {
    let mut has_single_quote = false;
    let mut has_double_quote = false;

    let mut buffer = String::new();

    if force_double_quote {
        buffer.push('"');
    }
    let mut iter = string.chars().peekable();
    while let Some(c) = iter.next() {
        match c {
            '\'' => {
                if force_double_quote {
                    buffer.push('\'');
                } else if has_double_quote {
                    return visit_quoted_string(buf, true, string);
                } else {
                    has_single_quote = true;
                    buffer.push('\'');
                }
            }
            '"' => {
                if force_double_quote {
                    buffer.push('\\');
                    buffer.push('"');
                } else if has_single_quote {
                    return visit_quoted_string(buf, true, string);
                } else {
                    has_double_quote = true;
                    buffer.push('"');
                }
            }
            '\x00'..='\x08' | '\x0A'..='\x1F' => {
                buffer.push('\\');
                if c as u32 > 0xF {
                    buffer.push(hex_char_for(c as u32 >> 4))
                }
                buffer.push(hex_char_for(c as u32 & 0xF));
                if iter.peek().is_none() {
                    break;
                }

                let next = iter.peek().unwrap();

                if next.is_ascii_hexdigit() || next == &' ' || next == &'\t' {
                    buffer.push(' ');
                }
            }
            '\\' => {
                buffer.push('\\');
                buffer.push('\\');
            }
            _ => buffer.push(c),
        }
    }

    if force_double_quote {
        buffer.push('"');
    } else {
        let quote = if has_double_quote { '\'' } else { '"' };
        buffer = format!("{}{}{}", quote, buffer, quote);
    }
    buf.push_str(&buffer);
    Ok(())
}

impl Value {
    pub fn is_null(&self, span: Span) -> SassResult<bool> {
        Ok(match self {
            &Value::Null => true,
            Value::Ident(i, QuoteKind::None) if i.is_empty() => true,
            Self::BinaryOp(..) | Self::Paren(..) | Self::UnaryOp(..) => {
                self.clone().eval(span)?.is_null(span)?
            }
            Self::List(v, _, Brackets::Bracketed) if v.is_empty() => false,
            Self::List(v, ..) => v.iter().all(|f| f.is_null(span).unwrap()),
            _ => false,
        })
    }

    pub fn to_css_string(&self, span: Span) -> SassResult<Cow<'static, str>> {
        Ok(match self {
            Self::Important => Cow::Borrowed("!important"),
            Self::Dimension(num, unit) => match unit {
                Unit::Mul(..) => {
                    return Err((format!("{}{} isn't a valid CSS value.", num, unit), span).into());
                }
                _ => Cow::Owned(format!("{}{}", num, unit)),
            },
            Self::Map(..) => {
                return Err((
                    format!("{} isn't a valid CSS value.", self.inspect(span)?),
                    span,
                )
                    .into())
            }
            Self::Function(..) => {
                return Err((
                    format!("{} isn't a valid CSS value.", self.inspect(span)?),
                    span,
                )
                    .into())
            }
            Self::List(vals, sep, brackets) => match brackets {
                Brackets::None => Cow::Owned(
                    vals.iter()
                        .filter(|x| !x.is_null(span).unwrap())
                        .map(|x| x.to_css_string(span))
                        .collect::<SassResult<Vec<Cow<'static, str>>>>()?
                        .join(sep.as_str()),
                ),
                Brackets::Bracketed => Cow::Owned(format!(
                    "[{}]",
                    vals.iter()
                        .filter(|x| !x.is_null(span).unwrap())
                        .map(|x| x.to_css_string(span))
                        .collect::<SassResult<Vec<Cow<'static, str>>>>()?
                        .join(sep.as_str()),
                )),
            },
            Self::Color(c) => Cow::Owned(c.to_string()),
            Self::UnaryOp(..) | Self::BinaryOp(..) => {
                self.clone().eval(span)?.to_css_string(span)?
            }
            Self::Paren(val) => val.to_css_string(span)?,
            Self::Ident(string, QuoteKind::None) => {
                let string = string.resolve();
                let mut after_newline = false;
                let mut buf = String::with_capacity(string.len());
                for c in string.chars() {
                    match c {
                        '\n' => {
                            buf.push(' ');
                            after_newline = true;
                        }
                        ' ' => {
                            if !after_newline {
                                buf.push(' ');
                            }
                        }
                        _ => {
                            buf.push(c);
                            after_newline = false;
                        }
                    }
                }
                Cow::Owned(buf)
            }
            Self::Ident(string, QuoteKind::Quoted) => {
                let string = string.resolve();
                let mut buf = String::with_capacity(string.len());
                visit_quoted_string(&mut buf, false, &string)?;
                Cow::Owned(buf)
            }
            Self::True => Cow::Borrowed("true"),
            Self::False => Cow::Borrowed("false"),
            Self::Null => Cow::Borrowed(""),
            Self::ArgList(args) => Cow::Owned(
                args.iter()
                    .filter(|x| !x.is_null(span).unwrap())
                    .map(|a| Ok(a.node.to_css_string(span)?.into()))
                    .collect::<SassResult<Vec<String>>>()?
                    .join(", "),
            ),
        })
    }

    pub fn is_true(&self, span: Span) -> SassResult<bool> {
        match self {
            Value::Null | Value::False => Ok(false),
            Self::BinaryOp(..) | Self::Paren(..) | Self::UnaryOp(..) => {
                self.clone().eval(span)?.is_true(span)
            }
            _ => Ok(true),
        }
    }

    pub fn unquote(self) -> Self {
        match self {
            Self::Ident(s1, _) => Self::Ident(s1, QuoteKind::None),
            Self::List(v, sep, bracket) => {
                Self::List(v.into_iter().map(Value::unquote).collect(), sep, bracket)
            }
            v => v,
        }
    }

    pub const fn span(self, span: Span) -> Spanned<Self> {
        Spanned { node: self, span }
    }

    pub fn kind(&self, span: Span) -> SassResult<&'static str> {
        match self {
            Self::Color(..) => Ok("color"),
            Self::Ident(..) | Self::Important => Ok("string"),
            Self::Dimension(..) => Ok("number"),
            Self::List(..) => Ok("list"),
            Self::Function(..) => Ok("function"),
            Self::ArgList(..) => Ok("arglist"),
            Self::True | Self::False => Ok("bool"),
            Self::Null => Ok("null"),
            Self::Map(..) => Ok("map"),
            Self::BinaryOp(..) | Self::Paren(..) | Self::UnaryOp(..) => {
                self.clone().eval(span)?.kind(span)
            }
        }
    }

    pub fn is_special_function(&self) -> bool {
        match self {
            Self::Ident(s, QuoteKind::None) => is_special_function(&s.resolve()),
            _ => false,
        }
    }

    pub fn bool(b: bool) -> Self {
        if b {
            Value::True
        } else {
            Value::False
        }
    }

    // TODO:
    // https://github.com/sass/dart-sass/blob/d4adea7569832f10e3a26d0e420ae51640740cfb/lib/src/ast/sass/expression/list.dart#L39
    pub fn inspect(&self, span: Span) -> SassResult<Cow<'static, str>> {
        Ok(match self {
            Value::List(v, _, brackets) if v.is_empty() => match brackets {
                Brackets::None => Cow::Borrowed("()"),
                Brackets::Bracketed => Cow::Borrowed("[]"),
            },
            Value::List(v, sep, brackets) if v.len() == 1 => match brackets {
                Brackets::None => match sep {
                    ListSeparator::Space => v[0].inspect(span)?,
                    ListSeparator::Comma => Cow::Owned(format!("({},)", v[0].inspect(span)?)),
                },
                Brackets::Bracketed => match sep {
                    ListSeparator::Space => Cow::Owned(format!("[{}]", v[0].inspect(span)?)),
                    ListSeparator::Comma => Cow::Owned(format!("[{},]", v[0].inspect(span)?)),
                },
            },
            Self::List(vals, sep, brackets) => Cow::Owned(match brackets {
                Brackets::None => vals
                    .iter()
                    .map(|x| x.inspect(span))
                    .collect::<SassResult<Vec<Cow<'static, str>>>>()?
                    .join(sep.as_str()),
                Brackets::Bracketed => format!(
                    "[{}]",
                    vals.iter()
                        .map(|x| x.inspect(span))
                        .collect::<SassResult<Vec<Cow<'static, str>>>>()?
                        .join(sep.as_str()),
                ),
            }),
            Value::Function(f) => Cow::Owned(format!("get-function(\"{}\")", f.name())),
            Value::Null => Cow::Borrowed("null"),
            Value::Map(map) => Cow::Owned(format!(
                "({})",
                map.iter()
                    .map(|(k, v)| Ok(format!(
                        "{}: {}",
                        k.to_css_string(span)?,
                        v.to_css_string(span)?
                    )))
                    .collect::<SassResult<Vec<String>>>()?
                    .join(", ")
            )),
            Value::Paren(v) => v.inspect(span)?,
            v => v.to_css_string(span)?,
        })
    }
}
