use std::{borrow::Cow, cmp::Ordering};

use codemap::{Span, Spanned};

use crate::{
    color::Color,
    common::{BinaryOp, Brackets, ListSeparator, QuoteKind},
    error::SassResult,
    evaluate::Visitor,
    selector::Selector,
    serializer::{serialize_color, serialize_number},
    unit::Unit,
    utils::{hex_char_for, is_special_function},
    Options, OutputStyle,
};

pub(crate) use arglist::ArgList;
pub(crate) use calculation::*;
pub(crate) use map::SassMap;
pub(crate) use number::*;
pub(crate) use sass_function::{SassFunction, UserDefinedFunction};
pub(crate) use sass_number::SassNumber;

mod arglist;
mod calculation;
mod map;
mod number;
mod sass_function;
mod sass_number;

#[derive(Debug, Clone)]
pub(crate) enum Value {
    True,
    False,
    Null,
    Dimension {
        num: Number,
        unit: Unit,
        as_slash: Option<Box<(SassNumber, SassNumber)>>,
    },
    List(Vec<Value>, ListSeparator, Brackets),
    // todo: benchmark unboxing this, now that it's smaller
    Color(Box<Color>),
    String(String, QuoteKind),
    Map(SassMap),
    ArgList(ArgList),
    /// Returned by `get-function()`
    // todo: benchmark boxing this (function refs are infrequent)
    FunctionRef(SassFunction),
    Calculation(SassCalculation),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Value::Calculation(calc1) => match other {
                Value::Calculation(calc2) => calc1 == calc2,
                _ => false,
            },
            Value::String(s1, ..) => match other {
                Value::String(s2, ..) => s1 == s2,
                _ => false,
            },
            Value::Dimension {
                num: n,
                unit,
                as_slash: _,
            } if !n.is_nan() => match other {
                Value::Dimension {
                    num: n2,
                    unit: unit2,
                    as_slash: _,
                } if !n.is_nan() => {
                    if !unit.comparable(unit2) {
                        false
                    } else if unit == unit2 {
                        n == n2
                    } else if unit == &Unit::None || unit2 == &Unit::None {
                        false
                    } else {
                        *n == n2.convert(unit2, unit)
                    }
                }
                _ => false,
            },
            Value::Dimension { num: n, .. } => {
                debug_assert!(n.is_nan());
                false
            }
            Value::List(list1, sep1, brackets1) => match other {
                Value::List(list2, sep2, brackets2) => {
                    if sep1 != sep2 || brackets1 != brackets2 || list1.len() != list2.len() {
                        false
                    } else {
                        for (a, b) in list1.iter().zip(list2) {
                            if a != b {
                                return false;
                            }
                        }
                        true
                    }
                }
                _ => false,
            },
            Value::Null => matches!(other, Value::Null),
            Value::True => matches!(other, Value::True),
            Value::False => matches!(other, Value::False),
            Value::FunctionRef(fn1) => {
                if let Value::FunctionRef(fn2) = other {
                    fn1 == fn2
                } else {
                    false
                }
            }
            Value::Map(map1) => {
                if let Value::Map(map2) = other {
                    map1 == map2
                } else {
                    false
                }
            }
            Value::Color(color1) => {
                if let Value::Color(color2) = other {
                    color1 == color2
                } else {
                    false
                }
            }
            Value::ArgList(list1) => match other {
                Value::ArgList(list2) => list1 == list2,
                Value::List(list2, ListSeparator::Comma, ..) => {
                    if list1.len() != list2.len() {
                        return false;
                    }

                    for (el1, el2) in list1.elems.iter().zip(list2) {
                        if el1 != el2 {
                            return false;
                        }
                    }

                    true
                }
                _ => false,
            },
        }
    }
}

impl Eq for Value {}

fn visit_quoted_string(buf: &mut String, force_double_quote: bool, string: &str) {
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
                    buffer.push(hex_char_for(c as u32 >> 4));
                }
                buffer.push(hex_char_for(c as u32 & 0xF));

                let next = match iter.peek() {
                    Some(v) => v,
                    None => break,
                };

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
}

impl Value {
    pub fn with_slash(
        self,
        numerator: SassNumber,
        denom: SassNumber,
        span: Span,
    ) -> SassResult<Self> {
        let number = self.assert_number(span)?;
        Ok(Value::Dimension {
            num: Number(number.num),
            unit: number.unit,
            as_slash: Some(Box::new((numerator, denom))),
        })
    }

    pub fn assert_number(self, span: Span) -> SassResult<SassNumber> {
        match self {
            Value::Dimension {
                num,
                unit,
                as_slash,
            } => Ok(SassNumber {
                num: num.0,
                unit,
                as_slash,
            }),
            _ => Err((format!("{} is not a number.", self.inspect(span)?), span).into()),
        }
    }

    pub fn assert_number_with_name(self, span: Span, name: &str) -> SassResult<SassNumber> {
        match self {
            Value::Dimension {
                num,
                unit,
                as_slash,
            } => Ok(SassNumber {
                num: num.0,
                unit,
                as_slash,
            }),
            _ => Err((
                format!("${name}: \"{}\" is not a number.", self.inspect(span)?),
                span,
            )
                .into()),
        }
    }

    // todo: rename is_blank
    pub fn is_null(&self) -> bool {
        match self {
            Value::Null => true,
            Value::String(i, QuoteKind::None) if i.is_empty() => true,
            Value::List(_, _, Brackets::Bracketed) => false,
            Value::List(v, ..) => v.iter().map(Value::is_null).all(|f| f),
            Value::ArgList(v, ..) => v.is_null(),
            _ => false,
        }
    }

    pub fn is_empty_list(&self) -> bool {
        match self {
            Value::List(v, ..) => v.is_empty(),
            Value::Map(m) => m.is_empty(),
            Value::ArgList(v) => v.elems.is_empty(),
            _ => false,
        }
    }

    #[track_caller]
    pub fn to_css_string(&self, span: Span, is_compressed: bool) -> SassResult<Cow<'static, str>> {
        Ok(match self {
            Value::Calculation(calc) => Cow::Owned(format!(
                "{}({})",
                calc.name,
                calc.args
                    .iter()
                    .map(|a| a.to_css_string(span, is_compressed))
                    .collect::<SassResult<Vec<String>>>()?
                    .join(if is_compressed {
                        ListSeparator::Comma.as_compressed_str()
                    } else {
                        ListSeparator::Comma.as_str()
                    }),
            )),
            Value::Dimension {
                num,
                unit,
                as_slash,
            } => match unit {
                _ => {
                    // if let Some(as_slash) = as_slash {
                    //     let numer = &as_slash.0;
                    //     let denom = &as_slash.1;

                    //     return Ok(Cow::Owned(format!(
                    //         "{}/{}",
                    //         // todo: superfluous clones
                    //         Value::Dimension {
                    //             num: Number(numer.num),
                    //             unit: numer.unit.clone(),
                    //             as_slash: numer.as_slash.clone()
                    //         }
                    //         .to_css_string(span, is_compressed)?,
                    //         Value::Dimension {
                    //             num: Number(denom.num),
                    //             unit: denom.unit.clone(),
                    //             as_slash: denom.as_slash.clone()
                    //         }
                    //         .to_css_string(span, is_compressed)?,
                    //     )));
                    // }

                    Cow::Owned(serialize_number(
                        &SassNumber {
                            num: num.0,
                            unit: unit.clone(),
                            as_slash: as_slash.clone(),
                        },
                        &Options::default().style(if is_compressed {
                            OutputStyle::Compressed
                        } else {
                            OutputStyle::Expanded
                        }),
                        span,
                    )?)
                    // if unit.is_complex() {
                    //     return Err((format!("")))
                    // }
                    // Cow::Owned(format!("{}{}", num.to_string(is_compressed), unit))
                }
            },
            Value::Map(..) | Value::FunctionRef(..) => {
                return Err((
                    format!("{} isn't a valid CSS value.", self.inspect(span)?),
                    span,
                )
                    .into())
            }
            Value::List(vals, sep, brackets) => match brackets {
                Brackets::None => Cow::Owned(
                    vals.iter()
                        .filter(|x| !x.is_null())
                        .map(|x| x.to_css_string(span, is_compressed))
                        .collect::<SassResult<Vec<Cow<'static, str>>>>()?
                        .join(if is_compressed {
                            sep.as_compressed_str()
                        } else {
                            sep.as_str()
                        }),
                ),
                Brackets::Bracketed => Cow::Owned(format!(
                    "[{}]",
                    vals.iter()
                        .filter(|x| !x.is_null())
                        .map(|x| x.to_css_string(span, is_compressed))
                        .collect::<SassResult<Vec<Cow<'static, str>>>>()?
                        .join(if is_compressed {
                            sep.as_compressed_str()
                        } else {
                            sep.as_str()
                        }),
                )),
            },
            Value::Color(c) => Cow::Owned(serialize_color(
                c,
                &Options::default().style(if is_compressed {
                    OutputStyle::Compressed
                } else {
                    OutputStyle::Expanded
                }),
                span,
            )),
            Value::String(string, QuoteKind::None) => {
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
            Value::String(string, QuoteKind::Quoted) => {
                let mut buf = String::with_capacity(string.len());
                visit_quoted_string(&mut buf, false, string);
                Cow::Owned(buf)
            }
            Value::True => Cow::Borrowed("true"),
            Value::False => Cow::Borrowed("false"),
            Value::Null => Cow::Borrowed(""),
            Value::ArgList(args) if args.is_empty() => {
                return Err(("() isn't a valid CSS value.", span).into());
            }
            Value::ArgList(args) => Cow::Owned(
                args.elems
                    .iter()
                    .filter(|x| !x.is_null())
                    .map(|a| a.to_css_string(span, is_compressed))
                    .collect::<SassResult<Vec<Cow<'static, str>>>>()?
                    .join(if is_compressed {
                        ListSeparator::Comma.as_compressed_str()
                    } else {
                        ListSeparator::Comma.as_str()
                    }),
            ),
        })
    }

    pub fn is_true(&self) -> bool {
        !matches!(self, Value::Null | Value::False)
    }

    pub fn unquote(self) -> Self {
        match self {
            Value::String(s1, _) => Value::String(s1, QuoteKind::None),
            Value::List(v, sep, bracket) => {
                Value::List(v.into_iter().map(Value::unquote).collect(), sep, bracket)
            }
            v => v,
        }
    }

    pub const fn span(self, span: Span) -> Spanned<Self> {
        Spanned { node: self, span }
    }

    pub fn kind(&self) -> &'static str {
        match self {
            Value::Color(..) => "color",
            Value::String(..) => "string",
            Value::Calculation(..) => "calculation",
            Value::Dimension { .. } => "number",
            Value::List(..) => "list",
            Value::FunctionRef(..) => "function",
            Value::ArgList(..) => "arglist",
            Value::True | Value::False => "bool",
            Value::Null => "null",
            Value::Map(..) => "map",
        }
    }

    pub fn as_slash(&self) -> Option<Box<(SassNumber, SassNumber)>> {
        match self {
            Value::Dimension { as_slash, .. } => as_slash.clone(),
            _ => None,
        }
    }

    pub fn without_slash(self) -> Self {
        match self {
            Value::Dimension {
                num,
                unit,
                as_slash: _,
            } => Value::Dimension {
                num,
                unit,
                as_slash: None,
            },
            _ => self,
        }
    }

    pub fn is_color(&self) -> bool {
        matches!(self, Value::Color(..))
    }

    pub fn is_special_function(&self) -> bool {
        match self {
            Value::String(s, QuoteKind::None) => is_special_function(s),
            Value::Calculation(..) => true,
            _ => false,
        }
    }

    pub fn is_var(&self) -> bool {
        match self {
            Value::String(s, QuoteKind::None) => {
                if s.len() < "var(--_)".len() {
                    return false;
                }

                s.starts_with("var(")
            }
            Value::Calculation(..) => true,
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

    pub fn cmp(&self, other: &Self, span: Span, op: BinaryOp) -> SassResult<Ordering> {
        Ok(match self {
            Value::Dimension { num: n, .. } if n.is_nan() => todo!(),
            Value::Dimension {
                num,
                unit,
                as_slash: _,
            } => match &other {
                Value::Dimension { num: n, .. } if n.is_nan() => todo!(),
                Value::Dimension {
                    num: num2,
                    unit: unit2,
                    as_slash: _,
                } => {
                    if !unit.comparable(unit2) {
                        return Err(
                            (format!("Incompatible units {} and {}.", unit2, unit), span).into(),
                        );
                    }
                    if unit == unit2 || unit == &Unit::None || unit2 == &Unit::None {
                        num.cmp(num2)
                    } else {
                        num.cmp(&num2.convert(unit2, unit))
                    }
                }
                _ => {
                    return Err((
                        format!(
                            "Undefined operation \"{} {} {}\".",
                            self.inspect(span)?,
                            op,
                            other.inspect(span)?
                        ),
                        span,
                    )
                        .into())
                }
            },
            _ => {
                return Err((
                    format!(
                        "Undefined operation \"{} {} {}\".",
                        self.inspect(span)?,
                        op,
                        other.inspect(span)?
                    ),
                    span,
                )
                    .into());
            }
        })
    }

    pub fn not_equals(&self, other: &Self) -> bool {
        match self {
            Value::String(s1, ..) => match other {
                Value::String(s2, ..) => s1 != s2,
                _ => true,
            },
            Value::Dimension {
                num: n,
                unit,
                as_slash: _,
            } if !n.is_nan() => match other {
                Value::Dimension {
                    num: n2,
                    unit: unit2,
                    as_slash: _,
                } if !n2.is_nan() => {
                    if !unit.comparable(unit2) {
                        true
                    } else if unit == unit2 {
                        n != n2
                    } else if unit == &Unit::None || unit2 == &Unit::None {
                        true
                    } else {
                        n != &n2.convert(unit2, unit)
                    }
                }
                _ => true,
            },
            Value::List(list1, sep1, brackets1) => match other {
                Value::List(list2, sep2, brackets2) => {
                    if sep1 != sep2 || brackets1 != brackets2 || list1.len() != list2.len() {
                        true
                    } else {
                        for (a, b) in list1.iter().zip(list2) {
                            if a.not_equals(b) {
                                return true;
                            }
                        }
                        false
                    }
                }
                _ => true,
            },
            s => s != other,
        }
    }

    // TODO:
    // https://github.com/sass/dart-sass/blob/d4adea7569832f10e3a26d0e420ae51640740cfb/lib/src/ast/sass/expression/list.dart#L39
    // todo: is this actually fallible?
    pub fn inspect(&self, span: Span) -> SassResult<Cow<'static, str>> {
        Ok(match self {
            Value::Calculation(SassCalculation { name, args }) => Cow::Owned(format!(
                "{name}({})",
                args.into_iter()
                    .map(|arg| arg.inspect(span))
                    .collect::<SassResult<String>>()?
            )),
            Value::List(v, _, brackets) if v.is_empty() => match brackets {
                Brackets::None => Cow::Borrowed("()"),
                Brackets::Bracketed => Cow::Borrowed("[]"),
            },
            Value::List(v, sep, brackets) if v.len() == 1 => match brackets {
                Brackets::None => match sep {
                    ListSeparator::Space | ListSeparator::Slash | ListSeparator::Undecided => {
                        v[0].inspect(span)?
                    }
                    ListSeparator::Comma => Cow::Owned(format!("({},)", v[0].inspect(span)?)),
                },
                Brackets::Bracketed => match sep {
                    ListSeparator::Space | ListSeparator::Slash | ListSeparator::Undecided => {
                        Cow::Owned(format!("[{}]", v[0].inspect(span)?))
                    }
                    ListSeparator::Comma => Cow::Owned(format!("[{},]", v[0].inspect(span)?)),
                },
            },
            Value::List(vals, sep, brackets) => Cow::Owned(match brackets {
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
            Value::FunctionRef(f) => Cow::Owned(format!("get-function(\"{}\")", f.name())),
            Value::Null => Cow::Borrowed("null"),
            Value::Map(map) => Cow::Owned(format!(
                "({})",
                map.iter()
                    .map(|(k, v)| Ok(format!("{}: {}", k.inspect(span)?, v.inspect(span)?)))
                    .collect::<SassResult<Vec<String>>>()?
                    .join(", ")
            )),
            Value::Dimension {
                num,
                unit,
                as_slash: _,
            } => Cow::Owned(format!("{}{}", num.inspect(), unit)),
            Value::ArgList(args) if args.is_empty() => Cow::Borrowed("()"),
            Value::ArgList(args) if args.len() == 1 => Cow::Owned(format!(
                "({},)",
                args.elems
                    .iter()
                    .filter(|x| !x.is_null())
                    .map(|a| a.inspect(span))
                    .collect::<SassResult<Vec<Cow<'static, str>>>>()?
                    .join(", "),
            )),
            Value::ArgList(args) => Cow::Owned(
                args.elems
                    .iter()
                    .filter(|x| !x.is_null())
                    .map(|a| a.inspect(span))
                    .collect::<SassResult<Vec<Cow<'static, str>>>>()?
                    .join(", "),
            ),
            Value::True | Value::False | Value::Color(..) | Value::String(..) => {
                self.to_css_string(span, false)?
            }
        })
    }

    pub fn as_list(self) -> Vec<Value> {
        match self {
            Value::List(v, ..) => v,
            Value::Map(m) => m.as_list(),
            Value::ArgList(v) => v.elems,
            v => vec![v],
        }
    }

    pub fn separator(&self) -> ListSeparator {
        match self {
            Value::List(_, list_separator, _) => *list_separator,
            Value::Map(..) | Value::ArgList(..) => ListSeparator::Comma,
            _ => ListSeparator::Space,
        }
    }

    /// Parses `self` as a selector list, in the same manner as the
    /// `selector-parse()` function.
    ///
    /// Returns a `SassError` if `self` isn't a type that can be parsed as a
    /// selector, or if parsing fails. If `allow_parent` is `true`, this allows
    /// parent selectors. Otherwise, they're considered parse errors.
    ///
    /// `name` is the argument name. It's used for error reporting.
    pub fn to_selector(
        self,
        visitor: &mut Visitor,
        name: &str,
        allows_parent: bool,
    ) -> SassResult<Selector> {
        let string = match self.clone().selector_string(visitor.parser.span_before)? {
            Some(v) => v,
            None => return Err((format!("${}: {} is not a valid selector: it must be a string,\n a list of strings, or a list of lists of strings.", name, self.inspect(visitor.parser.span_before)?), visitor.parser.span_before).into()),
        };
        Ok(Selector(visitor.parse_selector_from_string(
            &string,
            allows_parent,
            true,
        )?))
    }

    #[allow(clippy::only_used_in_recursion)]
    fn selector_string(self, span: Span) -> SassResult<Option<String>> {
        Ok(Some(match self {
            Value::String(text, ..) => text,
            Value::List(list, sep, ..) if !list.is_empty() => {
                let mut result = Vec::new();
                match sep {
                    ListSeparator::Comma => {
                        for complex in list {
                            if let Value::String(text, ..) = complex {
                                result.push(text);
                            } else if let Value::List(
                                _,
                                ListSeparator::Space | ListSeparator::Undecided,
                                ..,
                            ) = complex
                            {
                                result.push(match complex.selector_string(span)? {
                                    Some(v) => v,
                                    None => return Ok(None),
                                });
                            } else {
                                return Ok(None);
                            }
                        }
                    }
                    ListSeparator::Slash => return Ok(None),
                    ListSeparator::Space | ListSeparator::Undecided => {
                        for compound in list {
                            if let Value::String(text, ..) = compound {
                                result.push(text);
                            } else {
                                return Ok(None);
                            }
                        }
                    }
                }

                result.join(sep.as_str())
            }
            _ => return Ok(None),
        }))
    }

    pub fn unary_plus(self, visitor: &mut Visitor) -> SassResult<Self> {
        Ok(match self {
            Self::Dimension { .. } => self,
            Self::Calculation(..) => {
                return Err((
                    format!(
                        "Undefined operation \"+{}\".",
                        self.inspect(visitor.parser.span_before)?
                    ),
                    visitor.parser.span_before,
                )
                    .into())
            }
            _ => Self::String(
                format!(
                    "+{}",
                    &self.to_css_string(
                        visitor.parser.span_before,
                        visitor.parser.options.is_compressed()
                    )?
                ),
                QuoteKind::None,
            ),
        })
    }

    pub fn unary_neg(self, visitor: &mut Visitor) -> SassResult<Self> {
        Ok(match self {
            Self::Calculation(..) => {
                return Err((
                    format!(
                        "Undefined operation \"-{}\".",
                        self.inspect(visitor.parser.span_before)?
                    ),
                    visitor.parser.span_before,
                )
                    .into())
            }
            Self::Dimension {
                num,
                unit,
                as_slash,
            } => Self::Dimension {
                num: -num,
                unit,
                as_slash,
            },
            _ => Self::String(
                format!(
                    "-{}",
                    &self.to_css_string(
                        visitor.parser.span_before,
                        visitor.parser.options.is_compressed()
                    )?
                ),
                QuoteKind::None,
            ),
        })
    }

    pub fn unary_div(self, visitor: &mut Visitor) -> SassResult<Self> {
        Ok(match self {
            _ => Self::String(
                format!(
                    "/{}",
                    &self.to_css_string(
                        visitor.parser.span_before,
                        visitor.parser.options.is_compressed()
                    )?
                ),
                QuoteKind::None,
            ),
        })
    }

    pub fn unary_not(self) -> Self {
        match self {
            Self::False | Self::Null => Self::True,
            _ => Self::False,
        }
    }
}
