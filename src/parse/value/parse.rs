use std::{iter::Iterator, mem};

use num_bigint::BigInt;
use num_rational::{BigRational, Rational64};
use num_traits::{pow, One, ToPrimitive};

use codemap::{Span, Spanned};

use peekmore::PeekMore;

use crate::{
    builtin::GLOBAL_FUNCTIONS,
    color::{Color, NAMED_COLORS},
    common::{Brackets, Identifier, ListSeparator, Op, QuoteKind},
    error::SassResult,
    unit::Unit,
    utils::{
        devour_whitespace, eat_number, read_until_char, read_until_closing_paren,
        read_until_closing_square_brace, IsWhitespace,
    },
    value::{Number, SassFunction, SassMap, Value},
    Token,
};

use super::eval::{HigherIntermediateValue, ValueVisitor};

use super::super::Parser;

#[derive(Clone, Debug)]
enum IntermediateValue {
    Value(HigherIntermediateValue),
    Op(Op),
    Bracketed(Vec<Token>),
    Paren(Vec<Token>),
    Comma,
    Whitespace,
}

impl IntermediateValue {
    const fn span(self, span: Span) -> Spanned<Self> {
        Spanned { node: self, span }
    }
}

impl IsWhitespace for IntermediateValue {
    fn is_whitespace(&self) -> bool {
        if let IntermediateValue::Whitespace = self {
            return true;
        }
        false
    }
}

impl<'a> Parser<'a> {
    pub(crate) fn parse_value(&mut self, in_paren: bool) -> SassResult<Spanned<Value>> {
        self.whitespace();
        let span = match self.toks.peek() {
            Some(Token { kind: '}', .. })
            | Some(Token { kind: ';', .. })
            | Some(Token { kind: '{', .. })
            | None => return Err(("Expected expression.", self.span_before).into()),
            Some(Token { pos, .. }) => *pos,
        };
        let mut last_was_whitespace = false;
        let mut space_separated = Vec::new();
        let mut comma_separated = Vec::new();
        let mut iter = IntermediateValueIterator::new(self);
        while let Some(val) = iter.next() {
            let val = val?;
            match val.node {
                IntermediateValue::Value(v) => {
                    last_was_whitespace = false;
                    space_separated.push(v.span(val.span))
                }
                IntermediateValue::Op(op) => {
                    iter.parse_op(
                        Spanned {
                            node: op,
                            span: val.span,
                        },
                        &mut space_separated,
                        last_was_whitespace,
                        in_paren,
                    )?;
                }
                IntermediateValue::Whitespace => {
                    last_was_whitespace = true;
                    continue;
                }
                IntermediateValue::Comma => {
                    last_was_whitespace = false;

                    if space_separated.len() == 1 {
                        comma_separated.push(space_separated.pop().unwrap());
                    } else {
                        let mut span = space_separated
                            .get(0)
                            .ok_or(("Expected expression.", val.span))?
                            .span;
                        comma_separated.push(
                            HigherIntermediateValue::Literal(Value::List(
                                mem::take(&mut space_separated)
                                    .into_iter()
                                    .map(move |a| {
                                        span = span.merge(a.span);
                                        a.node
                                    })
                                    .map(|a| ValueVisitor::new(iter.parser, span).eval(a, in_paren))
                                    .collect::<SassResult<Vec<Value>>>()?,
                                ListSeparator::Space,
                                Brackets::None,
                            ))
                            .span(span),
                        );
                    }
                }
                IntermediateValue::Bracketed(t) => {
                    last_was_whitespace = false;
                    space_separated.push(
                        HigherIntermediateValue::Literal(
                            match iter.parser.parse_value_from_vec(t, in_paren)?.node {
                                Value::List(v, sep, Brackets::None) => {
                                    Value::List(v, sep, Brackets::Bracketed)
                                }
                                v => {
                                    Value::List(vec![v], ListSeparator::Space, Brackets::Bracketed)
                                }
                            },
                        )
                        .span(val.span),
                    )
                }
                IntermediateValue::Paren(t) => {
                    last_was_whitespace = false;
                    space_separated.push(iter.parse_paren(Spanned {
                        node: t,
                        span: val.span,
                    })?);
                }
            }
        }

        Ok(if !comma_separated.is_empty() {
            if space_separated.len() == 1 {
                comma_separated.push(space_separated.pop().unwrap());
            } else if !space_separated.is_empty() {
                comma_separated.push(
                    HigherIntermediateValue::Literal(Value::List(
                        space_separated
                            .into_iter()
                            .map(|a| ValueVisitor::new(self, span).eval(a.node, in_paren))
                            .collect::<SassResult<Vec<Value>>>()?,
                        ListSeparator::Space,
                        Brackets::None,
                    ))
                    .span(span),
                );
            }
            Value::List(
                comma_separated
                    .into_iter()
                    .map(|a| ValueVisitor::new(self, span).eval(a.node, in_paren))
                    .collect::<SassResult<Vec<Value>>>()?,
                ListSeparator::Comma,
                Brackets::None,
            )
            .span(span)
        } else if space_separated.len() == 1 {
            ValueVisitor::new(self, span)
                .eval(space_separated.pop().unwrap().node, in_paren)?
                .span(span)
        } else {
            Value::List(
                space_separated
                    .into_iter()
                    .map(|a| ValueVisitor::new(self, span).eval(a.node, in_paren))
                    .collect::<SassResult<Vec<Value>>>()?,
                ListSeparator::Space,
                Brackets::None,
            )
            .span(span)
        })
    }

    pub(crate) fn parse_value_from_vec(
        &mut self,
        toks: Vec<Token>,
        in_paren: bool,
    ) -> SassResult<Spanned<Value>> {
        Parser {
            toks: &mut toks.into_iter().peekmore(),
            map: self.map,
            path: self.path,
            scopes: self.scopes,
            global_scope: self.global_scope,
            super_selectors: self.super_selectors,
            span_before: self.span_before,
            content: self.content,
            flags: self.flags,
            at_root: self.at_root,
            at_root_has_selector: self.at_root_has_selector,
            extender: self.extender,
            content_scopes: self.content_scopes,
            options: self.options,
            modules: self.modules,
        }
        .parse_value(in_paren)
    }

    #[allow(clippy::eval_order_dependence)]
    fn parse_module_item(
        &mut self,
        module: &str,
        mut module_span: Span,
    ) -> SassResult<Spanned<IntermediateValue>> {
        Ok(IntermediateValue::Value(
            if matches!(self.toks.peek(), Some(Token { kind: '$', .. })) {
                self.toks.next();
                let var = self
                    .parse_identifier_no_interpolation(false)?
                    .map_node(|i| i.into());

                module_span = module_span.merge(var.span);

                let value = self
                    .modules
                    .get(module)
                    .ok_or((
                        format!("There is no module with the namespace \"{}\".", module),
                        module_span,
                    ))?
                    .get_var(var)?;
                HigherIntermediateValue::Literal(value.clone())
            } else {
                let fn_name = self
                    .parse_identifier_no_interpolation(false)?
                    .map_node(|i| i.into());

                let function = self
                    .modules
                    .get(module)
                    .ok_or((
                        format!("There is no module with the namespace \"{}\".", module),
                        module_span,
                    ))?
                    .get_fn(fn_name.node)
                    .ok_or(("Undefined function.", fn_name.span))?;

                if !matches!(self.toks.next(), Some(Token { kind: '(', .. })) {
                    todo!()
                }

                let call_args = self.parse_call_args()?;

                HigherIntermediateValue::Function(function, call_args)
            },
        )
        .span(module_span))
    }

    fn parse_ident_value(&mut self) -> SassResult<Spanned<IntermediateValue>> {
        let Spanned { node: mut s, span } = self.parse_identifier()?;

        self.span_before = span;

        let lower = s.to_ascii_lowercase();

        if lower == "progid" && matches!(self.toks.peek(), Some(Token { kind: ':', .. })) {
            s = lower;
            self.toks.next();
            s.push(':');
            s.push_str(&self.parse_progid()?);
            return Ok(Spanned {
                node: IntermediateValue::Value(HigherIntermediateValue::Literal(Value::String(
                    s,
                    QuoteKind::None,
                ))),
                span,
            });
        }

        match self.toks.peek() {
            Some(Token { kind: '(', .. }) => {
                self.toks.next();

                if lower == "min" {
                    match self.try_parse_min_max("min", true)? {
                        Some(val) => {
                            self.toks.truncate_iterator_to_cursor();
                            self.toks.next();
                            return Ok(IntermediateValue::Value(HigherIntermediateValue::Literal(
                                Value::String(val, QuoteKind::None),
                            ))
                            .span(span));
                        }
                        None => {
                            self.toks.reset_cursor();
                        }
                    }
                } else if lower == "max" {
                    match self.try_parse_min_max("max", true)? {
                        Some(val) => {
                            self.toks.truncate_iterator_to_cursor();
                            self.toks.next();
                            return Ok(IntermediateValue::Value(HigherIntermediateValue::Literal(
                                Value::String(val, QuoteKind::None),
                            ))
                            .span(span));
                        }
                        None => {
                            self.toks.reset_cursor();
                        }
                    }
                }

                let as_ident = Identifier::from(&s);
                let func = match self.scopes.get_fn(
                    Spanned {
                        node: as_ident,
                        span,
                    },
                    self.global_scope,
                ) {
                    Some(f) => f,
                    None => {
                        if let Some(f) = GLOBAL_FUNCTIONS.get(as_ident.as_str()) {
                            return Ok(IntermediateValue::Value(
                                HigherIntermediateValue::Function(
                                    SassFunction::Builtin(f.clone(), as_ident),
                                    self.parse_call_args()?,
                                ),
                            )
                            .span(span));
                        } else {
                            // check for special cased CSS functions
                            match lower.as_str() {
                                "calc" | "element" | "expression" => {
                                    s = lower;
                                    self.parse_calc_args(&mut s)?;
                                }
                                "url" => match self.try_parse_url()? {
                                    Some(val) => s = val,
                                    None => s.push_str(&self.parse_call_args()?.to_css_string()?),
                                },
                                _ => s.push_str(&self.parse_call_args()?.to_css_string()?),
                            }

                            return Ok(IntermediateValue::Value(HigherIntermediateValue::Literal(
                                Value::String(s, QuoteKind::None),
                            ))
                            .span(span));
                        }
                    }
                };

                let call_args = self.parse_call_args()?;
                return Ok(IntermediateValue::Value(HigherIntermediateValue::Function(
                    SassFunction::UserDefined(Box::new(func), as_ident),
                    call_args,
                ))
                .span(span));
            }
            Some(Token { kind: '.', .. }) => {
                self.toks.next();
                return self.parse_module_item(&s, span);
            }
            _ => {}
        }

        // check for named colors
        if let Some(c) = NAMED_COLORS.get_by_name(lower.as_str()) {
            return Ok(
                IntermediateValue::Value(HigherIntermediateValue::Literal(Value::Color(Box::new(
                    Color::new(c[0], c[1], c[2], c[3], s),
                ))))
                .span(span),
            );
        }

        // check for keywords
        Ok(match s.as_str() {
            "true" => IntermediateValue::Value(HigherIntermediateValue::Literal(Value::True)),
            "false" => IntermediateValue::Value(HigherIntermediateValue::Literal(Value::False)),
            "null" => IntermediateValue::Value(HigherIntermediateValue::Literal(Value::Null)),
            "not" => IntermediateValue::Op(Op::Not),
            "and" => IntermediateValue::Op(Op::And),
            "or" => IntermediateValue::Op(Op::Or),
            _ => IntermediateValue::Value(HigherIntermediateValue::Literal(Value::String(
                s,
                QuoteKind::None,
            ))),
        }
        .span(span))
    }

    fn next_is_hypen(&mut self) -> bool {
        if let Some(Token { kind, .. }) = self.toks.peek_forward(1) {
            matches!(kind, '-' | '_' | 'a'..='z' | 'A'..='Z')
        } else {
            false
        }
    }

    fn parse_intermediate_value(&mut self) -> Option<SassResult<Spanned<IntermediateValue>>> {
        let (kind, span) = match self.toks.peek() {
            Some(v) => (v.kind, v.pos()),
            None => return None,
        };

        self.span_before = span;

        if self.whitespace() {
            return Some(Ok(Spanned {
                node: IntermediateValue::Whitespace,
                span,
            }));
        }

        Some(Ok(match kind {
            _ if kind.is_ascii_alphabetic()
                || kind == '_'
                || kind == '\\'
                || (!kind.is_ascii() && !kind.is_control())
                || (kind == '-' && self.next_is_hypen()) =>
            {
                return Some(self.parse_ident_value());
            }
            '0'..='9' | '.' => {
                let Spanned {
                    node: val,
                    mut span,
                } = match eat_number(self.toks) {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                };
                let unit = if let Some(tok) = self.toks.peek() {
                    let Token { kind, .. } = *tok;
                    match kind {
                        'a'..='z' | 'A'..='Z' | '_' | '\\' | '\u{7f}'..=std::char::MAX => {
                            let u = match self.parse_identifier_no_interpolation(true) {
                                Ok(v) => v,
                                Err(e) => return Some(Err(e)),
                            };
                            span = span.merge(u.span);
                            Unit::from(u.node)
                        }
                        '%' => {
                            span = span.merge(self.toks.next().unwrap().pos());
                            Unit::Percent
                        }
                        _ => Unit::None,
                    }
                } else {
                    Unit::None
                };

                let n = if val.dec_len == 0 {
                    if val.num.len() <= 18 && val.times_ten.is_empty() {
                        let n = Rational64::new_raw(parse_i64(&val.num), 1);
                        return Some(Ok(IntermediateValue::Value(
                            HigherIntermediateValue::Literal(Value::Dimension(
                                Number::new_small(n),
                                unit,
                                false,
                            )),
                        )
                        .span(span)));
                    }
                    BigRational::new_raw(val.num.parse::<BigInt>().unwrap(), BigInt::one())
                } else {
                    if val.num.len() <= 18 && val.times_ten.is_empty() {
                        let n = Rational64::new(parse_i64(&val.num), pow(10, val.dec_len));
                        return Some(Ok(IntermediateValue::Value(
                            HigherIntermediateValue::Literal(Value::Dimension(
                                Number::new_small(n),
                                unit,
                                false,
                            )),
                        )
                        .span(span)));
                    }
                    BigRational::new(val.num.parse().unwrap(), pow(BigInt::from(10), val.dec_len))
                };

                if val.times_ten.is_empty() {
                    return Some(Ok(IntermediateValue::Value(
                        HigherIntermediateValue::Literal(Value::Dimension(
                            Number::new_big(n),
                            unit,
                            false,
                        )),
                    )
                    .span(span)));
                }

                let times_ten = pow(
                    BigInt::from(10),
                    match val
                        .times_ten
                        .parse::<BigInt>()
                        .unwrap()
                        .to_usize()
                        .ok_or(("Exponent too large (expected usize).", span))
                    {
                        Ok(v) => v,
                        Err(e) => return Some(Err(e.into())),
                    },
                );

                let times_ten = if val.times_ten_is_postive {
                    BigRational::new_raw(times_ten, BigInt::one())
                } else {
                    BigRational::new(BigInt::one(), times_ten)
                };

                IntermediateValue::Value(HigherIntermediateValue::Literal(Value::Dimension(
                    Number::new_big(n * times_ten),
                    unit,
                    false,
                )))
                .span(span)
            }
            '(' => {
                let mut span = self.toks.next().unwrap().pos();
                let mut inner = match read_until_closing_paren(self.toks) {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                };
                // todo: the above shouldn't eat the closing paren
                if let Some(last_tok) = inner.pop() {
                    if last_tok.kind != ')' {
                        return Some(Err(("expected \")\".", span).into()));
                    }
                    span = span.merge(last_tok.pos());
                }
                IntermediateValue::Paren(inner).span(span)
            }
            '&' => {
                let span = self.toks.next().unwrap().pos();
                if self.super_selectors.is_empty() && !self.at_root_has_selector && !self.at_root {
                    IntermediateValue::Value(HigherIntermediateValue::Literal(Value::Null))
                        .span(span)
                } else {
                    IntermediateValue::Value(HigherIntermediateValue::Literal(
                        self.super_selectors.last().clone().into_value(),
                    ))
                    .span(span)
                }
            }
            '#' => {
                if let Some(Token { kind: '{', pos }) = self.toks.peek_forward(1) {
                    self.span_before = *pos;
                    self.toks.reset_cursor();
                    return Some(self.parse_ident_value());
                }
                self.toks.reset_cursor();
                self.toks.next();
                let hex = match self.parse_hex() {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                };
                IntermediateValue::Value(HigherIntermediateValue::Literal(hex.node)).span(hex.span)
            }
            q @ '"' | q @ '\'' => {
                let span_start = self.toks.next().unwrap().pos();
                let Spanned { node, span } = match self.parse_quoted_string(q) {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                };
                IntermediateValue::Value(HigherIntermediateValue::Literal(node))
                    .span(span_start.merge(span))
            }
            '[' => {
                let mut span = self.toks.next().unwrap().pos();
                self.whitespace_or_comment();
                let mut inner = match read_until_closing_square_brace(self.toks) {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                };
                if let Some(last_tok) = inner.pop() {
                    if last_tok.kind != ']' {
                        return Some(Err(("expected \"]\".", span).into()));
                    }
                    span = span.merge(last_tok.pos());
                }
                if inner.is_empty() {
                    IntermediateValue::Value(HigherIntermediateValue::Literal(Value::List(
                        Vec::new(),
                        ListSeparator::Space,
                        Brackets::Bracketed,
                    )))
                } else {
                    IntermediateValue::Bracketed(inner)
                }
                .span(span)
            }
            '$' => {
                self.toks.next();
                let val = match self.parse_identifier_no_interpolation(false) {
                    Ok(v) => v.map_node(|i| i.into()),
                    Err(e) => return Some(Err(e)),
                };
                IntermediateValue::Value(HigherIntermediateValue::Literal(
                    match self.scopes.get_var(val, self.global_scope) {
                        Ok(v) => v.clone(),
                        Err(e) => return Some(Err(e)),
                    },
                ))
                .span(val.span)
            }
            '+' => {
                let span = self.toks.next().unwrap().pos();
                IntermediateValue::Op(Op::Plus).span(span)
            }
            '-' => {
                let span = self.toks.next().unwrap().pos();
                IntermediateValue::Op(Op::Minus).span(span)
            }
            '*' => {
                let span = self.toks.next().unwrap().pos();
                IntermediateValue::Op(Op::Mul).span(span)
            }
            '%' => {
                let span = self.toks.next().unwrap().pos();
                IntermediateValue::Op(Op::Rem).span(span)
            }
            ',' => {
                self.toks.next();
                IntermediateValue::Comma.span(span)
            }
            q @ '>' | q @ '<' => {
                let mut span = self.toks.next().unwrap().pos;
                #[allow(clippy::eval_order_dependence)]
                IntermediateValue::Op(if let Some(Token { kind: '=', .. }) = self.toks.peek() {
                    span = span.merge(self.toks.next().unwrap().pos);
                    match q {
                        '>' => Op::GreaterThanEqual,
                        '<' => Op::LessThanEqual,
                        _ => unreachable!(),
                    }
                } else {
                    match q {
                        '>' => Op::GreaterThan,
                        '<' => Op::LessThan,
                        _ => unreachable!(),
                    }
                })
                .span(span)
            }
            '=' => {
                let mut span = self.toks.next().unwrap().pos();
                if let Some(Token { kind: '=', pos }) = self.toks.next() {
                    span = span.merge(pos);
                    IntermediateValue::Op(Op::Equal).span(span)
                } else {
                    return Some(Err(("expected \"=\".", span).into()));
                }
            }
            '!' => {
                let mut span = self.toks.next().unwrap().pos();
                if let Some(Token { kind: '=', .. }) = self.toks.peek() {
                    span = span.merge(self.toks.next().unwrap().pos());
                    return Some(Ok(IntermediateValue::Op(Op::NotEqual).span(span)));
                }
                self.whitespace();
                let v = match self.parse_identifier() {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                };
                span = span.merge(v.span);
                match v.node.to_ascii_lowercase().as_str() {
                    "important" => {
                        IntermediateValue::Value(HigherIntermediateValue::Literal(Value::Important))
                            .span(span)
                    }
                    _ => return Some(Err(("Expected \"important\".", span).into())),
                }
            }
            '/' => {
                let span = self.toks.next().unwrap().pos();
                match self.toks.peek() {
                    Some(Token { kind: '/', .. }) | Some(Token { kind: '*', .. }) => {
                        let span = match self.parse_comment() {
                            Ok(c) => c.span,
                            Err(e) => return Some(Err(e)),
                        };
                        IntermediateValue::Whitespace.span(span)
                    }
                    Some(..) => IntermediateValue::Op(Op::Div).span(span),
                    None => return Some(Err(("Expected expression.", span).into())),
                }
            }
            ';' | '}' | '{' => return None,
            ':' | '?' | ')' | '@' | '^' | ']' | '|' => {
                self.toks.next();
                return Some(Err(("expected \";\".", span).into()));
            }
            '\u{0}'..='\u{8}' | '\u{b}'..='\u{1f}' | '\u{7f}'..=std::char::MAX | '`' | '~' => {
                self.toks.next();
                return Some(Err(("Expected expression.", span).into()));
            }
            ' ' | '\n' | '\t' => unreachable!("whitespace is checked prior to this match"),
            'A'..='Z' | 'a'..='z' | '_' | '\\' => {
                unreachable!("these chars are checked in an if stmt")
            }
        }))
    }

    fn parse_hex(&mut self) -> SassResult<Spanned<Value>> {
        let mut s = String::with_capacity(7);
        s.push('#');
        let first_char = self
            .toks
            .peek()
            .ok_or(("Expected identifier.", self.span_before))?
            .kind;
        let first_is_digit = first_char.is_ascii_digit();
        let first_is_hexdigit = first_char.is_ascii_hexdigit();
        if first_is_digit {
            while let Some(c) = self.toks.peek() {
                if !c.kind.is_ascii_hexdigit() || s.len() == 9 {
                    break;
                }
                let tok = self.toks.next().unwrap();
                self.span_before = self.span_before.merge(tok.pos());
                s.push(tok.kind);
            }
        // this branch exists so that we can emit `#` combined with
        // identifiers. e.g. `#ooobar` should be emitted exactly as written;
        // that is, `#ooobar`.
        } else {
            let ident = self.parse_identifier()?;
            if first_is_hexdigit
                && ident.node.chars().all(|c| c.is_ascii_hexdigit())
                && matches!(ident.node.len(), 3 | 4 | 6 | 8)
            {
                s.push_str(&ident.node);
            } else {
                return Ok(Spanned {
                    node: Value::String(format!("#{}", ident.node), QuoteKind::None),
                    span: ident.span,
                });
            }
        }
        let v = match u32::from_str_radix(&s[1..], 16) {
            Ok(a) => a,
            Err(_) => return Ok(Value::String(s, QuoteKind::None).span(self.span_before)),
        };
        let (red, green, blue, alpha) = match s.len().saturating_sub(1) {
            3 => (
                (((v & 0x0f00) >> 8) * 0x11) as u8,
                (((v & 0x00f0) >> 4) * 0x11) as u8,
                ((v & 0x000f) * 0x11) as u8,
                1,
            ),
            4 => (
                (((v & 0xf000) >> 12) * 0x11) as u8,
                (((v & 0x0f00) >> 8) * 0x11) as u8,
                (((v & 0x00f0) >> 4) * 0x11) as u8,
                ((v & 0x000f) * 0x11) as u8,
            ),
            6 => (
                ((v & 0x00ff_0000) >> 16) as u8,
                ((v & 0x0000_ff00) >> 8) as u8,
                (v & 0x0000_00ff) as u8,
                1,
            ),
            8 => (
                ((v & 0xff00_0000) >> 24) as u8,
                ((v & 0x00ff_0000) >> 16) as u8,
                ((v & 0x0000_ff00) >> 8) as u8,
                (v & 0x0000_00ff) as u8,
            ),
            _ => return Err(("Expected hex digit.", self.span_before).into()),
        };
        let color = Color::new(red, green, blue, alpha, s);
        Ok(Value::Color(Box::new(color)).span(self.span_before))
    }
}

struct IntermediateValueIterator<'a, 'b: 'a> {
    parser: &'a mut Parser<'b>,
    peek: Option<SassResult<Spanned<IntermediateValue>>>,
}

impl<'a, 'b: 'a> Iterator for IntermediateValueIterator<'a, 'b> {
    type Item = SassResult<Spanned<IntermediateValue>>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.peek.is_some() {
            self.peek.take()
        } else {
            self.parser.parse_intermediate_value()
        }
    }
}

impl<'a, 'b: 'a> IntermediateValueIterator<'a, 'b> {
    pub fn new(parser: &'a mut Parser<'b>) -> Self {
        Self { parser, peek: None }
    }

    fn peek(&mut self) -> &Option<SassResult<Spanned<IntermediateValue>>> {
        self.peek = self.next();
        &self.peek
    }

    fn whitespace(&mut self) -> bool {
        let mut found_whitespace = false;
        while let Some(w) = self.peek() {
            if !w.is_whitespace() {
                break;
            }
            found_whitespace = true;
            self.next();
        }
        found_whitespace
    }

    fn parse_op(
        &mut self,
        op: Spanned<Op>,
        space_separated: &mut Vec<Spanned<HigherIntermediateValue>>,
        last_was_whitespace: bool,
        in_paren: bool,
    ) -> SassResult<()> {
        match op.node {
            Op::Not => {
                self.whitespace();
                let right = self.single_value(in_paren)?;
                space_separated.push(Spanned {
                    node: HigherIntermediateValue::UnaryOp(op.node, Box::new(right.node)),
                    span: right.span,
                });
            }
            Op::Div => {
                self.whitespace();
                let right = self.single_value(in_paren)?;
                if let Some(left) = space_separated.pop() {
                    space_separated.push(Spanned {
                        node: HigherIntermediateValue::BinaryOp(
                            Box::new(left.node),
                            op.node,
                            Box::new(right.node),
                        ),
                        span: left.span.merge(right.span),
                    });
                } else {
                    self.whitespace();
                    space_separated.push(Spanned {
                        node: HigherIntermediateValue::Literal(Value::String(
                            format!(
                                "/{}",
                                ValueVisitor::new(self.parser, right.span)
                                    .eval(right.node, false)?
                                    .to_css_string(right.span)?
                            ),
                            QuoteKind::None,
                        )),
                        span: op.span.merge(right.span),
                    });
                }
            }
            Op::Plus => {
                if let Some(left) = space_separated.pop() {
                    self.whitespace();
                    let right = self.single_value(in_paren)?;
                    space_separated.push(Spanned {
                        node: HigherIntermediateValue::BinaryOp(
                            Box::new(left.node),
                            op.node,
                            Box::new(right.node),
                        ),
                        span: left.span.merge(right.span),
                    });
                } else {
                    self.whitespace();
                    let right = self.single_value(in_paren)?;
                    space_separated.push(Spanned {
                        node: HigherIntermediateValue::UnaryOp(op.node, Box::new(right.node)),
                        span: right.span,
                    });
                }
            }
            Op::Minus => {
                if self.whitespace() || !last_was_whitespace {
                    let right = self.single_value(in_paren)?;
                    if let Some(left) = space_separated.pop() {
                        space_separated.push(Spanned {
                            node: HigherIntermediateValue::BinaryOp(
                                Box::new(left.node),
                                op.node,
                                Box::new(right.node),
                            ),
                            span: left.span.merge(right.span),
                        });
                    } else {
                        space_separated.push(
                            right.map_node(|n| {
                                HigherIntermediateValue::UnaryOp(op.node, Box::new(n))
                            }),
                        );
                    }
                } else {
                    let right = self.single_value(in_paren)?;
                    space_separated.push(
                        right.map_node(|n| HigherIntermediateValue::UnaryOp(op.node, Box::new(n))),
                    );
                }
            }
            Op::And => {
                self.whitespace();
                // special case when the value is literally "and"
                if self.peek().is_none() {
                    space_separated.push(
                        HigherIntermediateValue::Literal(Value::String(
                            op.to_string(),
                            QuoteKind::None,
                        ))
                        .span(op.span),
                    );
                } else if let Some(left) = space_separated.pop() {
                    self.whitespace();
                    if ValueVisitor::new(self.parser, left.span)
                        .eval(left.node.clone(), false)?
                        .is_true()
                    {
                        let right = self.single_value(in_paren)?;
                        space_separated.push(
                            HigherIntermediateValue::BinaryOp(
                                Box::new(left.node),
                                op.node,
                                Box::new(right.node),
                            )
                            .span(left.span.merge(right.span)),
                        );
                    } else {
                        // we explicitly ignore errors here as a workaround for short circuiting
                        while let Some(value) = self.peek() {
                            if let Ok(Spanned {
                                node: IntermediateValue::Comma,
                                ..
                            }) = value
                            {
                                break;
                            }
                            self.next();
                        }
                        space_separated.push(left);
                    }
                } else {
                    return Err(("Expected expression.", op.span).into());
                }
            }
            Op::Or => {
                self.whitespace();
                // special case when the value is literally "or"
                if self.peek().is_none() {
                    space_separated.push(
                        HigherIntermediateValue::Literal(Value::String(
                            op.to_string(),
                            QuoteKind::None,
                        ))
                        .span(op.span),
                    );
                } else if let Some(left) = space_separated.pop() {
                    self.whitespace();
                    if ValueVisitor::new(self.parser, left.span)
                        .eval(left.node.clone(), false)?
                        .is_true()
                    {
                        // we explicitly ignore errors here as a workaround for short circuiting
                        while let Some(value) = self.peek() {
                            match value {
                                Ok(Spanned {
                                    node: IntermediateValue::Comma,
                                    ..
                                }) => break,
                                Ok(..) => {
                                    self.next();
                                }
                                Err(..) => {
                                    if let Some(v) = self.next() {
                                        v?;
                                    }
                                }
                            }
                        }
                        space_separated.push(left);
                    } else {
                        let right = self.single_value(in_paren)?;
                        space_separated.push(
                            HigherIntermediateValue::BinaryOp(
                                Box::new(left.node),
                                op.node,
                                Box::new(right.node),
                            )
                            .span(left.span.merge(right.span)),
                        );
                    }
                } else {
                    return Err(("Expected expression.", op.span).into());
                }
            }
            _ => {
                if let Some(left) = space_separated.pop() {
                    self.whitespace();
                    let right = self.single_value(in_paren)?;
                    space_separated.push(
                        HigherIntermediateValue::BinaryOp(
                            Box::new(left.node),
                            op.node,
                            Box::new(right.node),
                        )
                        .span(left.span.merge(right.span)),
                    );
                } else {
                    return Err(("Expected expression.", op.span).into());
                }
            }
        }
        Ok(())
    }

    fn single_value(&mut self, in_paren: bool) -> SassResult<Spanned<HigherIntermediateValue>> {
        let next = self
            .next()
            .ok_or(("Expected expression.", self.parser.span_before))??;
        Ok(match next.node {
            IntermediateValue::Value(v) => v.span(next.span),
            IntermediateValue::Op(op) => match op {
                Op::Minus => {
                    self.whitespace();
                    let val = self.single_value(in_paren)?;
                    Spanned {
                        node: HigherIntermediateValue::UnaryOp(Op::Minus, Box::new(val.node)),
                        span: next.span.merge(val.span),
                    }
                }
                Op::Not => {
                    self.whitespace();
                    let val = self.single_value(in_paren)?;
                    Spanned {
                        node: HigherIntermediateValue::UnaryOp(Op::Not, Box::new(val.node)),
                        span: next.span.merge(val.span),
                    }
                }
                Op::Plus => {
                    self.whitespace();
                    self.single_value(in_paren)?
                }
                Op::Div => {
                    self.whitespace();
                    let val = self.single_value(in_paren)?;
                    Spanned {
                        node: HigherIntermediateValue::Literal(Value::String(
                            format!(
                                "/{}",
                                ValueVisitor::new(self.parser, val.span)
                                    .eval(val.node, false)?
                                    .to_css_string(val.span)?
                            ),
                            QuoteKind::None,
                        )),
                        span: next.span.merge(val.span),
                    }
                }
                Op::And => Spanned {
                    node: HigherIntermediateValue::Literal(Value::String(
                        "and".into(),
                        QuoteKind::None,
                    )),
                    span: next.span,
                },
                Op::Or => Spanned {
                    node: HigherIntermediateValue::Literal(Value::String(
                        "or".into(),
                        QuoteKind::None,
                    )),
                    span: next.span,
                },
                _ => {
                    return Err(("Expected expression.", next.span).into());
                }
            },
            IntermediateValue::Whitespace => unreachable!(),
            IntermediateValue::Comma => {
                return Err(("Expected expression.", self.parser.span_before).into())
            }
            IntermediateValue::Bracketed(t) => {
                let v = self.parser.parse_value_from_vec(t, in_paren)?;
                HigherIntermediateValue::Literal(match v.node {
                    Value::List(v, sep, Brackets::None) => Value::List(v, sep, Brackets::Bracketed),
                    v => Value::List(vec![v], ListSeparator::Space, Brackets::Bracketed),
                })
                .span(v.span)
            }
            IntermediateValue::Paren(t) => {
                let val = self.parse_paren(Spanned {
                    node: t,
                    span: next.span,
                })?;
                Spanned {
                    node: HigherIntermediateValue::Paren(Box::new(val.node)),
                    span: val.span,
                }
            }
        })
    }

    fn parse_paren(
        &mut self,
        t: Spanned<Vec<Token>>,
    ) -> SassResult<Spanned<HigherIntermediateValue>> {
        if t.is_empty() {
            return Ok(HigherIntermediateValue::Literal(Value::List(
                Vec::new(),
                ListSeparator::Space,
                Brackets::None,
            ))
            .span(t.span));
        }

        let paren_toks = &mut t.node.into_iter().peekmore();

        let mut map = SassMap::new();
        let key = self
            .parser
            .parse_value_from_vec(read_until_char(paren_toks, ':')?, true)?;

        if paren_toks.peek().is_none() {
            return Ok(Spanned {
                node: HigherIntermediateValue::Paren(Box::new(HigherIntermediateValue::Literal(
                    key.node,
                ))),
                span: key.span,
            });
        }

        let val = self
            .parser
            .parse_value_from_vec(read_until_char(paren_toks, ',')?, true)?;

        map.insert(key.node, val.node);

        devour_whitespace(paren_toks);

        if paren_toks.peek().is_none() {
            return Ok(Spanned {
                node: HigherIntermediateValue::Literal(Value::Map(map)),
                span: key.span.merge(val.span),
            });
        }

        let mut span = key.span;

        loop {
            let key = self
                .parser
                .parse_value_from_vec(read_until_char(paren_toks, ':')?, true)?;
            devour_whitespace(paren_toks);
            let val = self
                .parser
                .parse_value_from_vec(read_until_char(paren_toks, ',')?, true)?;
            span = span.merge(val.span);
            devour_whitespace(paren_toks);
            if map.insert(key.node, val.node) {
                return Err(("Duplicate key.", key.span).into());
            }
            if paren_toks.peek().is_none() {
                break;
            }
        }
        Ok(Spanned {
            node: HigherIntermediateValue::Literal(Value::Map(map)),
            span,
        })
    }
}

impl IsWhitespace for SassResult<Spanned<IntermediateValue>> {
    fn is_whitespace(&self) -> bool {
        match self {
            Ok(v) => v.node.is_whitespace(),
            _ => false,
        }
    }
}

fn parse_i64(s: &str) -> i64 {
    s.as_bytes()
        .iter()
        .fold(0, |total, this| total * 10 + i64::from(this - b'0'))
}
