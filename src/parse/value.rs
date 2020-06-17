use std::{borrow::Borrow, iter::Iterator, mem};

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
        as_hex, devour_whitespace, eat_number, hex_char_for, is_name,
        peek_until_closing_curly_brace, peek_whitespace, read_until_char, read_until_closing_paren,
        read_until_closing_square_brace, IsWhitespace,
    },
    value::Value,
    value::{Number, SassMap},
    Token,
};

use super::Parser;

impl<'a> Parser<'a> {
    pub(super) fn parse_value(&mut self) -> SassResult<Spanned<Value>> {
        self.whitespace();
        let span = match self.toks.peek() {
            Some(Token { pos, .. }) => *pos,
            None => return Err(("Expected expression.", self.span_before).into()),
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
                    iter.eat_op(
                        Spanned {
                            node: op,
                            span: val.span,
                        },
                        &mut space_separated,
                        last_was_whitespace,
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
                            Value::List(
                                mem::take(&mut space_separated)
                                    .into_iter()
                                    .map(|a| {
                                        span = span.merge(a.span);
                                        a.node
                                    })
                                    .collect(),
                                ListSeparator::Space,
                                Brackets::None,
                            )
                            .span(span),
                        );
                    }
                }
                IntermediateValue::Bracketed(t) => {
                    last_was_whitespace = false;
                    if t.is_empty() {
                        space_separated.push(
                            Value::List(Vec::new(), ListSeparator::Space, Brackets::Bracketed)
                                .span(val.span),
                        );
                        continue;
                    }
                    space_separated.push(match iter.parser.parse_value_from_vec(t)?.node {
                        Value::List(v, sep, Brackets::None) => {
                            Value::List(v, sep, Brackets::Bracketed).span(val.span)
                        }
                        v => Value::List(vec![v], ListSeparator::Space, Brackets::Bracketed)
                            .span(val.span),
                    })
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
                    Value::List(
                        space_separated.into_iter().map(|a| a.node).collect(),
                        ListSeparator::Space,
                        Brackets::None,
                    )
                    .span(span),
                );
            }
            Value::List(
                comma_separated.into_iter().map(|a| a.node).collect(),
                ListSeparator::Comma,
                Brackets::None,
            )
            .span(span)
        } else if space_separated.len() == 1 {
            space_separated.pop().unwrap()
        } else {
            Value::List(
                space_separated.into_iter().map(|a| a.node).collect(),
                ListSeparator::Space,
                Brackets::None,
            )
            .span(span)
        })
    }

    pub(super) fn parse_value_from_vec(&mut self, toks: Vec<Token>) -> SassResult<Spanned<Value>> {
        Parser {
            toks: &mut toks.into_iter().peekmore(),
            map: self.map,
            path: self.path,
            scopes: self.scopes,
            global_scope: self.global_scope,
            super_selectors: self.super_selectors,
            span_before: self.span_before,
            content: self.content.clone(),
            in_mixin: self.in_mixin,
            in_function: self.in_function,
            in_control_flow: self.in_control_flow,
            at_root: self.at_root,
            at_root_has_selector: self.at_root_has_selector,
        }
        .parse_value()
    }

    fn parse_ident_value(&mut self) -> SassResult<Spanned<IntermediateValue>> {
        let Spanned { node: mut s, span } = self.parse_identifier()?;

        self.span_before = span;

        let lower = s.to_ascii_lowercase();

        if lower == "progid" && self.toks.peek().is_some() && self.toks.peek().unwrap().kind == ':'
        {
            s = lower;
            self.toks.next();
            s.push(':');
            s.push_str(&self.eat_progid()?);
            return Ok(Spanned {
                node: IntermediateValue::Value(Value::String(s, QuoteKind::None)),
                span,
            });
        }

        if let Some(Token { kind: '(', .. }) = self.toks.peek() {
            self.toks.next();
            let as_ident = Identifier::from(&s);
            let ident_as_string = as_ident.clone().into_inner();
            let func = match self.scopes.last().get_fn(
                Spanned {
                    node: as_ident,
                    span,
                },
                self.global_scope,
            ) {
                Ok(f) => f,
                Err(_) => {
                    if let Some(f) = GLOBAL_FUNCTIONS.get(ident_as_string.as_str()) {
                        return Ok(
                            IntermediateValue::Value(f.0(self.parse_call_args()?, self)?)
                                .span(span),
                        );
                    } else {
                        // check for special cased CSS functions
                        match lower.as_str() {
                            "calc" | "element" | "expression" => {
                                s = lower;
                                self.eat_calc_args(&mut s)?;
                            }
                            // "min" => {}
                            // "max" => {}
                            "url" => match self.try_eat_url()? {
                                Some(val) => s = val,
                                None => s.push_str(&self.parse_call_args()?.to_css_string(self)?),
                            },
                            _ => s.push_str(&self.parse_call_args()?.to_css_string(self)?),
                        }

                        return Ok(
                            IntermediateValue::Value(Value::String(s, QuoteKind::None)).span(span)
                        );
                    }
                }
            };

            let call_args = self.parse_call_args()?;
            return Ok(IntermediateValue::Value(self.eval_function(func, call_args)?).span(span));
        }

        // check for named colors
        if let Some(c) = NAMED_COLORS.get_by_name(lower.as_str()) {
            return Ok(IntermediateValue::Value(Value::Color(Box::new(Color::new(
                c[0], c[1], c[2], c[3], s,
            ))))
            .span(span));
        }

        // check for keywords
        Ok(match lower.as_str() {
            "true" => IntermediateValue::Value(Value::True),
            "false" => IntermediateValue::Value(Value::False),
            "null" => IntermediateValue::Value(Value::Null),
            "not" => IntermediateValue::Op(Op::Not),
            "and" => IntermediateValue::Op(Op::And),
            "or" => IntermediateValue::Op(Op::Or),
            _ => IntermediateValue::Value(Value::String(s, QuoteKind::None)),
        }
        .span(span))
    }

    fn next_is_hypen(&mut self) -> bool {
        self.toks.peek_forward(1).is_some()
            && matches!(self.toks.peek().unwrap().kind, '-' | '_' | 'a'..='z' | 'A'..='Z')
    }

    fn parse_intermediate_value(&mut self) -> Option<SassResult<Spanned<IntermediateValue>>> {
        let (kind, span) = match self.toks.peek() {
            Some(v) => (v.kind, v.pos()),
            None => return None,
        };

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
                        return Some(Ok(IntermediateValue::Value(Value::Dimension(
                            Number::new_machine(n),
                            unit,
                        ))
                        .span(span)));
                    }
                    BigRational::new_raw(val.num.parse::<BigInt>().unwrap(), BigInt::one())
                } else {
                    if val.num.len() <= 18 && val.times_ten.is_empty() {
                        let n = Rational64::new(parse_i64(&val.num), pow(10, val.dec_len));
                        return Some(Ok(IntermediateValue::Value(Value::Dimension(
                            Number::new_machine(n),
                            unit,
                        ))
                        .span(span)));
                    }
                    BigRational::new(val.num.parse().unwrap(), pow(BigInt::from(10), val.dec_len))
                };

                if val.times_ten.is_empty() {
                    return Some(Ok(IntermediateValue::Value(Value::Dimension(
                        Number::new_big(n),
                        unit,
                    ))
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

                IntermediateValue::Value(Value::Dimension(Number::new_big(n * times_ten), unit))
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
                if self.super_selectors.is_empty() {
                    IntermediateValue::Value(Value::Null).span(span)
                } else {
                    IntermediateValue::Value(self.super_selectors.last().clone().into_value())
                        .span(span)
                }
            }
            '#' => {
                if let Some(Token { kind: '{', pos }) = self.toks.peek_forward(1) {
                    self.span_before = *pos;
                    self.toks.reset_view();
                    return Some(self.parse_ident_value());
                }
                self.toks.reset_view();
                self.toks.next();
                let hex = match self.parse_hex() {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                };
                IntermediateValue::Value(hex.node).span(hex.span)
            }
            q @ '"' | q @ '\'' => {
                let span_start = self.toks.next().unwrap().pos();
                let Spanned { node, span } = match self.parse_quoted_string(q) {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                };
                IntermediateValue::Value(node).span(span_start.merge(span))
            }
            '[' => {
                let mut span = self.toks.next().unwrap().pos();
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
                IntermediateValue::Bracketed(inner).span(span)
            }
            '$' => {
                self.toks.next();
                let val = match self.parse_identifier_no_interpolation(false) {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                };
                let span = val.span;
                IntermediateValue::Value(
                    match self.scopes.last().get_var(val, self.global_scope) {
                        Ok(v) => v,
                        Err(e) => return Some(Err(e)),
                    }
                    .node,
                )
                .span(span)
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
                if v.node.to_ascii_lowercase().as_str() == "important" {
                    IntermediateValue::Value(Value::Important).span(span)
                } else {
                    return Some(Err(("Expected \"important\".", span).into()));
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
                return Some(Err(("expected \";\".", span).into()))
            }
            '\u{0}'..='\u{8}' | '\u{b}'..='\u{1f}' | '\u{7f}'..=std::char::MAX | '`' | '~' => {
                return Some(Err(("Expected expression.", span).into()))
            }
            ' ' | '\n' | '\t' => unreachable!("whitespace is checked prior to this match"),
            'A'..='Z' | 'a'..='z' | '_' | '\\' => {
                unreachable!("these chars are checked in an if stmt")
            }
        }))
    }

    fn parse_hex(&mut self) -> SassResult<Spanned<Value>> {
        let mut s = String::with_capacity(8);
        if self
            .toks
            .peek()
            .ok_or(("Expected identifier.", self.span_before))?
            .kind
            .is_ascii_digit()
        {
            while let Some(c) = self.toks.peek() {
                if !c.kind.is_ascii_hexdigit() || s.len() == 8 {
                    break;
                }
                let tok = self.toks.next().unwrap();
                self.span_before = self.span_before.merge(tok.pos());
                s.push(tok.kind);
            }
        } else {
            let i = self.parse_identifier()?;
            if i.node.chars().all(|c| c.is_ascii_hexdigit()) {
                s = i.node;
                self.span_before = self.span_before.merge(i.span);
            } else {
                return Ok(Spanned {
                    node: Value::String(format!("#{}", i.node), QuoteKind::None),
                    span: i.span,
                });
            }
        }
        match s.len() {
            3 => {
                let v = match u16::from_str_radix(&s, 16) {
                    Ok(a) => a,
                    Err(_) => {
                        return Ok(Value::String(format!("#{}", s), QuoteKind::None)
                            .span(self.span_before))
                    }
                };
                let red = (((v & 0xf00) >> 8) * 0x11) as u8;
                let green = (((v & 0x0f0) >> 4) * 0x11) as u8;
                let blue = ((v & 0x00f) * 0x11) as u8;
                Ok(
                    Value::Color(Box::new(Color::new(red, green, blue, 1, format!("#{}", s))))
                        .span(self.span_before),
                )
            }
            4 => {
                let v = match u16::from_str_radix(&s, 16) {
                    Ok(a) => a,
                    Err(_) => {
                        return Ok(Value::String(format!("#{}", s), QuoteKind::None)
                            .span(self.span_before))
                    }
                };
                let red = (((v & 0xf000) >> 12) * 0x11) as u8;
                let green = (((v & 0x0f00) >> 8) * 0x11) as u8;
                let blue = (((v & 0x00f0) >> 4) * 0x11) as u8;
                let alpha = ((v & 0x000f) * 0x11) as u8;
                Ok(Value::Color(Box::new(Color::new(
                    red,
                    green,
                    blue,
                    alpha,
                    format!("#{}", s),
                )))
                .span(self.span_before))
            }
            6 => {
                let v = match u32::from_str_radix(&s, 16) {
                    Ok(a) => a,
                    Err(_) => {
                        return Ok(Value::String(format!("#{}", s), QuoteKind::None)
                            .span(self.span_before))
                    }
                };
                let red = ((v & 0x00ff_0000) >> 16) as u8;
                let green = ((v & 0x0000_ff00) >> 8) as u8;
                let blue = (v & 0x0000_00ff) as u8;
                Ok(
                    Value::Color(Box::new(Color::new(red, green, blue, 1, format!("#{}", s))))
                        .span(self.span_before),
                )
            }
            8 => {
                let v = match u32::from_str_radix(&s, 16) {
                    Ok(a) => a,
                    Err(_) => {
                        return Ok(Value::String(format!("#{}", s), QuoteKind::None)
                            .span(self.span_before))
                    }
                };
                let red = ((v & 0xff00_0000) >> 24) as u8;
                let green = ((v & 0x00ff_0000) >> 16) as u8;
                let blue = ((v & 0x0000_ff00) >> 8) as u8;
                let alpha = (v & 0x0000_00ff) as u8;
                Ok(Value::Color(Box::new(Color::new(
                    red,
                    green,
                    blue,
                    alpha,
                    format!("#{}", s),
                )))
                .span(self.span_before))
            }
            _ => Err(("Expected hex digit.", self.span_before).into()),
        }
    }

    fn eat_calc_args(&mut self, buf: &mut String) -> SassResult<()> {
        buf.reserve(2);
        buf.push('(');
        let mut nesting = 0;
        while let Some(tok) = self.toks.next() {
            match tok.kind {
                ' ' | '\t' | '\n' => {
                    self.whitespace();
                    buf.push(' ');
                }
                '#' => {
                    if let Some(Token { kind: '{', pos }) = self.toks.peek() {
                        self.span_before = *pos;
                        self.toks.next();
                        let interpolation = self.parse_interpolation()?;
                        buf.push_str(&interpolation.node.to_css_string(interpolation.span)?);
                    } else {
                        buf.push('#');
                    }
                }
                '(' => {
                    nesting += 1;
                    buf.push('(');
                }
                ')' => {
                    if nesting == 0 {
                        break;
                    } else {
                        nesting -= 1;
                        buf.push(')');
                    }
                }
                c => buf.push(c),
            }
        }
        buf.push(')');
        Ok(())
    }

    fn eat_progid(&mut self) -> SassResult<String> {
        let mut string = String::new();
        let mut span = self.toks.peek().unwrap().pos();
        while let Some(tok) = self.toks.next() {
            span = span.merge(tok.pos());
            match tok.kind {
                'a'..='z' | 'A'..='Z' | '.' => {
                    string.push(tok.kind);
                }
                '(' => {
                    self.eat_calc_args(&mut string)?;
                    break;
                }
                _ => return Err(("expected \"(\".", span).into()),
            }
        }
        Ok(string)
    }

    fn try_eat_url(&mut self) -> SassResult<Option<String>> {
        let mut buf = String::from("url(");
        let mut peek_counter = 0;
        peek_counter += peek_whitespace(self.toks);
        while let Some(tok) = self.toks.peek() {
            let kind = tok.kind;
            self.toks.move_forward(1);
            peek_counter += 1;
            if kind == '!'
                || kind == '%'
                || kind == '&'
                || (kind >= '*' && kind <= '~')
                || kind as u32 >= 0x0080
            {
                buf.push(kind);
            } else if kind == '\\' {
                buf.push_str(&self.peek_escape()?);
            } else if kind == '#' {
                if let Some(Token { kind: '{', .. }) = self.toks.peek() {
                    self.toks.move_forward(1);
                    peek_counter += 1;
                    let (interpolation, count) = self.peek_interpolation()?;
                    peek_counter += count;
                    match interpolation.node {
                        Value::String(ref s, ..) => buf.push_str(s),
                        v => buf.push_str(v.to_css_string(interpolation.span)?.borrow()),
                    };
                } else {
                    buf.push('#');
                }
            } else if kind == ')' {
                buf.push(')');
                self.toks.take(peek_counter).for_each(drop);
                return Ok(Some(buf));
            } else if kind.is_whitespace() {
                peek_counter += peek_whitespace(self.toks);
                let next = match self.toks.peek() {
                    Some(v) => v,
                    None => break,
                };
                if next.kind == ')' {
                    buf.push(')');
                    self.toks.take(peek_counter + 1).for_each(drop);
                    return Ok(Some(buf));
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        self.toks.reset_view();
        Ok(None)
    }

    fn peek_interpolation(&mut self) -> SassResult<(Spanned<Value>, usize)> {
        let vec = peek_until_closing_curly_brace(self.toks);
        let peek_counter = vec.len();
        self.toks.move_forward(1);
        let val = self.parse_value_from_vec(vec)?;
        Ok((
            Spanned {
                node: val.node.eval(val.span)?.node.unquote(),
                span: val.span,
            },
            peek_counter,
        ))
    }

    fn peek_escape(&mut self) -> SassResult<String> {
        let mut value = 0;
        let first = match self.toks.peek() {
            Some(t) => *t,
            None => return Ok(String::new()),
        };
        let mut span = first.pos;
        if first.kind == '\n' {
            return Err(("Expected escape sequence.", first.pos()).into());
        } else if first.kind.is_ascii_hexdigit() {
            for _ in 0..6 {
                let next = match self.toks.peek() {
                    Some(t) => t,
                    None => break,
                };
                if !next.kind.is_ascii_hexdigit() {
                    break;
                }
                value *= 16;
                value += as_hex(next.kind);
                span = span.merge(next.pos);
                self.toks.peek_forward(1);
            }
            if self.toks.peek().is_some() && self.toks.peek().unwrap().kind.is_whitespace() {
                self.toks.peek_forward(1);
            }
        } else {
            value = self.toks.peek_forward(1).unwrap().kind as u32;
        }

        let c = std::char::from_u32(value).ok_or(("Invalid escape sequence.", span))?;
        if is_name(c) {
            Ok(c.to_string())
        } else if value <= 0x1F || value == 0x7F {
            let mut buf = String::with_capacity(4);
            buf.push('\\');
            if value > 0xF {
                buf.push(hex_char_for(value >> 4));
            }
            buf.push(hex_char_for(value & 0xF));
            buf.push(' ');
            Ok(buf)
        } else {
            Ok(format!("\\{}", c))
        }
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
            mem::take(&mut self.peek)
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

    fn eat_op(
        &mut self,
        op: Spanned<Op>,
        space_separated: &mut Vec<Spanned<Value>>,
        last_was_whitespace: bool,
    ) -> SassResult<()> {
        match op.node {
            Op::Not => {
                self.whitespace();
                let right = self.single_value()?;
                space_separated.push(Spanned {
                    node: Value::UnaryOp(op.node, Box::new(right.node)),
                    span: right.span,
                });
            }
            Op::Div => {
                self.whitespace();
                let right = self.single_value()?;
                if let Some(left) = space_separated.pop() {
                    space_separated.push(Spanned {
                        node: Value::BinaryOp(Box::new(left.node), op.node, Box::new(right.node)),
                        span: left.span.merge(right.span),
                    });
                } else {
                    self.whitespace();
                    space_separated.push(Spanned {
                        node: Value::String(
                            format!("/{}", right.node.to_css_string(right.span)?),
                            QuoteKind::None,
                        ),
                        span: op.span.merge(right.span),
                    });
                }
            }
            Op::Plus => {
                if let Some(left) = space_separated.pop() {
                    self.whitespace();
                    let right = self.single_value()?;
                    space_separated.push(Spanned {
                        node: Value::BinaryOp(Box::new(left.node), op.node, Box::new(right.node)),
                        span: left.span.merge(right.span),
                    });
                } else {
                    self.whitespace();
                    let right = self.single_value()?;
                    space_separated.push(Spanned {
                        node: Value::UnaryOp(op.node, Box::new(right.node)),
                        span: right.span,
                    });
                }
            }
            Op::Minus => {
                if self.whitespace() || !last_was_whitespace {
                    let right = self.single_value()?;
                    if let Some(left) = space_separated.pop() {
                        space_separated.push(Spanned {
                            node: Value::BinaryOp(
                                Box::new(left.node),
                                op.node,
                                Box::new(right.node),
                            ),
                            span: left.span.merge(right.span),
                        });
                    } else {
                        space_separated
                            .push(right.map_node(|n| Value::UnaryOp(op.node, Box::new(n))));
                    }
                } else {
                    let right = self.single_value()?;
                    space_separated.push(right.map_node(|n| Value::UnaryOp(op.node, Box::new(n))));
                }
            }
            Op::And | Op::Or => {
                self.whitespace();
                // special case when the value is literally "and" or "or"
                if self.peek().is_none() {
                    space_separated
                        .push(Value::String(op.to_string(), QuoteKind::None).span(op.span));
                } else if let Some(left) = space_separated.pop() {
                    self.whitespace();
                    let right = self.single_value()?;
                    space_separated.push(
                        Value::BinaryOp(Box::new(left.node), op.node, Box::new(right.node))
                            .span(left.span.merge(right.span)),
                    );
                } else {
                    return Err(("Expected expression.", op.span).into());
                }
            }
            _ => {
                if let Some(left) = space_separated.pop() {
                    self.whitespace();
                    let right = self.single_value()?;
                    space_separated.push(
                        Value::BinaryOp(Box::new(left.node), op.node, Box::new(right.node))
                            .span(left.span.merge(right.span)),
                    );
                } else {
                    return Err(("Expected expression.", op.span).into());
                }
            }
        }
        Ok(())
    }

    fn single_value(&mut self) -> SassResult<Spanned<Value>> {
        let next = self
            .next()
            .ok_or(("Expected expression.", self.parser.span_before))??;
        Ok(match next.node {
            IntermediateValue::Value(v) => v.span(next.span),
            IntermediateValue::Op(op) => match op {
                Op::Minus => {
                    self.whitespace();
                    let val = self.single_value()?;
                    Spanned {
                        node: val.node.neg(val.span)?,
                        span: next.span.merge(val.span),
                    }
                }
                Op::Not => {
                    self.whitespace();
                    let val = self.single_value()?;
                    Spanned {
                        node: Value::UnaryOp(Op::Not, Box::new(val.node)),
                        span: next.span.merge(val.span),
                    }
                }
                Op::Plus => {
                    self.whitespace();
                    self.single_value()?
                }
                Op::Div => {
                    self.whitespace();
                    let val = self.single_value()?;
                    Spanned {
                        node: Value::String(
                            format!("/{}", val.node.to_css_string(val.span)?),
                            QuoteKind::None,
                        ),
                        span: next.span.merge(val.span),
                    }
                }
                Op::And => Spanned {
                    node: Value::String("and".into(), QuoteKind::None),
                    span: next.span,
                },
                Op::Or => Spanned {
                    node: Value::String("or".into(), QuoteKind::None),
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
                let v = self.parser.parse_value_from_vec(t)?;
                match v.node {
                    Value::List(v, sep, Brackets::None) => Value::List(v, sep, Brackets::Bracketed),
                    v => Value::List(vec![v], ListSeparator::Space, Brackets::Bracketed),
                }
                .span(v.span)
            }
            IntermediateValue::Paren(t) => {
                let val = self.parse_paren(Spanned {
                    node: t,
                    span: next.span,
                })?;
                Spanned {
                    node: Value::Paren(Box::new(val.node)),
                    span: val.span,
                }
            }
        })
    }

    fn parse_paren(&mut self, t: Spanned<Vec<Token>>) -> SassResult<Spanned<Value>> {
        if t.is_empty() {
            return Ok(Value::List(Vec::new(), ListSeparator::Space, Brackets::None).span(t.span));
        }

        let paren_toks = &mut t.node.into_iter().peekmore();

        let mut map = SassMap::new();
        let key = self
            .parser
            .parse_value_from_vec(read_until_char(paren_toks, ':')?)?;

        if paren_toks.peek().is_none() {
            return Ok(Spanned {
                node: Value::Paren(Box::new(key.node)),
                span: key.span,
            });
        }

        let val = self
            .parser
            .parse_value_from_vec(read_until_char(paren_toks, ',')?)?;

        map.insert(key.node, val.node);

        if paren_toks.peek().is_none() {
            return Ok(Spanned {
                node: Value::Map(map),
                span: key.span.merge(val.span),
            });
        }

        let mut span = key.span;

        loop {
            let key = self
                .parser
                .parse_value_from_vec(read_until_char(paren_toks, ':')?)?;
            devour_whitespace(paren_toks);
            let val = self
                .parser
                .parse_value_from_vec(read_until_char(paren_toks, ',')?)?;
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
            node: Value::Map(map),
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

#[derive(Clone, Debug, Eq, PartialEq)]
enum IntermediateValue {
    Value(Value),
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
        if self == &IntermediateValue::Whitespace {
            return true;
        }
        false
    }
}

fn parse_i64(s: &str) -> i64 {
    s.as_bytes()
        .iter()
        .fold(0, |total, this| total * 10 + i64::from(this - b'0'))
}
