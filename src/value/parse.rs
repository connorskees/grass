use std::iter::Iterator;
use std::mem;

use num_bigint::BigInt;
use num_rational::{BigRational, Rational64};
use num_traits::{pow, One, ToPrimitive};

use codemap::{Span, Spanned};

use peekmore::{PeekMore, PeekMoreIterator};

use super::css_function::{eat_calc_args, eat_progid, try_eat_url};

use crate::args::eat_call_args;
use crate::builtin::GLOBAL_FUNCTIONS;
use crate::color::{Color, NAMED_COLORS};
use crate::common::{Brackets, Identifier, ListSeparator, Op, QuoteKind};
use crate::error::SassResult;
use crate::scope::Scope;
use crate::selector::Selector;
use crate::unit::Unit;
use crate::utils::{
    devour_whitespace, eat_comment, eat_ident, eat_ident_no_interpolation, eat_number,
    parse_quoted_string, read_until_char, read_until_closing_paren,
    read_until_closing_square_brace, read_until_newline, IsWhitespace,
};
use crate::value::Value;
use crate::Token;

use super::map::SassMap;
use super::number::Number;

fn parse_hex<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    scope: &Scope,
    super_selector: &Selector,
    mut span: Span,
) -> SassResult<Spanned<Value>> {
    let mut s = String::with_capacity(8);
    if toks
        .peek()
        .ok_or(("Expected identifier.", span))?
        .kind
        .is_ascii_digit()
    {
        while let Some(c) = toks.peek() {
            if !c.kind.is_ascii_hexdigit() || s.len() == 8 {
                break;
            }
            let tok = toks.next().unwrap();
            span = span.merge(tok.pos());
            s.push(tok.kind);
        }
    } else {
        let i = eat_ident(toks, scope, super_selector, span)?;
        if i.node.chars().all(|c| c.is_ascii_hexdigit()) {
            s = i.node;
            span = span.merge(i.span);
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
                Err(_) => return Ok(Value::String(format!("#{}", s), QuoteKind::None).span(span)),
            };
            let red = (((v & 0xf00) >> 8) * 0x11) as u8;
            let green = (((v & 0x0f0) >> 4) * 0x11) as u8;
            let blue = ((v & 0x00f) * 0x11) as u8;
            Ok(
                Value::Color(Box::new(Color::new(red, green, blue, 1, format!("#{}", s))))
                    .span(span),
            )
        }
        4 => {
            let v = match u16::from_str_radix(&s, 16) {
                Ok(a) => a,
                Err(_) => return Ok(Value::String(format!("#{}", s), QuoteKind::None).span(span)),
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
            .span(span))
        }
        6 => {
            let v = match u32::from_str_radix(&s, 16) {
                Ok(a) => a,
                Err(_) => return Ok(Value::String(format!("#{}", s), QuoteKind::None).span(span)),
            };
            let red = ((v & 0x00ff_0000) >> 16) as u8;
            let green = ((v & 0x0000_ff00) >> 8) as u8;
            let blue = (v & 0x0000_00ff) as u8;
            Ok(
                Value::Color(Box::new(Color::new(red, green, blue, 1, format!("#{}", s))))
                    .span(span),
            )
        }
        8 => {
            let v = match u32::from_str_radix(&s, 16) {
                Ok(a) => a,
                Err(_) => return Ok(Value::String(format!("#{}", s), QuoteKind::None).span(span)),
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
            .span(span))
        }
        _ => Err(("Expected hex digit.", span).into()),
    }
}

struct IntermediateValueIterator<'a, I: Iterator<Item = Token>> {
    toks: &'a mut PeekMoreIterator<I>,
    scope: &'a Scope,
    super_selector: &'a Selector,
}

impl<'a, I: Iterator<Item = Token>> Iterator for IntermediateValueIterator<'a, I> {
    type Item = SassResult<Spanned<IntermediateValue>>;
    fn next(&mut self) -> Option<Self::Item> {
        Value::parse_intermediate_value(self.toks, self.scope, self.super_selector)
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
    fn span(self, span: Span) -> Spanned<Self> {
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

fn parse_paren(
    t: Spanned<Vec<Token>>,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<Spanned<Value>> {
    if t.is_empty() {
        return Ok(Value::List(Vec::new(), ListSeparator::Space, Brackets::None).span(t.span));
    }

    let paren_toks = &mut t.node.into_iter().peekmore();

    let mut map = SassMap::new();
    let key = Value::from_vec(read_until_char(paren_toks, ':'), scope, super_selector)?;

    if paren_toks.peek().is_none() {
        return Ok(Spanned {
            node: Value::Paren(Box::new(key.node)),
            span: key.span,
        });
    }

    let val = Value::from_vec(read_until_char(paren_toks, ','), scope, super_selector)?;

    map.insert(key.node, val.node);

    if paren_toks.peek().is_none() {
        return Ok(Spanned {
            node: Value::Map(map),
            span: key.span.merge(val.span),
        });
    }

    let mut span = key.span;

    loop {
        let key = Value::from_vec(read_until_char(paren_toks, ':'), scope, super_selector)?;
        devour_whitespace(paren_toks);
        let val = Value::from_vec(read_until_char(paren_toks, ','), scope, super_selector)?;
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

fn eat_op<I: Iterator<Item = Token>>(
    iter: &mut PeekMoreIterator<IntermediateValueIterator<I>>,
    scope: &Scope,
    super_selector: &Selector,
    op: Spanned<Op>,
    space_separated: &mut Vec<Spanned<Value>>,
    last_was_whitespace: bool,
) -> SassResult<()> {
    match op.node {
        Op::Not => {
            devour_whitespace(iter);
            let right = single_value(iter, scope, super_selector, op.span)?;
            space_separated.push(Spanned {
                node: Value::UnaryOp(op.node, Box::new(right.node)),
                span: right.span,
            });
        }
        Op::Div => {
            devour_whitespace(iter);
            let right = single_value(iter, scope, super_selector, op.span)?;
            if let Some(left) = space_separated.pop() {
                space_separated.push(Spanned {
                    node: Value::BinaryOp(Box::new(left.node), op.node, Box::new(right.node)),
                    span: left.span.merge(right.span),
                });
            } else {
                devour_whitespace(iter);
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
                devour_whitespace(iter);
                let right = single_value(iter, scope, super_selector, op.span)?;
                space_separated.push(Spanned {
                    node: Value::BinaryOp(Box::new(left.node), op.node, Box::new(right.node)),
                    span: left.span.merge(right.span),
                });
            } else {
                devour_whitespace(iter);
                let right = single_value(iter, scope, super_selector, op.span)?;
                space_separated.push(Spanned {
                    node: Value::UnaryOp(op.node, Box::new(right.node)),
                    span: right.span,
                });
            }
        }
        Op::Minus => {
            if devour_whitespace(iter) || !last_was_whitespace {
                let right = single_value(iter, scope, super_selector, op.span)?;
                if let Some(left) = space_separated.pop() {
                    space_separated.push(Spanned {
                        node: Value::BinaryOp(Box::new(left.node), op.node, Box::new(right.node)),
                        span: left.span.merge(right.span),
                    });
                } else {
                    space_separated.push(right.map_node(|n| Value::UnaryOp(op.node, Box::new(n))));
                }
            } else {
                let right = single_value(iter, scope, super_selector, op.span)?;
                space_separated.push(right.map_node(|n| Value::UnaryOp(op.node, Box::new(n))));
            }
        }
        Op::And | Op::Or => {
            devour_whitespace(iter);
            // special case when the value is literally "and" or "or"
            if iter.peek().is_none() {
                space_separated.push(Value::String(op.to_string(), QuoteKind::None).span(op.span));
            } else if let Some(left) = space_separated.pop() {
                devour_whitespace(iter);
                let right = single_value(iter, scope, super_selector, left.span)?;
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
                devour_whitespace(iter);
                let right = single_value(iter, scope, super_selector, left.span)?;
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

fn single_value<I: Iterator<Item = Token>>(
    iter: &mut PeekMoreIterator<IntermediateValueIterator<I>>,
    scope: &Scope,
    super_selector: &Selector,
    span: Span,
) -> SassResult<Spanned<Value>> {
    let next = iter.next().ok_or(("Expected expression.", span))??;
    Ok(match next.node {
        IntermediateValue::Value(v) => v.span(next.span),
        IntermediateValue::Op(op) => match op {
            Op::Minus => {
                devour_whitespace(iter);
                let val = single_value(iter, scope, super_selector, span)?;
                Spanned {
                    node: val.node.neg(val.span)?,
                    span: next.span.merge(val.span),
                }
            }
            Op::Not => {
                devour_whitespace(iter);
                let val = single_value(iter, scope, super_selector, span)?;
                Spanned {
                    node: Value::UnaryOp(Op::Not, Box::new(val.node)),
                    span: next.span.merge(val.span),
                }
            }
            Op::Plus => {
                devour_whitespace(iter);
                single_value(iter, scope, super_selector, span)?
            }
            Op::Div => {
                devour_whitespace(iter);
                let val = single_value(iter, scope, super_selector, span)?;
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
        IntermediateValue::Comma => return Err(("Expected expression.", span).into()),
        IntermediateValue::Bracketed(t) => {
            let v = Value::from_vec(t, scope, super_selector)?;
            match v.node {
                Value::List(v, sep, Brackets::None) => Value::List(v, sep, Brackets::Bracketed),
                v => Value::List(vec![v], ListSeparator::Space, Brackets::Bracketed),
            }
            .span(v.span)
        }
        IntermediateValue::Paren(t) => {
            let val = parse_paren(
                Spanned {
                    node: t,
                    span: next.span,
                },
                scope,
                super_selector,
            )?;
            Spanned {
                node: Value::Paren(Box::new(val.node)),
                span: val.span,
            }
        }
    })
}

impl Value {
    pub fn from_tokens<I: Iterator<Item = Token>>(
        toks: &mut PeekMoreIterator<I>,
        scope: &Scope,
        super_selector: &Selector,
    ) -> SassResult<Spanned<Self>> {
        let span = match toks.peek() {
            Some(Token { pos, .. }) => *pos,
            None => todo!("Expected expression."),
        };
        devour_whitespace(toks);
        let mut last_was_whitespace = false;
        let mut space_separated = Vec::new();
        let mut comma_separated = Vec::new();
        let mut iter = IntermediateValueIterator {
            toks,
            scope,
            super_selector,
        }
        .peekmore();
        while let Some(val) = iter.next() {
            let val = val?;
            match val.node {
                IntermediateValue::Value(v) => {
                    last_was_whitespace = false;
                    space_separated.push(v.span(val.span))
                }
                IntermediateValue::Op(op) => {
                    eat_op(
                        &mut iter,
                        scope,
                        super_selector,
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
                    space_separated.push(match Value::from_vec(t, scope, super_selector)?.node {
                        Value::List(v, sep, Brackets::None) => {
                            Value::List(v, sep, Brackets::Bracketed).span(val.span)
                        }
                        v => Value::List(vec![v], ListSeparator::Space, Brackets::Bracketed)
                            .span(val.span),
                    })
                }
                IntermediateValue::Paren(t) => {
                    last_was_whitespace = false;
                    space_separated.push(parse_paren(
                        Spanned {
                            node: t,
                            span: val.span,
                        },
                        scope,
                        super_selector,
                    )?);
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

    pub fn from_vec(
        toks: Vec<Token>,
        scope: &Scope,
        super_selector: &Selector,
    ) -> SassResult<Spanned<Value>> {
        Self::from_tokens(&mut toks.into_iter().peekmore(), scope, super_selector)
    }

    fn ident<I: Iterator<Item = Token>>(
        toks: &mut PeekMoreIterator<I>,
        scope: &Scope,
        super_selector: &Selector,
        span_before: Span,
    ) -> SassResult<Spanned<IntermediateValue>> {
        let Spanned { node: mut s, span } = eat_ident(toks, scope, super_selector, span_before)?;

        let lower = s.to_ascii_lowercase();

        if lower == "progid" && toks.peek().is_some() && toks.peek().unwrap().kind == ':' {
            s = lower;
            toks.next();
            s.push(':');
            s.push_str(&eat_progid(toks, scope, super_selector)?);
            return Ok(Spanned {
                node: IntermediateValue::Value(Value::String(s, QuoteKind::None)),
                span,
            });
        }

        if let Some(Token { kind: '(', .. }) = toks.peek() {
            let as_ident = Identifier::from(&s);
            toks.next();
            let func = match scope.get_fn(Spanned {
                node: as_ident.clone(),
                span,
            }) {
                Ok(f) => f,
                Err(_) => match GLOBAL_FUNCTIONS.get(as_ident.into_inner().as_str()) {
                    Some(f) => {
                        return Ok(IntermediateValue::Value(f.0(
                            eat_call_args(toks)?,
                            scope,
                            super_selector,
                        )?)
                        .span(span))
                    }
                    None => {
                        match lower.as_str() {
                            "calc" | "element" | "expression" => {
                                s = lower;
                                eat_calc_args(toks, scope, super_selector, &mut s)?;
                            }
                            // "min" => {}
                            // "max" => {}
                            "url" => match try_eat_url(toks, scope, super_selector)? {
                                Some(val) => s = val,
                                None => s.push_str(
                                    &eat_call_args(toks)?.to_css_string(scope, super_selector)?,
                                ),
                            },
                            _ => s.push_str(
                                &eat_call_args(toks)?.to_css_string(scope, super_selector)?,
                            ),
                        }
                        return Ok(
                            IntermediateValue::Value(Value::String(s, QuoteKind::None)).span(span)
                        );
                    }
                },
            };
            return Ok(IntermediateValue::Value(func.eval(
                eat_call_args(toks)?,
                scope,
                super_selector,
            )?)
            .span(span));
        }

        if let Some(c) = NAMED_COLORS.get_by_name(&lower.as_str()) {
            return Ok(IntermediateValue::Value(Value::Color(Box::new(Color::new(
                c[0], c[1], c[2], c[3], s,
            ))))
            .span(span));
        }

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

    fn parse_intermediate_value<I: Iterator<Item = Token>>(
        toks: &mut PeekMoreIterator<I>,
        scope: &Scope,
        super_selector: &Selector,
    ) -> Option<SassResult<Spanned<IntermediateValue>>> {
        let (kind, span) = match toks.peek() {
            Some(v) => (v.kind, v.pos()),
            None => return None,
        };

        if devour_whitespace(toks) {
            return Some(Ok(Spanned {
                node: IntermediateValue::Whitespace,
                span,
            }));
        }

        let next_is_hypen = |toks: &mut PeekMoreIterator<I>| {
            toks.peek_forward(1).is_some()
                && matches!(toks.peek().unwrap().kind, '-' | '_' | 'a'..='z' | 'A'..='Z')
        };
        Some(Ok(match kind {
            _ if kind.is_ascii_alphabetic()
                || kind == '_'
                || kind == '\\'
                || (!kind.is_ascii() && !kind.is_control())
                || (kind == '-' && next_is_hypen(toks)) =>
            {
                return Some(Self::ident(toks, scope, super_selector, span));
            }
            '0'..='9' | '.' => {
                let Spanned {
                    node: val,
                    mut span,
                } = match eat_number(toks) {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                };
                let unit = if let Some(tok) = toks.peek() {
                    let Token { kind, pos } = *tok;
                    match kind {
                        'a'..='z' | 'A'..='Z' | '_' | '\\' => {
                            let u = match eat_ident_no_interpolation(toks, true, pos) {
                                Ok(v) => v,
                                Err(e) => return Some(Err(e)),
                            };
                            span = span.merge(u.span);
                            Unit::from(u.node)
                        }
                        '%' => {
                            span = span.merge(toks.next().unwrap().pos());
                            Unit::Percent
                        }
                        _ => Unit::None,
                    }
                } else {
                    Unit::None
                };

                let n = if val.dec_len == 0 {
                    if val.num.len() <= 18 && val.times_ten.is_empty() {
                        let n = Rational64::new_raw(val.num.parse::<i64>().unwrap(), 1);
                        return Some(Ok(IntermediateValue::Value(Value::Dimension(
                            Number::new_machine(n),
                            unit,
                        ))
                        .span(span)));
                    }
                    BigRational::new_raw(val.num.parse::<BigInt>().unwrap(), BigInt::one())
                } else {
                    if val.num.len() <= 18 && val.times_ten.is_empty() {
                        let n =
                            Rational64::new(val.num.parse::<i64>().unwrap(), pow(10, val.dec_len));
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
                let mut span = toks.next().unwrap().pos();
                let mut inner = read_until_closing_paren(toks);
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
                let span = toks.next().unwrap().pos();
                IntermediateValue::Value(super_selector.into_value()).span(span)
            }
            '#' => {
                if let Some(Token { kind: '{', pos }) = toks.peek_forward(1) {
                    let span_before = *pos;
                    toks.reset_view();
                    return Some(Self::ident(toks, scope, super_selector, span_before));
                }
                toks.reset_view();
                toks.next();
                let hex = match parse_hex(toks, scope, super_selector, span) {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                };
                IntermediateValue::Value(hex.node).span(hex.span)
            }
            q @ '"' | q @ '\'' => {
                let span_start = toks.next().unwrap().pos();
                let Spanned { node, span } =
                    match parse_quoted_string(toks, scope, q, super_selector) {
                        Ok(v) => v,
                        Err(e) => return Some(Err(e)),
                    };
                IntermediateValue::Value(node).span(span_start.merge(span))
            }
            '[' => {
                let mut span = toks.next().unwrap().pos();
                let mut inner = read_until_closing_square_brace(toks);
                if let Some(last_tok) = inner.pop() {
                    if last_tok.kind != ']' {
                        return Some(Err(("expected \"]\".", span).into()));
                    }
                    span = span.merge(last_tok.pos());
                }
                IntermediateValue::Bracketed(inner).span(span)
            }
            '$' => {
                toks.next();
                let val = match eat_ident_no_interpolation(toks, false, span) {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                };
                IntermediateValue::Value(
                    match scope.get_var(val.clone()) {
                        Ok(v) => v,
                        Err(e) => return Some(Err(e)),
                    }
                    .node,
                )
                .span(val.span)
            }
            '+' => {
                let span = toks.next().unwrap().pos();
                IntermediateValue::Op(Op::Plus).span(span)
            }
            '-' => {
                let span = toks.next().unwrap().pos();
                IntermediateValue::Op(Op::Minus).span(span)
            }
            '*' => {
                let span = toks.next().unwrap().pos();
                IntermediateValue::Op(Op::Mul).span(span)
            }
            '%' => {
                let span = toks.next().unwrap().pos();
                IntermediateValue::Op(Op::Rem).span(span)
            }
            ',' => {
                toks.next();
                IntermediateValue::Comma.span(span)
            }
            q @ '>' | q @ '<' => {
                let mut span = toks.next().unwrap().pos();
                IntermediateValue::Op(if let Some(Token { kind: '=', .. }) = toks.peek() {
                    span = span.merge(toks.next().unwrap().pos());
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
                let mut span = toks.next().unwrap().pos();
                if let Some(Token { kind: '=', pos }) = toks.next() {
                    span = span.merge(pos);
                    IntermediateValue::Op(Op::Equal).span(span)
                } else {
                    return Some(Err(("expected \"=\".", span).into()));
                }
            }
            '!' => {
                let mut span = toks.next().unwrap().pos();
                if let Some(Token { kind: '=', .. }) = toks.peek() {
                    span = span.merge(toks.next().unwrap().pos());
                    return Some(Ok(IntermediateValue::Op(Op::NotEqual).span(span)));
                }
                devour_whitespace(toks);
                let v = match eat_ident(toks, scope, super_selector, span) {
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
                let span = toks.next().unwrap().pos();
                match toks.peek() {
                    Some(Token { kind: '/', .. }) => {
                        read_until_newline(toks);
                        devour_whitespace(toks);
                        IntermediateValue::Whitespace.span(span)
                    }
                    Some(Token { kind: '*', .. }) => {
                        toks.next();
                        match eat_comment(toks, &Scope::new(), &Selector::new()) {
                            Ok(..) => {}
                            Err(e) => return Some(Err(e)),
                        }
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
}
