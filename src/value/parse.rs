use std::convert::TryFrom;
use std::iter::{Iterator, Peekable};
use std::mem;

use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::pow;

use crate::args::eat_call_args;
use crate::builtin::GLOBAL_FUNCTIONS;
use crate::color::Color;
use crate::common::{Brackets, ListSeparator, Op, QuoteKind};
use crate::error::SassResult;
use crate::scope::Scope;
use crate::selector::Selector;
use crate::unit::Unit;
use crate::utils::{
    devour_whitespace, eat_comment, eat_ident, eat_ident_no_interpolation, eat_number,
    parse_interpolation, parse_quoted_string, read_until_char, read_until_closing_paren,
    read_until_closing_square_brace, read_until_newline, IsWhitespace,
};
use crate::value::Value;
use crate::Token;

use super::map::SassMap;
use super::number::Number;

fn parse_hex<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<Value> {
    let mut s = String::with_capacity(8);
    if toks
        .peek()
        .ok_or("Expected identifier.")?
        .kind
        .is_ascii_digit()
    {
        while let Some(c) = toks.peek() {
            if !c.kind.is_ascii_hexdigit() || s.len() == 8 {
                break;
            }
            s.push(toks.next().unwrap().kind);
        }
    } else {
        let i = eat_ident(toks, scope, super_selector)?;
        if i.chars().all(|c| c.is_ascii_hexdigit()) {
            s = i;
        } else {
            return Ok(Value::Ident(format!("#{}", i), QuoteKind::None));
        }
    }
    match s.len() {
        3 => {
            let v = match u16::from_str_radix(&s, 16) {
                Ok(a) => a,
                Err(_) => return Ok(Value::Ident(format!("#{}", s), QuoteKind::None)),
            };
            let red = (((v & 0xf00) >> 8) * 0x11) as u8;
            let green = (((v & 0x0f0) >> 4) * 0x11) as u8;
            let blue = ((v & 0x00f) * 0x11) as u8;
            Ok(Value::Color(Color::new(
                red,
                green,
                blue,
                1,
                format!("#{}", s),
            )))
        }
        4 => {
            let v = match u16::from_str_radix(&s, 16) {
                Ok(a) => a,
                Err(_) => return Ok(Value::Ident(format!("#{}", s), QuoteKind::None)),
            };
            let red = (((v & 0xf000) >> 12) * 0x11) as u8;
            let green = (((v & 0x0f00) >> 8) * 0x11) as u8;
            let blue = (((v & 0x00f0) >> 4) * 0x11) as u8;
            let alpha = ((v & 0x000f) * 0x11) as u8;
            Ok(Value::Color(Color::new(
                red,
                green,
                blue,
                alpha,
                format!("#{}", s),
            )))
        }
        6 => {
            let v = match u32::from_str_radix(&s, 16) {
                Ok(a) => a,
                Err(_) => return Ok(Value::Ident(format!("#{}", s), QuoteKind::None)),
            };
            let red = ((v & 0x00ff_0000) >> 16) as u8;
            let green = ((v & 0x0000_ff00) >> 8) as u8;
            let blue = (v & 0x0000_00ff) as u8;
            Ok(Value::Color(Color::new(
                red,
                green,
                blue,
                1,
                format!("#{}", s),
            )))
        }
        8 => {
            let v = match u32::from_str_radix(&s, 16) {
                Ok(a) => a,
                Err(_) => return Ok(Value::Ident(format!("#{}", s), QuoteKind::None)),
            };
            let red = ((v & 0xff00_0000) >> 24) as u8;
            let green = ((v & 0x00ff_0000) >> 16) as u8;
            let blue = ((v & 0x0000_ff00) >> 8) as u8;
            let alpha = (v & 0x0000_00ff) as u8;
            Ok(Value::Color(Color::new(
                red,
                green,
                blue,
                alpha,
                format!("#{}", s),
            )))
        }
        _ => Err("Expected hex digit.".into()),
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

impl IsWhitespace for IntermediateValue {
    fn is_whitespace(&self) -> bool {
        if self == &IntermediateValue::Whitespace {
            return true;
        }
        false
    }
}

fn eat_op<I: Iterator<Item = IntermediateValue>>(
    iter: &mut Peekable<I>,
    scope: &Scope,
    super_selector: &Selector,
    op: Op,
    space_separated: &mut Vec<Value>,
) -> SassResult<()> {
    match op {
        Op::Not => {
            devour_whitespace(iter);
            let right = single_value(iter, scope, super_selector)?;
            space_separated.push(Value::UnaryOp(op, Box::new(right)));
        }
        Op::Plus => {
            if let Some(left) = space_separated.pop() {
                devour_whitespace(iter);
                let right = single_value(iter, scope, super_selector)?;
                space_separated.push(Value::BinaryOp(Box::new(left), op, Box::new(right)));
            } else {
                devour_whitespace(iter);
                let right = single_value(iter, scope, super_selector)?;
                space_separated.push(Value::UnaryOp(op, Box::new(right)));
            }
        }
        Op::Minus => {
            if devour_whitespace(iter) {
                let right = single_value(iter, scope, super_selector)?;
                if let Some(left) = space_separated.pop() {
                    space_separated.push(Value::BinaryOp(Box::new(left), op, Box::new(right)));
                } else {
                    space_separated.push(Value::UnaryOp(op, Box::new(right)));
                }
            } else {
                let right = single_value(iter, scope, super_selector)?;
                space_separated.push(Value::UnaryOp(op, Box::new(right)));
            }
        }
        Op::And | Op::Or => {
            devour_whitespace(iter);
            if iter.peek().is_none() {
                space_separated.push(Value::Ident(op.to_string(), QuoteKind::None));
            } else if let Some(left) = space_separated.pop() {
                devour_whitespace(iter);
                let right = single_value(iter, scope, super_selector)?;
                space_separated.push(Value::BinaryOp(Box::new(left), op, Box::new(right)));
            } else {
                return Err("Expected expression.".into());
            }
        }
        _ => {
            if let Some(left) = space_separated.pop() {
                devour_whitespace(iter);
                let right = single_value(iter, scope, super_selector)?;
                space_separated.push(Value::BinaryOp(Box::new(left), op, Box::new(right)));
            } else {
                return Err("Expected expression.".into());
            }
        }
    }
    Ok(())
}

fn single_value<I: Iterator<Item = IntermediateValue>>(
    iter: &mut Peekable<I>,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<Value> {
    Ok(match iter.next().ok_or("Expected expression.")? {
        IntermediateValue::Value(v) => v,
        IntermediateValue::Op(op) => match op {
            Op::Minus => {
                devour_whitespace(iter);
                (-single_value(iter, scope, super_selector)?)?
            }
            Op::Not => {
                devour_whitespace(iter);
                Value::UnaryOp(op, Box::new(single_value(iter, scope, super_selector)?))
            }
            _ => todo!(),
        },
        IntermediateValue::Whitespace => unreachable!(),
        IntermediateValue::Comma => return Err("Expected expression.".into()),
        IntermediateValue::Bracketed(t) => {
            match Value::from_tokens(&mut t.into_iter().peekable(), scope, super_selector)? {
                Value::List(v, sep, Brackets::None) => Value::List(v, sep, Brackets::Bracketed),
                v => Value::List(vec![v], ListSeparator::Space, Brackets::Bracketed),
            }
        }
        IntermediateValue::Paren(t) => {
            let inner = Value::from_tokens(&mut t.into_iter().peekable(), scope, super_selector)?;
            Value::Paren(Box::new(inner))
        }
    })
}

impl Value {
    pub fn from_tokens<I: Iterator<Item = Token>>(
        toks: &mut Peekable<I>,
        scope: &Scope,
        super_selector: &Selector,
    ) -> SassResult<Self> {
        let mut intermediate_values = Vec::new();
        while toks.peek().is_some() {
            intermediate_values.push(Self::parse_intermediate_value(toks, scope, super_selector)?);
        }
        let mut space_separated = Vec::new();
        let mut comma_separated = Vec::new();
        let mut iter = intermediate_values.into_iter().peekable();
        while let Some(val) = iter.next() {
            match val {
                IntermediateValue::Value(v) => space_separated.push(v),
                IntermediateValue::Op(op) => {
                    eat_op(&mut iter, scope, super_selector, op, &mut space_separated)?;
                }
                IntermediateValue::Whitespace => continue,
                IntermediateValue::Comma => {
                    if space_separated.len() == 1 {
                        comma_separated.push(space_separated.pop().unwrap());
                    } else {
                        comma_separated.push(Value::List(
                            mem::take(&mut space_separated),
                            ListSeparator::Space,
                            Brackets::None,
                        ));
                    }
                }
                IntermediateValue::Bracketed(t) => space_separated.push(match Value::from_tokens(
                    &mut t.into_iter().peekable(),
                    scope,
                    super_selector,
                )? {
                    Value::List(v, sep, Brackets::None) => Value::List(v, sep, Brackets::Bracketed),
                    v => Value::List(vec![v], ListSeparator::Space, Brackets::Bracketed),
                }),
                IntermediateValue::Paren(t) => {
                    if t.is_empty() {
                        space_separated.push(Value::List(
                            Vec::new(),
                            ListSeparator::Space,
                            Brackets::None,
                        ));
                        continue;
                    }

                    let paren_toks = &mut t.into_iter().peekable();

                    let mut map = SassMap::new();
                    let key = Value::from_tokens(
                        &mut read_until_char(paren_toks, ':').into_iter().peekable(),
                        scope,
                        super_selector,
                    )?;

                    if paren_toks.peek().is_none() {
                        space_separated.push(Value::Paren(Box::new(key)));
                        continue;
                    }

                    let val = Self::from_tokens(
                        &mut read_until_char(paren_toks, ',').into_iter().peekable(),
                        scope,
                        super_selector,
                    )?;

                    map.insert(key, val);

                    if paren_toks.peek().is_none() {
                        space_separated.push(Value::Map(map));
                        continue;
                    }

                    loop {
                        let key = Value::from_tokens(
                            &mut read_until_char(paren_toks, ':').into_iter().peekable(),
                            scope,
                            super_selector,
                        )?;
                        devour_whitespace(paren_toks);
                        let val = Self::from_tokens(
                            &mut read_until_char(paren_toks, ',').into_iter().peekable(),
                            scope,
                            super_selector,
                        )?;
                        devour_whitespace(paren_toks);
                        map.insert(key, val);
                        if paren_toks.peek().is_none() {
                            break;
                        }
                    }
                    space_separated.push(Value::Map(map))
                }
            }
        }

        Ok(if !comma_separated.is_empty() {
            if space_separated.len() == 1 {
                comma_separated.push(space_separated.pop().unwrap());
            } else if !space_separated.is_empty() {
                comma_separated.push(Value::List(
                    space_separated,
                    ListSeparator::Space,
                    Brackets::None,
                ));
            }
            Value::List(comma_separated, ListSeparator::Comma, Brackets::None)
        } else if space_separated.len() == 1 {
            space_separated.pop().unwrap()
        } else {
            Value::List(space_separated, ListSeparator::Space, Brackets::None)
        })
    }

    fn ident<I: Iterator<Item = Token>>(
        toks: &mut Peekable<I>,
        scope: &Scope,
        super_selector: &Selector,
    ) -> SassResult<IntermediateValue> {
        let mut s = eat_ident(toks, scope, super_selector)?;
        match toks.peek() {
            Some(Token { kind: '(', .. }) => {
                toks.next();
                let func = match scope.get_fn(&s) {
                    Ok(f) => f,
                    Err(_) => match GLOBAL_FUNCTIONS.get(&s) {
                        Some(f) => {
                            return Ok(IntermediateValue::Value(f(
                                &mut eat_call_args(toks, scope, super_selector)?,
                                scope,
                            )?))
                        }
                        None => {
                            s.push('(');
                            let mut unclosed_parens = 0;
                            while let Some(t) = toks.next() {
                                match &t.kind {
                                    '(' => {
                                        unclosed_parens += 1;
                                    }
                                    '#' if toks.next().unwrap().kind == '{' => s.push_str(
                                        &parse_interpolation(toks, scope, super_selector)?
                                            .to_string(),
                                    ),
                                    '$' => s.push_str(
                                        &scope
                                            .get_var(&eat_ident(toks, scope, super_selector)?)?
                                            .to_string(),
                                    ),
                                    ')' => {
                                        if unclosed_parens <= 1 {
                                            s.push(')');
                                            break;
                                        } else {
                                            unclosed_parens -= 1;
                                        }
                                    }
                                    _ => {}
                                }
                                s.push_str(&t.kind.to_string());
                            }
                            return Ok(IntermediateValue::Value(Value::Ident(s, QuoteKind::None)));
                        }
                    },
                };
                Ok(IntermediateValue::Value(
                    func.clone()
                        .args(&mut eat_call_args(toks, scope, super_selector)?)?
                        .call(super_selector, func.body())?,
                ))
            }
            _ => {
                if let Ok(c) = crate::color::ColorName::try_from(s.as_ref()) {
                    Ok(IntermediateValue::Value(Value::Color(c.into_color(s))))
                } else {
                    match s.to_ascii_lowercase().as_str() {
                        "true" => Ok(IntermediateValue::Value(Value::True)),
                        "false" => Ok(IntermediateValue::Value(Value::False)),
                        "null" => Ok(IntermediateValue::Value(Value::Null)),
                        "not" => Ok(IntermediateValue::Op(Op::Not)),
                        "and" => Ok(IntermediateValue::Op(Op::And)),
                        "or" => Ok(IntermediateValue::Op(Op::Or)),
                        _ => Ok(IntermediateValue::Value(Value::Ident(s, QuoteKind::None))),
                    }
                }
            }
        }
    }

    fn parse_intermediate_value<I: Iterator<Item = Token>>(
        toks: &mut Peekable<I>,
        scope: &Scope,
        super_selector: &Selector,
    ) -> SassResult<IntermediateValue> {
        if devour_whitespace(toks) {
            return Ok(IntermediateValue::Whitespace);
        }
        let kind = match toks.peek() {
            Some(v) => v.kind,
            None => panic!("unexpected eof"),
        };
        match kind {
            ',' => {
                toks.next();
                Ok(IntermediateValue::Comma)
            }
            '0'..='9' | '.' => {
                let val = eat_number(toks)?;
                let unit = if let Some(tok) = toks.peek() {
                    match tok.kind {
                        'a'..='z' | 'A'..='Z' | '_' => {
                            Unit::from(&eat_ident(toks, scope, super_selector)?)
                        }
                        '%' => {
                            toks.next();
                            Unit::Percent
                        }
                        _ => Unit::None,
                    }
                } else {
                    Unit::None
                };
                let n = if let Ok(v) = val.parse::<BigRational>() {
                    // the number is an integer!
                    v
                // the number is floating point
                } else {
                    let mut num = String::new();
                    let mut chars = val.chars();
                    let mut num_dec = 0;
                    while let Some(c) = chars.next() {
                        if c == '.' {
                            break;
                        }
                        num.push(c);
                    }
                    for c in chars {
                        num_dec += 1;
                        num.push(c);
                    }
                    BigRational::new(num.parse().unwrap(), pow(BigInt::from(10), num_dec))
                };
                Ok(IntermediateValue::Value(Value::Dimension(
                    Number::new(n),
                    unit,
                )))
            }
            '(' => {
                toks.next();
                let mut inner = read_until_closing_paren(toks);
                // todo: the above shouldn't eat the closing paren
                if !inner.is_empty() && inner.pop().unwrap().kind != ')' {
                    return Err("expected \")\".".into());
                }
                Ok(IntermediateValue::Paren(inner))
            }
            '&' => {
                toks.next();
                Ok(IntermediateValue::Value(Value::Ident(
                    super_selector.to_string(),
                    QuoteKind::None,
                )))
            }
            '#' => {
                if let Ok(s) = eat_ident(toks, scope, super_selector) {
                    Ok(IntermediateValue::Value(Value::Ident(s, QuoteKind::None)))
                } else {
                    Ok(IntermediateValue::Value(parse_hex(
                        toks,
                        scope,
                        super_selector,
                    )?))
                }
            }
            _ if kind.is_ascii_alphabetic()
                || kind == '_'
                || kind == '\\'
                || (!kind.is_ascii() && !kind.is_control()) =>
            {
                Self::ident(toks, scope, super_selector)
            }
            q @ '"' | q @ '\'' => {
                toks.next();
                Ok(IntermediateValue::Value(parse_quoted_string(
                    toks,
                    scope,
                    q,
                    super_selector,
                )?))
            }
            '[' => {
                toks.next();
                let mut inner = read_until_closing_square_brace(toks);
                inner.pop();
                Ok(IntermediateValue::Bracketed(inner))
            }
            '$' => {
                toks.next();
                Ok(IntermediateValue::Value(
                    scope.get_var(&eat_ident_no_interpolation(toks)?)?,
                ))
            }
            '@' => Err("expected \";\".".into()),
            '+' => {
                toks.next();
                Ok(IntermediateValue::Op(Op::Plus))
            }
            '-' => {
                toks.next();
                Ok(IntermediateValue::Op(Op::Minus))
            }
            '*' => {
                toks.next();
                Ok(IntermediateValue::Op(Op::Mul))
            }
            '%' => {
                toks.next();
                Ok(IntermediateValue::Op(Op::Rem))
            }
            q @ '>' | q @ '<' => {
                toks.next();
                Ok(IntermediateValue::Op(if toks.peek().unwrap().kind == '=' {
                    toks.next();
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
                }))
            }
            '=' => {
                toks.next();
                if toks.next().unwrap().kind == '=' {
                    Ok(IntermediateValue::Op(Op::Equal))
                } else {
                    Err("expected \"=\".".into())
                }
            }
            '!' => {
                toks.next();
                if toks.peek().is_some() && toks.peek().unwrap().kind == '=' {
                    toks.next();
                    return Ok(IntermediateValue::Op(Op::NotEqual));
                }
                devour_whitespace(toks);
                let v = eat_ident(toks, scope, super_selector)?;
                if v.to_ascii_lowercase().as_str() == "important" {
                    Ok(IntermediateValue::Value(Value::Important))
                } else {
                    Err("Expected \"important\".".into())
                }
            }
            '/' => {
                toks.next();
                if toks.peek().is_none() {
                    return Err("Expected expression.".into());
                }
                if '*' == toks.peek().unwrap().kind {
                    toks.next();
                    eat_comment(toks, &Scope::new(), &Selector::new())?;
                    Ok(IntermediateValue::Whitespace)
                } else if '/' == toks.peek().unwrap().kind {
                    read_until_newline(toks);
                    devour_whitespace(toks);
                    Ok(IntermediateValue::Whitespace)
                } else {
                    Ok(IntermediateValue::Op(Op::Div))
                }
            }
            ':' | '?' | ')' => Err("expected \";\".".into()),
            v if v.is_control() => Err("Expected expression.".into()),
            v => {
                dbg!(v);
                panic!("Unexpected token in value parsing")
            }
        }
    }
}
