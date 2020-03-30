use std::convert::TryFrom;
use std::iter::{Iterator, Peekable};

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
    devour_whitespace, eat_comment, eat_ident, eat_number, parse_interpolation,
    parse_quoted_string, read_until_newline,
};
use crate::value::Value;
use crate::Token;

use super::number::Number;

fn parse_hex<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<Value> {
    let mut s = String::with_capacity(8);
    if toks.peek().unwrap().kind.is_ascii_digit() {
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

impl Value {
    pub fn from_tokens<I: Iterator<Item = Token>>(
        toks: &mut Peekable<I>,
        scope: &Scope,
        super_selector: &Selector,
    ) -> SassResult<Self> {
        let left = Self::_from_tokens(toks, scope, super_selector)?;
        devour_whitespace(toks);
        let next = match toks.peek() {
            Some(x) => x,
            None => return Ok(left),
        };
        match next.kind {
            ';' | ')' | ']' => Ok(left),
            ',' => {
                toks.next();
                devour_whitespace(toks);
                if toks.peek() == None {
                    return Ok(Value::List(
                        vec![left],
                        ListSeparator::Comma,
                        Brackets::None,
                    ));
                } else if let Some(tok) = toks.peek() {
                    if tok.kind == ')' {
                        return Ok(Value::List(
                            vec![left],
                            ListSeparator::Comma,
                            Brackets::None,
                        ));
                    } else if tok.kind == ']' {
                        return Ok(Value::List(
                            vec![left],
                            ListSeparator::Comma,
                            Brackets::Bracketed,
                        ));
                    }
                }
                let right = Self::from_tokens(toks, scope, super_selector)?;
                if let Value::List(v, ListSeparator::Comma, Brackets::None) = right {
                    let mut v2 = vec![left];
                    v2.extend(v);
                    Ok(Value::List(v2, ListSeparator::Comma, Brackets::None))
                } else {
                    Ok(Value::List(
                        vec![left, right],
                        ListSeparator::Comma,
                        Brackets::None,
                    ))
                }
            }
            '+' | '*' | '%' => {
                let op = match next.kind {
                    '+' => Op::Plus,
                    '*' => Op::Mul,
                    '%' => Op::Rem,
                    _ => unsafe { std::hint::unreachable_unchecked() },
                };
                toks.next();
                devour_whitespace(toks);
                let right = Self::from_tokens(toks, scope, super_selector)?;
                Ok(Value::BinaryOp(Box::new(left), op, Box::new(right)))
            }
            '=' => {
                toks.next();
                if toks.peek().unwrap().kind == '=' {
                    toks.next();
                    devour_whitespace(toks);
                    let right = Self::from_tokens(toks, scope, super_selector)?;
                    Ok(Value::BinaryOp(Box::new(left), Op::Equal, Box::new(right)))
                } else {
                    return Err("expected \"=\".".into());
                }
            }
            '!' => {
                toks.next();
                if toks.peek().unwrap().kind == '=' {
                    toks.next();
                    devour_whitespace(toks);
                    let right = Self::from_tokens(toks, scope, super_selector)?;
                    Ok(Value::BinaryOp(
                        Box::new(left),
                        Op::NotEqual,
                        Box::new(right),
                    ))
                } else if eat_ident(toks, scope, super_selector)?
                    .to_ascii_lowercase()
                    .as_str()
                    == "important"
                {
                    Ok(Value::List(
                        vec![left, Value::Important],
                        ListSeparator::Space,
                        Brackets::None,
                    ))
                } else {
                    return Err("Expected \"important\".".into());
                }
            }
            '-' => {
                toks.next();
                if devour_whitespace(toks) {
                    let right = Self::from_tokens(toks, scope, super_selector)?;
                    Ok(Value::BinaryOp(Box::new(left), Op::Minus, Box::new(right)))
                } else {
                    let right = Self::from_tokens(toks, scope, super_selector)?;
                    if let Value::List(mut v, ListSeparator::Space, ..) = right {
                        let mut v2 = vec![left];
                        let val = v.remove(0);
                        v2.push((-val)?);
                        v2.extend(v);
                        Ok(Value::List(v2, ListSeparator::Space, Brackets::None))
                    } else {
                        Ok(Value::List(
                            vec![left, (-right)?],
                            ListSeparator::Space,
                            Brackets::None,
                        ))
                    }
                }
            }
            '/' => {
                toks.next();
                match toks.peek().unwrap().kind {
                    v @ '*' | v @ '/' => {
                        toks.next();
                        if v == '*' {
                            eat_comment(toks, &Scope::new(), &Selector::new())?;
                        } else {
                            read_until_newline(toks);
                        }
                        devour_whitespace(toks);
                        if toks.peek().is_none() {
                            return Ok(left);
                        }
                        let right = Self::from_tokens(toks, scope, super_selector)?;
                        if let Value::List(v, ListSeparator::Space, ..) = right {
                            let mut v2 = vec![left];
                            v2.extend(v);
                            Ok(Value::List(v2, ListSeparator::Space, Brackets::None))
                        } else {
                            Ok(Value::List(
                                vec![left, right],
                                ListSeparator::Space,
                                Brackets::None,
                            ))
                        }
                    }
                    _ => {
                        devour_whitespace(toks);
                        let right = Self::from_tokens(toks, scope, super_selector)?;
                        Ok(Value::BinaryOp(Box::new(left), Op::Div, Box::new(right)))
                    }
                }
            }
            _ => {
                devour_whitespace(toks);
                let right = Self::from_tokens(toks, scope, super_selector)?;
                if let Value::List(v, ListSeparator::Space, ..) = right {
                    let mut v2 = vec![left];
                    v2.extend(v);
                    Ok(Value::List(v2, ListSeparator::Space, Brackets::None))
                } else {
                    Ok(Value::List(
                        vec![left, right],
                        ListSeparator::Space,
                        Brackets::None,
                    ))
                }
            }
        }
    }

    fn _from_tokens<I: Iterator<Item = Token>>(
        toks: &mut Peekable<I>,
        scope: &Scope,
        super_selector: &Selector,
    ) -> SassResult<Self> {
        let kind = if let Some(tok) = toks.peek() {
            tok.kind
        } else {
            panic!("Unexpected EOF");
        };
        match kind {
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
                Ok(Value::Dimension(Number::new(n), unit))
            }
            '(' => {
                toks.next();
                devour_whitespace(toks);
                if toks.peek().unwrap().kind == ')' {
                    toks.next();
                    return Ok(Value::List(
                        Vec::new(),
                        ListSeparator::Space,
                        Brackets::None,
                    ));
                }
                let val = Self::from_tokens(toks, scope, super_selector)?;
                let next = toks.next();
                if next.is_none() || next.unwrap().kind != ')' {
                    return Err("expected \")\".".into());
                }
                Ok(Value::Paren(Box::new(val)))
            }
            '&' => {
                toks.next();
                Ok(Value::Ident(super_selector.to_string(), QuoteKind::None))
            }
            '#' => {
                if let Ok(s) = eat_ident(toks, scope, super_selector) {
                    Ok(Value::Ident(s, QuoteKind::None))
                } else {
                    Ok(parse_hex(toks, scope, super_selector)?)
                }
            }
            'a'..='z' | 'A'..='Z' | '_' | '\\' => {
                let mut s = eat_ident(toks, scope, super_selector)?;
                match toks.peek() {
                    Some(Token { kind: '(', .. }) => {
                        toks.next();
                        let func = match scope.get_fn(&s) {
                            Ok(f) => f,
                            Err(_) => match GLOBAL_FUNCTIONS.get(&s) {
                                Some(f) => {
                                    return f(
                                        &mut eat_call_args(toks, scope, super_selector)?,
                                        scope,
                                    )
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
                                                    .get_var(&eat_ident(
                                                        toks,
                                                        scope,
                                                        super_selector,
                                                    )?)?
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
                                    return Ok(Value::Ident(s, QuoteKind::None));
                                }
                            },
                        };
                        Ok(func
                            .clone()
                            .args(&mut eat_call_args(toks, scope, super_selector)?)?
                            .call(super_selector, func.body())?)
                    }
                    _ => {
                        if let Ok(c) = crate::color::ColorName::try_from(s.as_ref()) {
                            Ok(Value::Color(c.into_color(s)))
                        } else {
                            match s.to_ascii_lowercase().as_str() {
                                "true" => Ok(Value::True),
                                "false" => Ok(Value::False),
                                "null" => Ok(Value::Null),
                                _ => Ok(Value::Ident(s, QuoteKind::None)),
                            }
                        }
                    }
                }
            }
            q @ '"' | q @ '\'' => {
                toks.next();
                parse_quoted_string(toks, scope, q, super_selector)
            }
            '[' => {
                toks.next();
                if let Some(tok) = toks.peek() {
                    if tok.kind == ']' {
                        toks.next();
                        return Ok(Value::List(
                            Vec::new(),
                            ListSeparator::Space,
                            Brackets::Bracketed,
                        ));
                    }
                }
                let inner = Self::from_tokens(toks, scope, super_selector)?;
                devour_whitespace(toks);
                toks.next();
                Ok(match inner {
                    Value::List(v, sep, ..) => Value::List(v, sep, Brackets::Bracketed),
                    v => Value::List(vec![v], ListSeparator::Space, Brackets::Bracketed),
                })
            }
            '$' => {
                toks.next();
                Ok(scope.get_var(&eat_ident(toks, scope, super_selector)?)?)
            }
            '@' => Err("expected \";\".".into()),
            '+' => {
                toks.next();
                devour_whitespace(toks);
                let v = Self::_from_tokens(toks, scope, super_selector)?;
                Ok(Value::UnaryOp(Op::Plus, Box::new(v)))
            }
            '-' => {
                toks.next();
                devour_whitespace(toks);
                let v = Self::_from_tokens(toks, scope, super_selector)?;
                Ok(Value::UnaryOp(Op::Minus, Box::new(v)))
            }
            '!' => {
                toks.next();
                let v = eat_ident(toks, scope, super_selector)?;
                if v.to_ascii_lowercase().as_str() == "important" {
                    Ok(Value::Important)
                } else {
                    Err("Expected \"important\".".into())
                }
            }
            '/' => {
                toks.next();
                if '*' == toks.peek().unwrap().kind {
                    toks.next();
                    eat_comment(toks, &Scope::new(), &Selector::new())?;
                    Self::_from_tokens(toks, scope, super_selector)
                } else if '/' == toks.peek().unwrap().kind {
                    read_until_newline(toks);
                    devour_whitespace(toks);
                    Self::_from_tokens(toks, scope, super_selector)
                } else {
                    todo!()
                }
            }
            v if v.is_control() => Err("Expected expression.".into()),
            v => {
                dbg!(v);
                panic!("Unexpected token in value parsing")
            }
        }
    }
}
