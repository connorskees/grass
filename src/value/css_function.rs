use peekmore::PeekMoreIterator;

use crate::error::SassResult;
use crate::scope::Scope;
use crate::selector::Selector;
use crate::utils::{
    devour_whitespace, parse_interpolation, peek_escape, peek_until_closing_curly_brace,
    peek_whitespace,
};
use crate::Token;

pub(crate) fn eat_calc_args<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    scope: &Scope,
    super_selector: &Selector,
    buf: &mut String,
) -> SassResult<()> {
    buf.reserve(2);
    buf.push('(');
    let mut nesting = 0;
    while let Some(tok) = toks.next() {
        match tok.kind {
            ' ' | '\t' | '\n' => {
                devour_whitespace(toks);
                buf.push(' ');
            }
            '#' => {
                if toks.peek().is_some() && toks.peek().unwrap().kind == '{' {
                    let span = toks.next().unwrap().pos();
                    buf.push_str(
                        &parse_interpolation(toks, scope, super_selector)?.to_css_string(span)?,
                    );
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

pub(crate) fn is_special_function(s: &str) -> bool {
    s.starts_with("calc(")
        || s.starts_with("var(")
        || s.starts_with("env(")
        || s.starts_with("min(")
        || s.starts_with("max(")
}

pub(crate) fn eat_progid<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<String> {
    let mut string = String::new();
    let mut span = toks.peek().unwrap().pos();
    while let Some(tok) = toks.next() {
        span = span.merge(tok.pos());
        match tok.kind {
            'a'..='z' | 'A'..='Z' | '.' => {
                string.push(tok.kind);
            }
            '(' => {
                eat_calc_args(toks, scope, super_selector, &mut string)?;
                break;
            }
            _ => return Err(("expected \"(\".", span).into()),
        }
    }
    Ok(string)
}

pub(crate) fn try_eat_url<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<Option<String>> {
    let mut buf = String::from("url(");
    let mut peek_counter = 0;
    peek_counter += peek_whitespace(toks);
    while let Some(tok) = toks.peek() {
        let kind = tok.kind;
        toks.move_forward(1);
        peek_counter += 1;
        if kind == '!'
            || kind == '%'
            || kind == '&'
            || (kind >= '*' && kind <= '~')
            || kind as u32 >= 0x0080
        {
            buf.push(kind);
        } else if kind == '\\' {
            buf.push_str(&peek_escape(toks)?);
        } else if kind == '#' {
            let next = toks.peek();
            if next.is_some() && next.unwrap().kind == '{' {
                toks.move_forward(1);
                peek_counter += 1;
                let (interpolation, count) = peek_interpolation(toks, scope, super_selector)?;
                peek_counter += count;
                buf.push_str(&match interpolation.node {
                    Value::String(s, ..) => s,
                    v => v.to_css_string(interpolation.span)?.into(),
                });
            } else {
                buf.push('#');
            }
        } else if kind == ')' {
            buf.push(')');
            toks.take(peek_counter).for_each(drop);
            return Ok(Some(buf));
        } else if kind.is_whitespace() {
            peek_counter += peek_whitespace(toks);
            let next = match toks.peek() {
                Some(v) => v,
                None => break,
            };
            if next.kind == ')' {
                buf.push(')');
                toks.take(peek_counter + 1).for_each(drop);
                return Ok(Some(buf));
            } else {
                break;
            }
        } else {
            break;
        }
    }
    toks.reset_view();
    Ok(None)
}

use crate::value::Value;
use codemap::Spanned;

fn peek_interpolation<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<(Spanned<Value>, usize)> {
    let vec = peek_until_closing_curly_brace(toks);
    let peek_counter = vec.len();
    toks.move_forward(1);
    let val = Value::from_vec(vec, scope, super_selector)?;
    Ok((
        Spanned {
            node: val.node.eval(val.span)?.node.unquote(),
            span: val.span,
        },
        peek_counter,
    ))
}
