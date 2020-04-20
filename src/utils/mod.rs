use std::iter::Iterator;

use codemap::Spanned;

use peekmore::PeekMoreIterator;

use crate::error::SassResult;
use crate::selector::Selector;
use crate::value::Value;
use crate::{Scope, Token};

pub(crate) use chars::*;
pub(crate) use comment_whitespace::*;
pub(crate) use read_until::*;
pub(crate) use strings::*;
pub(crate) use variables::*;

mod chars;
mod comment_whitespace;
mod read_until;
mod strings;
mod variables;

pub(crate) fn parse_interpolation<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<Spanned<Value>> {
    let val = Value::from_vec(read_until_closing_curly_brace(toks), scope, super_selector)?;
    toks.next();
    Ok(Spanned {
        node: val.node.eval(val.span)?.node.unquote(),
        span: val.span,
    })
}

pub(crate) fn eat_number<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
) -> SassResult<Spanned<String>> {
    let mut whole = String::new();
    let mut span = if let Some(tok) = toks.peek() {
        tok.pos()
    } else {
        todo!()
    };
    while let Some(c) = toks.peek() {
        if !c.kind.is_numeric() {
            break;
        }
        let tok = toks.next().unwrap();
        span = span.merge(tok.pos());
        whole.push(tok.kind);
    }

    if toks.peek().is_none() {
        return Ok(Spanned { node: whole, span });
    }

    let mut dec = String::new();

    let next_tok = toks.peek().unwrap().clone();

    if next_tok.kind == '.' {
        toks.next();
        dec.push('.');
        while let Some(c) = toks.peek() {
            if !c.kind.is_numeric() {
                break;
            }
            let tok = toks.next().unwrap();
            span = span.merge(tok.pos());
            dec.push(tok.kind);
        }
    }

    if dec.len() == 1 {
        return Err(("Expected digit.", next_tok.pos()).into());
    }

    whole.push_str(&dec);
    Ok(Spanned { node: whole, span })
}
