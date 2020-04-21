use std::iter::Iterator;

use codemap::Spanned;

use peekmore::PeekMoreIterator;

use crate::error::SassResult;
use crate::Token;

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

    let next_tok = *toks.peek().unwrap();

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
