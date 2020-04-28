use std::iter::Iterator;

use codemap::Spanned;

use peekmore::PeekMoreIterator;

use crate::error::SassResult;
use crate::Token;

#[derive(Debug)]
pub(crate) struct ParsedNumber {
    pub num: String,
    pub dec_len: usize,
    // TODO: maybe we just return a bigint?
    pub times_ten: String,
    pub times_ten_is_postive: bool,
    pub is_float: bool,
}

impl ParsedNumber {
    pub const fn new(
        num: String,
        dec_len: usize,
        times_ten: String,
        times_ten_is_postive: bool,
        is_float: bool,
    ) -> Self {
        Self {
            num,
            dec_len,
            times_ten,
            times_ten_is_postive,
            is_float,
        }
    }
}

pub(crate) fn eat_number<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
) -> SassResult<Spanned<ParsedNumber>> {
    let mut whole = String::with_capacity(1);
    // TODO: merge this span with chars
    let span = if let Some(tok) = toks.peek() {
        tok.pos()
    } else {
        todo!()
    };
    eat_whole_number(toks, &mut whole);

    if toks.peek().is_none() {
        return Ok(Spanned {
            node: ParsedNumber::new(whole, 0, String::new(), true, false),
            span,
        });
    }

    let mut dec = String::new();
    let mut is_float = false;

    let next_tok = *toks.peek().unwrap();

    if next_tok.kind == '.' {
        toks.next();
        is_float = true;
        eat_whole_number(toks, &mut dec);
    }

    if dec.is_empty() && is_float {
        return Err(("Expected digit.", next_tok.pos()).into());
    }

    let mut times_ten = String::new();
    let mut times_ten_is_postive = true;
    loop {
        match toks.peek() {
            // TODO: https://github.com/rust-lang/rust/issues/54883
            Some(Token { kind: 'e', .. }) | Some(Token { kind: 'E', .. }) => {
                if toks.peek_forward(1).is_none() {
                    break;
                } else {
                    let Token { kind, pos } = *toks.peek().unwrap();
                    match kind {
                        '-' => {
                            toks.next();
                            times_ten_is_postive = false;
                        }
                        '0'..='9' => {}
                        _ => break,
                    }

                    toks.next();

                    eat_whole_number(toks, &mut times_ten);

                    if times_ten.is_empty() && !times_ten_is_postive {
                        if let Some(t) = toks.peek() {
                            return Err(("Expected digit.", t.pos()).into());
                        }
                        return Err(("Expected digit.", pos).into());
                    }
                }
            }
            Some(..) | None => break,
        }
        break;
    }

    toks.reset_view();

    whole.push_str(&dec);

    Ok(Spanned {
        node: ParsedNumber::new(whole, dec.len(), times_ten, times_ten_is_postive, is_float),
        span,
    })
}

fn eat_whole_number<I: Iterator<Item = Token>>(toks: &mut PeekMoreIterator<I>, buf: &mut String) {
    while let Some(c) = toks.peek() {
        if !c.kind.is_numeric() {
            break;
        }
        let tok = toks.next().unwrap();
        buf.push(tok.kind);
    }
}
