use std::borrow::Cow;
use std::iter::Iterator;

use codemap::Spanned;

use peekmore::PeekMoreIterator;

use crate::error::SassResult;
use crate::Token;

pub(crate) struct ParsedNumber {
    pub v: String,
    // TODO: maybe we just return a bigint?
    pub times_ten: Cow<'static, str>,
    pub times_ten_is_postive: bool,
}

impl ParsedNumber {
    pub fn new(v: String, times_ten: Cow<'static, str>, times_ten_is_postive: bool) -> Self {
        Self {
            v,
            times_ten,
            times_ten_is_postive,
        }
    }
}

pub(crate) fn eat_number<'a, I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
) -> SassResult<Spanned<ParsedNumber>> {
    let mut whole = String::new();
    // TODO: merge this span with chars
    let span = if let Some(tok) = toks.peek() {
        tok.pos()
    } else {
        todo!()
    };
    eat_whole_number(toks, &mut whole);

    if toks.peek().is_none() {
        return Ok(Spanned {
            node: ParsedNumber::new(whole, Cow::from("0"), true),
            span,
        });
    }

    let mut dec = String::new();

    let next_tok = *toks.peek().unwrap();

    if next_tok.kind == '.' {
        toks.next();
        dec.push('.');
        eat_whole_number(toks, &mut dec);
    }

    if dec.len() == 1 {
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
                    let Token { kind, pos } = toks.peek().unwrap().clone();
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
        node: ParsedNumber::new(
            whole,
            if !times_ten.is_empty() {
                Cow::from(times_ten)
            } else {
                Cow::from("0")
            },
            times_ten_is_postive,
        ),
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
