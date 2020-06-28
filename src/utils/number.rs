use codemap::Spanned;

use peekmore::PeekMoreIterator;

use crate::{error::SassResult, Token};

#[derive(Debug)]
pub(crate) struct ParsedNumber {
    /// The full number excluding the decimal
    ///
    /// E.g. for `1.23`, this would be `"123"`
    pub num: String,

    /// The length of the decimal
    ///
    /// E.g. for `1.23`, this would be `2`
    pub dec_len: usize,

    /// The number following e in a scientific notated number
    ///
    /// E.g. for `1e23`, this would be `"23"`,
    /// for `1`, this would be an empty string
    // TODO: maybe we just return a bigint?
    pub times_ten: String,

    /// Whether or not `times_ten` is negative
    ///
    /// E.g. for `1e-23` this would be `true`,
    /// for `1e23` this would be `false`
    pub times_ten_is_postive: bool,
}

impl ParsedNumber {
    pub const fn new(
        num: String,
        dec_len: usize,
        times_ten: String,
        times_ten_is_postive: bool,
    ) -> Self {
        Self {
            num,
            dec_len,
            times_ten,
            times_ten_is_postive,
        }
    }
}

pub(crate) fn eat_number<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
) -> SassResult<Spanned<ParsedNumber>> {
    let mut whole = String::with_capacity(1);
    // TODO: merge this span with chars
    let span = toks.peek().unwrap().pos;
    eat_whole_number(toks, &mut whole);

    if toks.peek().is_none() {
        return Ok(Spanned {
            node: ParsedNumber::new(whole, 0, String::new(), true),
            span,
        });
    }

    let mut dec = String::new();

    let next_tok = *toks.peek().unwrap();

    if next_tok.kind == '.' {
        toks.next();
        eat_whole_number(toks, &mut dec);

        if dec.is_empty() {
            return Err(("Expected digit.", next_tok.pos()).into());
        }
    }

    let mut times_ten = String::new();
    let mut times_ten_is_postive = true;
    #[allow(clippy::never_loop)]
    loop {
        if let Some(Token { kind: 'e', .. }) | Some(Token { kind: 'E', .. }) = toks.peek() {
            let t = if let Some(tok) = toks.peek_forward(1) {
                *tok
            } else {
                break;
            };

            match t.kind {
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
                return Err(("Expected digit.", toks.peek().unwrap_or(&t).pos).into());
            }
        }
        break;
    }

    toks.reset_cursor();

    whole.push_str(&dec);

    Ok(Spanned {
        node: ParsedNumber::new(whole, dec.len(), times_ten, times_ten_is_postive),
        span,
    })
}

fn eat_whole_number<I: Iterator<Item = Token>>(toks: &mut PeekMoreIterator<I>, buf: &mut String) {
    while let Some(c) = toks.peek() {
        if !c.kind.is_ascii_digit() {
            break;
        }
        let tok = toks.next().unwrap();
        buf.push(tok.kind);
    }
}
