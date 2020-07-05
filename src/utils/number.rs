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
    let mut span = toks.peek().unwrap().pos;
    let mut whole = eat_whole_number(toks);

    if toks.peek().is_none() {
        return Ok(Spanned {
            node: ParsedNumber::new(whole, 0, String::new(), true),
            span,
        });
    }

    let next_tok = *toks.peek().unwrap();

    let dec_len = if next_tok.kind == '.' {
        toks.next();

        let dec = eat_whole_number(toks);
        if dec.is_empty() {
            return Err(("Expected digit.", next_tok.pos()).into());
        }

        whole.push_str(&dec);

        dec.len()
    } else {
        0
    };

    let mut times_ten = String::new();
    let mut times_ten_is_postive = true;
    if let Some(Token { kind: 'e', .. }) | Some(Token { kind: 'E', .. }) = toks.peek() {
        if let Some(&tok) = toks.peek_next() {
            if tok.kind == '-' {
                toks.next();
                times_ten_is_postive = false;

                toks.next();
                times_ten = eat_whole_number(toks);

                if times_ten.is_empty() {
                    return Err(("Expected digit.", toks.peek().unwrap_or(&tok).pos).into());
                }
            } else if matches!(tok.kind, '0'..='9') {
                toks.next();
                times_ten = eat_whole_number(toks);

                if times_ten.len() > 2 {
                    return Err(("Exponent too large.", toks.peek().unwrap_or(&tok).pos).into());
                }
            }
        }
    }

    if let Ok(Some(Token { pos, .. })) = toks.peek_previous() {
        span = span.merge(*pos);
    }

    toks.reset_cursor();

    Ok(Spanned {
        node: ParsedNumber::new(whole, dec_len, times_ten, times_ten_is_postive),
        span,
    })
}

pub(crate) fn eat_whole_number<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
) -> String {
    let mut buf = String::new();
    while let Some(c) = toks.peek() {
        if !c.kind.is_ascii_digit() {
            break;
        }
        let tok = toks.next().unwrap();
        buf.push(tok.kind);
    }
    buf
}
