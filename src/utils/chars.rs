use std::iter::Iterator;

use peekmore::PeekMoreIterator;

use super::read_until_closing_quote;

use crate::error::SassResult;
use crate::Token;

/// Reads until the char is found, consuming the char,
/// or until the end of the iterator is hit
pub(crate) fn read_until_char<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    c: char,
) -> SassResult<Vec<Token>> {
    let mut v = Vec::new();
    while let Some(tok) = toks.next() {
        match tok.kind {
            '"' | '\'' => {
                v.push(tok);
                v.extend(read_until_closing_quote(toks, tok.kind)?);
                continue;
            }
            t if t == c => break,
            _ => {}
        }
        v.push(tok)
    }
    Ok(v)
}

pub(crate) fn hex_char_for(number: u32) -> char {
    debug_assert!(number < 0x10);
    std::char::from_u32(if number < 0xA {
        0x30 + number
    } else {
        0x61 - 0xA + number
    })
    .unwrap()
}

pub(crate) fn is_name(c: char) -> bool {
    is_name_start(c) || c.is_digit(10) || c == '-'
}

pub(crate) fn is_name_start(c: char) -> bool {
    // NOTE: in the dart-sass implementation, identifiers cannot start
    // with numbers. We explicitly differentiate from the reference
    // implementation here in order to support selectors beginning with numbers.
    // This can be considered a hack and in the future it would be nice to refactor
    // how this is handled.
    c == '_' || c.is_alphanumeric() || c as u32 >= 0x0080
}

pub(crate) fn as_hex(c: char) -> u32 {
    match c {
        '0'..='9' => c as u32 - '0' as u32,
        'A'..='F' => 10 + c as u32 - 'A' as u32,
        'a'..='f' => 10 + c as u32 - 'a' as u32,
        _ => panic!(),
    }
}
