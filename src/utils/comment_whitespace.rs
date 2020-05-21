use std::iter::Iterator;

use codemap::Spanned;

use peekmore::PeekMoreIterator;

use crate::error::SassResult;
use crate::selector::Selector;
use crate::{Scope, Token};

use super::parse_interpolation;

pub(crate) trait IsWhitespace {
    fn is_whitespace(&self) -> bool;
}

impl IsWhitespace for char {
    fn is_whitespace(&self) -> bool {
        self.is_ascii_whitespace()
    }
}

pub(crate) fn devour_whitespace<I: Iterator<Item = W>, W: IsWhitespace>(
    s: &mut PeekMoreIterator<I>,
) -> bool {
    let mut found_whitespace = false;
    while let Some(w) = s.peek() {
        if !w.is_whitespace() {
            break;
        }
        found_whitespace = true;
        s.next();
    }
    found_whitespace
}

pub(crate) fn peek_whitespace<I: Iterator<Item = W>, W: IsWhitespace>(
    s: &mut PeekMoreIterator<I>,
) -> usize {
    let mut peek_counter = 0;
    while let Some(w) = s.peek() {
        if !w.is_whitespace() {
            break;
        }
        peek_counter += 1;
        s.peek_forward(1);
    }
    peek_counter
}

pub(crate) fn devour_whitespace_or_comment<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
) -> SassResult<bool> {
    let mut found_whitespace = false;
    while let Some(tok) = toks.peek() {
        if tok.kind == '/' {
            let next = match toks.peek_forward(1) {
                Some(v) => v,
                None => return Ok(found_whitespace),
            };
            match next.kind {
                '*' => {
                    toks.next();
                    eat_comment(toks, &Scope::new(), &Selector::new())?;
                }
                '/' => read_until_newline(toks),
                _ => {
                    toks.reset_view();
                    return Ok(found_whitespace);
                }
            };
            found_whitespace = true;
            continue;
        }
        if !tok.is_whitespace() {
            break;
        }
        found_whitespace = true;
        toks.next();
    }
    Ok(found_whitespace)
}

/// Eat and return the contents of a comment.
///
/// This function assumes that the starting "/*" has already been consumed
/// The entirety of the comment, including the ending "*/" is consumed.
/// Note that the ending "*/" is not included in the output.
pub(crate) fn eat_comment<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<Spanned<String>> {
    let mut comment = String::new();
    let mut span = if let Some(tok) = toks.peek() {
        tok.pos()
    } else {
        // Hit this path: "/*"
        todo!("")
    };
    while let Some(tok) = toks.next() {
        span = span.merge(tok.pos());
        if tok.kind == '*' && toks.peek().unwrap().kind == '/' {
            toks.next();
            break;
        } else if tok.kind == '#' && toks.peek().unwrap().kind == '{' {
            toks.next();
            comment
                .push_str(&parse_interpolation(toks, scope, super_selector)?.to_css_string(span)?);
            continue;
        }
        comment.push(tok.kind);
    }
    devour_whitespace(toks);
    Ok(Spanned {
        node: comment,
        span,
    })
}

/// Eat tokens until a newline
///
/// This exists largely to eat silent comments, "//"
/// We only have to check for \n as the lexing step normalizes all newline characters
///
/// The newline is consumed
pub(crate) fn read_until_newline<I: Iterator<Item = Token>>(toks: &mut PeekMoreIterator<I>) {
    for tok in toks {
        if tok.kind == '\n' {
            break;
        }
    }
}
