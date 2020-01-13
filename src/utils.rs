use std::iter::Iterator;
use std::iter::Peekable;

pub trait IsWhitespace {
    fn is_whitespace(&self) -> bool;
}

pub fn devour_whitespace<I: Iterator<Item = W>, W: IsWhitespace>(s: &mut Peekable<I>) -> bool {
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
