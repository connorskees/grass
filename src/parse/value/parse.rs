use std::iter::Iterator;

use super::super::Parser;

impl<'a, 'b> Parser<'a, 'b> {
    pub(crate) fn parse_whole_number(&mut self) -> String {
        let mut buf = String::new();

        while let Some(c) = self.toks.peek() {
            if !c.kind.is_ascii_digit() {
                break;
            }

            let tok = self.toks.next().unwrap();
            buf.push(tok.kind);
        }

        buf
    }
}
