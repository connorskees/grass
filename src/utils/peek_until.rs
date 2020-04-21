use std::iter::Iterator;

use peekmore::PeekMoreIterator;

use crate::Token;

use super::IsWhitespace;

pub(crate) fn peek_until_closing_curly_brace<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
) -> Vec<Token> {
    let mut t = Vec::new();
    let mut nesting = 0;
    while let Some(tok) = toks.peek() {
        match tok.kind {
            q @ '"' | q @ '\'' => {
                t.push(*toks.peek().unwrap());
                toks.move_forward(1);
                t.extend(peek_until_closing_quote(toks, q));
            }
            '{' => {
                nesting += 1;
                t.push(*toks.peek().unwrap());
                toks.move_forward(1);
            }
            '}' => {
                if nesting == 0 {
                    break;
                } else {
                    nesting -= 1;
                    t.push(*toks.peek().unwrap());
                    toks.move_forward(1);
                }
            }
            '/' => {
                let next = *toks.peek_forward(1).unwrap();
                match toks.peek().unwrap().kind {
                    '/' => peek_until_newline(toks),
                    _ => t.push(next),
                };
                continue;
            }
            _ => {
                t.push(*toks.peek().unwrap());
                toks.move_forward(1);
            }
        }
    }
    peek_whitespace(toks);
    t
}

fn peek_until_closing_quote<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    q: char,
) -> Vec<Token> {
    let mut t = Vec::new();
    while let Some(tok) = toks.peek() {
        match tok.kind {
            '"' if q == '"' => {
                t.push(*tok);
                toks.move_forward(1);
                break;
            }
            '\'' if q == '\'' => {
                t.push(*tok);
                toks.move_forward(1);
                break;
            }
            '\\' => {
                t.push(*tok);
                t.push(*toks.peek_forward(1).unwrap());
            }
            '#' => {
                t.push(*tok);
                let next = toks.peek().unwrap();
                if next.kind == '{' {
                    t.push(*toks.peek_forward(1).unwrap());
                    t.append(&mut peek_until_closing_curly_brace(toks));
                }
            }
            _ => t.push(*tok),
        }
        toks.move_forward(1);
    }
    t
}

fn peek_until_newline<I: Iterator<Item = Token>>(toks: &mut PeekMoreIterator<I>) {
    while let Some(tok) = toks.peek() {
        if tok.kind == '\n' {
            break;
        }
        toks.move_forward(1);
    }
}

fn peek_whitespace<I: Iterator<Item = W>, W: IsWhitespace>(s: &mut PeekMoreIterator<I>) -> bool {
    let mut found_whitespace = false;
    while let Some(w) = s.peek() {
        if !w.is_whitespace() {
            break;
        }
        found_whitespace = true;
        s.move_forward(1);
    }
    found_whitespace
}
