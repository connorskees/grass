use std::iter::Peekable;

use crate::error::SassResult;
use crate::scope::Scope;
use crate::selector::Selector;
use crate::utils::{devour_whitespace, parse_interpolation};
use crate::Token;

pub(crate) fn eat_calc_args<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<String> {
    let mut string = String::from("(");
    let mut nesting = 0;
    while let Some(tok) = toks.next() {
        match tok.kind {
            ' ' | '\t' | '\n' => {
                devour_whitespace(toks);
                string.push(' ');
            }
            '#' => {
                if toks.peek().is_some() && toks.peek().unwrap().kind == '{' {
                    toks.next();
                    string.push_str(&parse_interpolation(toks, scope, super_selector)?.to_string());
                } else {
                    string.push('#');
                }
            }
            '(' => {
                nesting += 1;
                string.push('(');
            }
            ')' => {
                if nesting == 0 {
                    break;
                } else {
                    nesting -= 1;
                    string.push(')');
                }
            }
            c => string.push(c),
        }
    }
    string.push(')');
    Ok(string)
}

#[allow(dead_code)]
pub(crate) fn is_special_function(s: &str) -> bool {
    s.starts_with("calc(")
        || s.starts_with("var(")
        || s.starts_with("env(")
        || s.starts_with("min(")
        || s.starts_with("max(")
}

pub(crate) fn eat_progid<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<String> {
    let mut string = String::new();
    while let Some(tok) = toks.next() {
        match tok.kind {
            'a'..='z' | 'A'..='Z' | '.' => {
                string.push(tok.kind);
            }
            '(' => {
                string.push_str(&eat_calc_args(toks, scope, super_selector)?);
                break;
            }
            _ => return Err("expected \"(\".".into()),
        }
    }
    Ok(string)
}
