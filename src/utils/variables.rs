use std::iter::Iterator;

use codemap::{Span, Spanned};

use peekmore::PeekMoreIterator;

use crate::error::SassResult;
use crate::selector::Selector;
use crate::value::Value;
use crate::{Scope, Token};

use super::{
    devour_whitespace, peek_ident_no_interpolation, read_until_closing_paren,
    read_until_closing_quote, read_until_newline,
};

pub(crate) struct VariableDecl {
    pub val: Spanned<Value>,
    pub default: bool,
    pub global: bool,
}

impl VariableDecl {
    pub const fn new(val: Spanned<Value>, default: bool, global: bool) -> VariableDecl {
        VariableDecl {
            val,
            default,
            global,
        }
    }
}

pub(crate) fn eat_variable_value<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    scope: &Scope,
    super_selector: &Selector,
    span_before: Span,
) -> SassResult<VariableDecl> {
    devour_whitespace(toks);
    let mut default = false;
    let mut global = false;

    let mut val_toks = Vec::new();
    let mut nesting = 0;
    while let Some(tok) = toks.peek() {
        match tok.kind {
            ';' => {
                toks.next();
                break;
            }
            '\\' => {
                val_toks.push(toks.next().unwrap());
                if toks.peek().is_some() {
                    val_toks.push(toks.next().unwrap());
                }
            }
            '"' | '\'' => {
                let quote = toks.next().unwrap();
                val_toks.push(quote);
                val_toks.extend(read_until_closing_quote(toks, quote.kind)?);
            }
            '#' => {
                val_toks.push(toks.next().unwrap());
                match toks.peek().unwrap().kind {
                    '{' => nesting += 1,
                    ';' => break,
                    '}' => {
                        if nesting == 0 {
                            break;
                        } else {
                            nesting -= 1;
                        }
                    }
                    _ => {}
                }
                val_toks.push(toks.next().unwrap());
            }
            '{' => break,
            '}' => {
                if nesting == 0 {
                    break;
                } else {
                    nesting -= 1;
                    val_toks.push(toks.next().unwrap());
                }
            }
            '/' => {
                let next = toks.next().unwrap();
                match toks.peek().unwrap().kind {
                    '/' => read_until_newline(toks),
                    _ => val_toks.push(next),
                };
                continue;
            }
            '(' => {
                val_toks.push(toks.next().unwrap());
                val_toks.extend(read_until_closing_paren(toks)?);
            }
            '!' => {
                let pos = tok.pos();
                if toks.peek_forward(1).is_none() {
                    return Err(("Expected identifier.", pos).into());
                }
                // todo: it should not be possible to declare the same flag more than once
                let mut ident = peek_ident_no_interpolation(toks, false)?;
                ident.node.make_ascii_lowercase();
                match ident.node.as_str() {
                    "global" => {
                        toks.take(7).for_each(drop);
                        global = true;
                    }
                    "default" => {
                        toks.take(8).for_each(drop);
                        default = true;
                    }
                    "important" => {
                        toks.reset_view();
                        val_toks.push(toks.next().unwrap());
                        continue;
                    }
                    _ => {
                        return Err(("Invalid flag name.", ident.span).into());
                    }
                }
            }
            _ => val_toks.push(toks.next().unwrap()),
        }
    }
    devour_whitespace(toks);
    let val = Value::from_vec(val_toks, scope, super_selector, span_before)?;
    Ok(VariableDecl::new(val, default, global))
}
