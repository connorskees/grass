use std::iter::Iterator;

use codemap::Spanned;

use peekmore::{PeekMore, PeekMoreIterator};

use crate::error::SassResult;
use crate::selector::Selector;
use crate::value::Value;
use crate::{Scope, Token};

use super::{devour_whitespace, eat_ident, read_until_semicolon_or_closing_curly_brace};

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
) -> SassResult<VariableDecl> {
    devour_whitespace(toks);
    let mut default = false;
    let mut global = false;
    let mut raw = read_until_semicolon_or_closing_curly_brace(toks)
        .into_iter()
        .peekmore();
    if toks.peek().is_some() && toks.peek().unwrap().kind == ';' {
        toks.next();
    }
    let mut val_toks = Vec::new();
    while let Some(tok) = raw.next() {
        match tok.kind {
            '!' => {
                let next = raw.next().unwrap();
                match next.kind {
                    'i' => todo!("!important"),
                    'g' => {
                        let s = eat_ident(&mut raw, scope, super_selector)?;
                        if s.node.to_ascii_lowercase().as_str() == "lobal" {
                            global = true;
                        } else {
                            return Err(("Invalid flag name.", s.span).into());
                        }
                    }
                    'd' => {
                        let s = eat_ident(&mut raw, scope, super_selector)?;
                        if s.to_ascii_lowercase().as_str() == "efault" {
                            default = true;
                        } else {
                            return Err(("Invalid flag name.", s.span).into());
                        }
                    }
                    _ => return Err(("Invalid flag name.", next.pos()).into()),
                }
            }
            _ => val_toks.push(tok),
        }
    }
    devour_whitespace(toks);

    let val = Value::from_vec(val_toks, scope, super_selector)?;
    Ok(VariableDecl::new(val, default, global))
}
