use std::iter::Iterator;

use codemap::{Span, Spanned};

use peekmore::{PeekMore, PeekMoreIterator};

use num_traits::cast::ToPrimitive;

use super::parse::ruleset_eval;
use super::AtRule;

use crate::error::SassResult;
use crate::scope::Scope;
use crate::selector::Selector;
use crate::unit::Unit;
use crate::utils::{
    devour_whitespace, eat_ident, peek_ident_no_interpolation, read_until_closing_curly_brace,
    read_until_open_curly_brace,
};
use crate::value::{Number, Value};
use crate::{Stmt, Token};

#[derive(Debug, Clone)]
pub(crate) struct For {
    pub var: Spanned<String>,
    // TODO: optimization: this could be a generic or &dyn Iterator maybe?
    pub iter: Vec<usize>,
    pub body: Vec<Token>,
}

impl For {
    pub fn ruleset_eval(
        self,
        scope: &mut Scope,
        super_selector: &Selector,
        content: Option<&[Spanned<Stmt>]>,
    ) -> SassResult<Vec<Spanned<Stmt>>> {
        let mut stmts = Vec::new();
        for i in self.iter {
            scope.insert_var(
                &self.var.node,
                Spanned {
                    node: Value::Dimension(Number::from(i), Unit::None),
                    span: self.var.span,
                },
            )?;
            ruleset_eval(
                &mut self.body.clone().into_iter().peekmore(),
                scope,
                super_selector,
                false,
                content,
                &mut stmts,
            )?;
        }
        Ok(stmts)
    }

    pub fn iter(&self) -> std::slice::Iter<'_, usize> {
        self.iter.iter()
    }
}

pub(crate) fn parse_for<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    scope: &mut Scope,
    super_selector: &Selector,
    span: Span,
) -> SassResult<AtRule> {
    devour_whitespace(toks);
    let var = match toks.next().ok_or(("expected \"$\".", span))?.kind {
        '$' => eat_ident(toks, scope, super_selector)?,
        _ => return Err(("expected \"$\".", span).into()),
    };
    devour_whitespace(toks);
    if toks.peek().is_none()
        || eat_ident(toks, scope, super_selector)?.to_ascii_lowercase() != "from"
    {
        return Err(("Expected \"from\".", var.span).into());
    }
    devour_whitespace(toks);
    let mut from_toks = Vec::new();
    let mut through = 0;
    while let Some(tok) = toks.peek() {
        match tok.kind {
            't' | 'T' | '\\' => {
                let ident = peek_ident_no_interpolation(toks, false)?;
                match ident.node.to_ascii_lowercase().as_str() {
                    "through" => {
                        through = 1;
                        // todo: it should take more if there were escapes
                        toks.take(7).for_each(drop);
                        break;
                    }
                    "to" => {
                        // todo: it should take more if there were escapes
                        toks.take(2).for_each(drop);
                        break;
                    }
                    _ => {
                        return Err(("Invalid flag name.", ident.span).into());
                    }
                }
            }
            '{' => {
                return Err(("Expected \"to\" or \"through\".", tok.pos()).into());
            }
            _ => from_toks.push(toks.next().unwrap()),
        }
    }
    devour_whitespace(toks);
    let from_val = Value::from_vec(from_toks, scope, super_selector)?;
    let from = match from_val.node {
        Value::Dimension(n, _) => match n.to_integer().to_usize() {
            Some(v) => v,
            None => return Err((format!("{} is not a int.", n), from_val.span).into()),
        },
        v => {
            return Err((
                format!("{} is not an integer.", v.inspect(from_val.span)?),
                from_val.span,
            )
                .into())
        }
    };

    let to_toks = read_until_open_curly_brace(toks);
    toks.next();
    let to_val = Value::from_vec(to_toks, scope, super_selector)?;
    let to = match to_val.node {
        Value::Dimension(n, _) => match n.to_integer().to_usize() {
            Some(v) => v,
            None => return Err((format!("{} is not a int.", n), to_val.span).into()),
        },
        v => {
            return Err((
                format!("{} is not an integer.", v.to_css_string(to_val.span)?),
                to_val.span,
            )
                .into())
        }
    };
    let body = read_until_closing_curly_brace(toks);
    toks.next();

    devour_whitespace(toks);

    let iter = if from < to {
        (from..(to + through)).collect()
    } else {
        ((to - through)..(from + 1)).skip(1).rev().collect()
    };

    Ok(AtRule::For(For { iter, body, var }))
}
