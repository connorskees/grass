use std::iter::Iterator;

use codemap::{Span, Spanned};

use peekmore::{PeekMore, PeekMoreIterator};

use num_traits::cast::ToPrimitive;

use super::parse::eat_stmts;
use super::AtRule;

use crate::error::SassResult;
use crate::scope::Scope;
use crate::selector::Selector;
use crate::unit::Unit;
use crate::utils::{
    devour_whitespace, eat_ident, read_until_closing_curly_brace, read_until_open_curly_brace,
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
            stmts.extend(eat_stmts(
                &mut self.body.clone().into_iter().peekmore(),
                scope,
                super_selector,
            )?);
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
    while let Some(tok) = toks.next() {
        let mut these_toks = vec![tok];
        match these_toks[0].kind.to_ascii_lowercase() {
            't' => {
                these_toks.push(toks.next().unwrap());
                match these_toks[1].kind.to_ascii_lowercase() {
                    'h' => {
                        let r = toks.next().unwrap();
                        these_toks.push(r);
                        if r.kind != 'r' {
                            from_toks.extend(these_toks);
                            continue;
                        }
                        let o = toks.next().unwrap();
                        these_toks.push(o);
                        if o.kind != 'o' {
                            from_toks.extend(these_toks);
                            continue;
                        }
                        let u = toks.next().unwrap();
                        these_toks.push(u);
                        if u.kind != 'u' {
                            from_toks.extend(these_toks);
                            continue;
                        }
                        let g = toks.next().unwrap();
                        these_toks.push(g);
                        if g.kind != 'g' {
                            from_toks.extend(these_toks);
                            continue;
                        }
                        let h = toks.next().unwrap();
                        these_toks.push(h);
                        if h.kind != 'h' {
                            from_toks.extend(these_toks);
                            continue;
                        }
                        let peek = toks.peek().unwrap().kind;
                        if peek.is_alphanumeric() || peek == '\\' {
                            from_toks.extend(these_toks);
                            continue;
                        }
                        through = 1;
                        break;
                    }
                    'o' => {
                        if toks.peek().unwrap().kind.is_whitespace() {
                            break;
                        } else {
                            from_toks.extend(these_toks);
                        }
                    }
                    _ => {
                        from_toks.extend(these_toks);
                    }
                }
            }
            '{' => {
                return Err(("Expected \"to\" or \"through\".", tok.pos()).into());
            }
            _ => from_toks.extend(these_toks),
        }
    }
    let from_val = Value::from_vec(from_toks, scope, super_selector)?;
    let from = match from_val.node {
        Value::Dimension(n, _) => match n.to_integer().to_usize() {
            Some(v) => v,
            None => return Err((format!("{} is not a int.", n), from_val.span).into()),
        },
        v => {
            return Err((
                format!("{} is not an integer.", v.to_css_string(from_val.span)?),
                from_val.span,
            )
                .into())
        }
    };
    devour_whitespace(toks);
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
