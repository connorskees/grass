use std::iter::{Iterator, Rev, Skip};
use std::ops::Range;

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

pub(crate) enum ForIterator {
    Forward(Range<isize>),
    Backward(Rev<Skip<Range<isize>>>),
}

impl Iterator for ForIterator {
    type Item = isize;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Forward(i) => i.next(),
            Self::Backward(i) => i.next(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct For {
    pub var: Spanned<String>,
    from: isize,
    to: isize,
    through: isize,
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
        for i in self.iter() {
            scope.insert_var(
                &self.var.node,
                Spanned {
                    node: Value::Dimension(Number::from(i), Unit::None),
                    span: self.var.span,
                },
            )?;
            ruleset_eval(
                &mut self.body.iter().cloned().peekmore(),
                scope,
                super_selector,
                false,
                content,
                &mut stmts,
            )?;
        }
        Ok(stmts)
    }

    pub fn iter(&self) -> ForIterator {
        if self.from < self.to {
            ForIterator::Forward(self.from..(self.to + self.through))
        } else {
            ForIterator::Backward(((self.to - self.through)..(self.from + 1)).skip(1).rev())
        }
    }
}

pub(crate) fn parse_for<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    scope: &mut Scope,
    super_selector: &Selector,
    span: Span,
) -> SassResult<AtRule> {
    devour_whitespace(toks);
    let next = toks.next().ok_or(("expected \"$\".", span))?;
    let var = match next.kind {
        '$' => eat_ident(toks, scope, super_selector, next.pos)?,
        _ => return Err(("expected \"$\".", span).into()),
    };
    devour_whitespace(toks);
    if toks.peek().is_none() {
        return Err(("Expected \"from\".", var.span).into());
    }
    let span_before = toks.peek().unwrap().pos;
    if eat_ident(toks, scope, super_selector, span_before)?.to_ascii_lowercase() != "from" {
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
    let from_val = Value::from_vec(from_toks, scope, super_selector, span_before)?;
    let from = match from_val.node.eval(from_val.span)?.node {
        Value::Dimension(n, _) => match n.to_integer().to_isize() {
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

    let to_toks = read_until_open_curly_brace(toks)?;
    toks.next();
    let to_val = Value::from_vec(to_toks, scope, super_selector, from_val.span)?;
    let to = match to_val.node.eval(to_val.span)?.node {
        Value::Dimension(n, _) => match n.to_integer().to_isize() {
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
    let body = read_until_closing_curly_brace(toks)?;
    toks.next();

    devour_whitespace(toks);

    Ok(AtRule::For(For {
        from,
        to,
        through,
        body,
        var,
    }))
}
