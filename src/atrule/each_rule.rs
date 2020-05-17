use codemap::{Span, Spanned};

use peekmore::{PeekMore, PeekMoreIterator};

use crate::common::{Brackets, ListSeparator};
use crate::error::SassResult;
use crate::scope::Scope;
use crate::selector::Selector;
use crate::utils::{
    devour_whitespace, eat_ident, read_until_closing_curly_brace, read_until_open_curly_brace,
};
use crate::value::Value;
use crate::{Stmt, Token};

use super::{ruleset_eval, AtRule};

#[derive(Debug, Clone)]
pub(crate) struct Each {
    vars: Vec<Spanned<String>>,
    iter: Vec<Value>,
    body: Vec<Token>,
}

impl Each {
    pub fn ruleset_eval(
        self,
        scope: &mut Scope,
        super_selector: &Selector,
        content: Option<&[Spanned<Stmt>]>,
    ) -> SassResult<Vec<Spanned<Stmt>>> {
        let mut stmts = Vec::new();
        for row in self.iter {
            let this_iterator = match row {
                Value::List(v, ..) => v,
                Value::Map(m) => m
                    .into_iter()
                    .map(|(k, v)| Value::List(vec![k, v], ListSeparator::Space, Brackets::None))
                    .collect(),
                v => vec![v],
            };

            if self.vars.len() == 1 {
                if this_iterator.len() == 1 {
                    scope.insert_var(
                        &self.vars[0],
                        Spanned {
                            node: this_iterator[0].clone(),
                            span: self.vars[0].span,
                        },
                    )?;
                } else {
                    scope.insert_var(
                        &self.vars[0],
                        Spanned {
                            node: Value::List(this_iterator, ListSeparator::Space, Brackets::None),
                            span: self.vars[0].span,
                        },
                    )?;
                }
            } else {
                for (var, val) in self.vars.clone().into_iter().zip(
                    this_iterator
                        .into_iter()
                        .chain(std::iter::once(Value::Null).cycle()),
                ) {
                    scope.insert_var(
                        &var.node,
                        Spanned {
                            node: val,
                            span: var.span,
                        },
                    )?;
                }
            }

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
}

pub(crate) fn parse_each<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    scope: &mut Scope,
    super_selector: &Selector,
    mut span: Span,
) -> SassResult<AtRule> {
    devour_whitespace(toks);
    let mut vars = Vec::new();
    loop {
        let next = toks.next().ok_or(("expected \"$\".", span))?;
        span = next.pos();
        match next.kind {
            '$' => vars.push(eat_ident(toks, scope, super_selector, next.pos)?),
            _ => return Err(("expected \"$\".", next.pos()).into()),
        }
        devour_whitespace(toks);
        if toks
            .peek()
            .ok_or(("expected \"$\".", vars[vars.len() - 1].span))?
            .kind
            == ','
        {
            toks.next();
            devour_whitespace(toks);
        } else {
            break;
        }
    }
    let i = eat_ident(toks, scope, super_selector, span)?;
    if i.node.to_ascii_lowercase() != "in" {
        return Err(("Expected \"in\".", i.span).into());
    }
    devour_whitespace(toks);
    let iter_val = Value::from_vec(read_until_open_curly_brace(toks), scope, super_selector)?;
    let iter = match iter_val.node.eval(iter_val.span)?.node {
        Value::List(v, ..) => v,
        Value::Map(m) => m
            .into_iter()
            .map(|(k, v)| Value::List(vec![k, v], ListSeparator::Space, Brackets::None))
            .collect(),
        v => vec![v],
    };
    toks.next();
    devour_whitespace(toks);
    let mut body = read_until_closing_curly_brace(toks);
    body.push(toks.next().unwrap());
    devour_whitespace(toks);

    Ok(AtRule::Each(Each { vars, iter, body }))
}
