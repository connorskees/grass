use codemap::{Span, Spanned};

use peekmore::{PeekMore, PeekMoreIterator};

use super::{eat_stmts, AtRule};

use crate::error::SassResult;
use crate::scope::Scope;
use crate::selector::Selector;
use crate::utils::{
    devour_whitespace, read_until_closing_curly_brace, read_until_open_curly_brace,
};
use crate::value::Value;
use crate::{Stmt, Token};

#[derive(Debug, Clone)]
pub(crate) struct While {
    pub cond: Vec<Token>,
    pub body: Vec<Token>,
}

impl While {
    pub fn ruleset_eval(
        self,
        scope: &mut Scope,
        super_selector: &Selector,
    ) -> SassResult<Vec<Spanned<Stmt>>> {
        let mut stmts = Vec::new();
        let mut val = Value::from_vec(self.cond.clone(), scope, super_selector)?;
        let scope = &mut scope.clone();
        while val.node.is_true(val.span)? {
            stmts.extend(eat_stmts(
                &mut self.body.clone().into_iter().peekmore(),
                scope,
                super_selector,
            )?);
            val = Value::from_vec(self.cond.clone(), scope, super_selector)?;
        }
        Ok(stmts)
    }
}

pub(crate) fn parse_while<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    span: Span,
) -> SassResult<Spanned<AtRule>> {
    devour_whitespace(toks);
    let cond = read_until_open_curly_brace(toks);

    if cond.is_empty() {
        return Err(("Expected expression.", span).into());
    }

    toks.next();

    let body = read_until_closing_curly_brace(toks);

    toks.next();

    devour_whitespace(toks);
    Ok(Spanned {
        node: AtRule::While(While { cond, body }),
        span,
    })
}
