use codemap::{Span, Spanned};

use peekmore::{PeekMore, PeekMoreIterator};

use super::ruleset_eval;

use crate::error::SassResult;
use crate::scope::Scope;
use crate::selector::Selector;
use crate::utils::{
    devour_whitespace, devour_whitespace_or_comment, peek_ident_no_interpolation,
    read_until_closing_curly_brace, read_until_open_curly_brace,
};
use crate::value::Value;
use crate::{Stmt, Token};

#[derive(Debug, Clone)]
pub(crate) struct If {
    pub branches: Vec<Branch>,
    pub else_: Vec<Token>,
}

#[derive(Debug, Clone)]
pub(crate) struct Branch {
    pub cond: Spanned<Value>,
    pub toks: Vec<Token>,
}

impl Branch {
    pub fn new(cond: Spanned<Value>, toks: Vec<Token>) -> Branch {
        Branch { cond, toks }
    }
}

impl If {
    pub fn from_tokens<I: Iterator<Item = Token>>(
        toks: &mut PeekMoreIterator<I>,
        scope: &Scope,
        super_selector: &Selector,
        span_before: Span,
    ) -> SassResult<If> {
        devour_whitespace_or_comment(toks)?;
        let mut branches = Vec::new();
        let init_cond_toks = read_until_open_curly_brace(toks)?;
        if init_cond_toks.is_empty() {
            return Err(("Expected expression.", span_before).into());
        }
        let span_before = match toks.next() {
            Some(t) => t.pos,
            None => return Err(("Expected expression.", span_before).into()),
        };
        let init_cond = Value::from_vec(init_cond_toks, scope, super_selector, span_before)?;
        devour_whitespace_or_comment(toks)?;
        let mut init_toks = read_until_closing_curly_brace(toks)?;
        if let Some(tok) = toks.next() {
            init_toks.push(tok);
        } else {
            return Err(("expected \"}\".", span_before).into());
        }
        devour_whitespace(toks);

        branches.push(Branch::new(init_cond, init_toks));

        let mut else_ = Vec::new();

        loop {
            if let Some(Token { kind: '@', pos }) = toks.peek().cloned() {
                toks.peek_forward(1);
                let ident = peek_ident_no_interpolation(toks, false, pos)?;
                if ident.as_str() != "else" {
                    toks.reset_view();
                    break;
                }
                toks.take(4).for_each(drop);
            } else {
                break;
            }
            devour_whitespace(toks);
            if let Some(tok) = toks.next() {
                devour_whitespace(toks);
                match tok.kind.to_ascii_lowercase() {
                    'i' if toks.next().unwrap().kind.to_ascii_lowercase() == 'f' => {
                        let pos = toks.next().unwrap().pos;
                        let cond = Value::from_vec(
                            read_until_open_curly_brace(toks)?,
                            scope,
                            super_selector,
                            pos,
                        )?;
                        toks.next();
                        devour_whitespace(toks);
                        branches.push(Branch::new(cond, read_until_closing_curly_brace(toks)?));
                        toks.next();
                        devour_whitespace(toks);
                    }
                    '{' => {
                        else_ = read_until_closing_curly_brace(toks)?;
                        toks.next();
                        break;
                    }
                    _ => {
                        return Err(("expected \"{\".", tok.pos()).into());
                    }
                }
            } else {
                break;
            }
        }
        devour_whitespace(toks);

        Ok(If { branches, else_ })
    }

    pub fn eval(
        self,
        scope: &mut Scope,
        super_selector: &Selector,
        content: Option<&[Spanned<Stmt>]>,
    ) -> SassResult<Vec<Spanned<Stmt>>> {
        let mut stmts = Vec::new();
        let mut toks = Vec::new();
        let mut found_true = false;
        for branch in self.branches {
            if branch.cond.node.is_true(branch.cond.span)? {
                toks = branch.toks;
                found_true = true;
                break;
            }
        }
        if !found_true {
            toks = self.else_;
        }
        ruleset_eval(
            &mut toks.into_iter().peekmore(),
            scope,
            super_selector,
            false,
            content,
            &mut stmts,
        )?;
        Ok(stmts)
    }
}
