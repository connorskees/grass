use std::iter::Peekable;

use super::{eat_stmts, AtRule};

use crate::error::SassResult;
use crate::scope::Scope;
use crate::selector::Selector;
use crate::utils::{
    devour_whitespace, eat_ident, read_until_closing_curly_brace, read_until_open_curly_brace,
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
    pub cond: Vec<Token>,
    pub toks: Vec<Token>,
}

impl Branch {
    pub fn new(cond: Vec<Token>, toks: Vec<Token>) -> Branch {
        Branch { cond, toks }
    }
}

impl If {
    pub fn from_tokens<I: Iterator<Item = Token>>(toks: &mut Peekable<I>) -> SassResult<If> {
        let mut branches = Vec::new();
        let init_cond = read_until_open_curly_brace(toks);
        toks.next();
        devour_whitespace(toks);
        let mut init_toks = read_until_closing_curly_brace(toks);
        init_toks.push(toks.next().unwrap());
        devour_whitespace(toks);

        branches.push(Branch::new(init_cond, init_toks));

        let mut else_ = Vec::new();

        loop {
            if toks.peek().is_some() {
                if toks.peek().unwrap().kind == '@' {
                    toks.next();
                } else {
                    break;
                }
                if eat_ident(toks, &Scope::new(), &Selector::new())?.to_ascii_lowercase() == "else"
                {
                    devour_whitespace(toks);
                    if let Some(tok) = toks.next() {
                        devour_whitespace(toks);
                        match tok.kind.to_ascii_lowercase() {
                            'i' if toks.next().unwrap().kind.to_ascii_lowercase() == 'f' => {
                                toks.next();
                                let cond = read_until_open_curly_brace(toks);
                                toks.next();
                                devour_whitespace(toks);
                                let toks_ = read_until_closing_curly_brace(toks);
                                toks.next();
                                devour_whitespace(toks);
                                branches.push(Branch::new(cond, toks_))
                            }
                            '{' => {
                                else_ = read_until_closing_curly_brace(toks);
                                toks.next();
                                break;
                            }
                            _ => {
                                return Err("expected \"{\".".into());
                            }
                        }
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        devour_whitespace(toks);

        Ok(If { branches, else_ })
    }

    pub fn eval(self, scope: &mut Scope, super_selector: &Selector) -> SassResult<Vec<Stmt>> {
        let mut stmts = Vec::new();
        let mut toks = Vec::new();
        let mut found_true = false;
        for branch in self.branches {
            if Value::from_tokens(
                &mut branch.cond.into_iter().peekable(),
                scope,
                super_selector,
            )?
            .is_true()?
            {
                toks = branch.toks;
                found_true = true;
                break;
            }
        }
        if !found_true {
            toks = self.else_;
        }
        for stmt in eat_stmts(&mut toks.into_iter().peekable(), scope, super_selector)? {
            match stmt {
                Stmt::AtRule(AtRule::If(i)) => stmts.extend(i.eval(scope, super_selector)?),
                Stmt::RuleSet(r) if r.selector.is_empty() => stmts.extend(r.rules),
                v => stmts.push(v),
            }
        }
        Ok(stmts)
    }
}
