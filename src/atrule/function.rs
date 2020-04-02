use std::iter::Peekable;

use super::eat_stmts;

use crate::args::{eat_func_args, CallArgs, FuncArgs};
use crate::atrule::AtRule;
use crate::error::SassResult;
use crate::scope::Scope;
use crate::selector::Selector;
use crate::utils::{devour_whitespace, eat_ident};
use crate::value::Value;
use crate::{Stmt, Token};

#[derive(Debug, Clone)]
pub(crate) struct Function {
    scope: Scope,
    args: FuncArgs,
    body: Vec<Stmt>,
}

impl Function {
    pub fn new(scope: Scope, args: FuncArgs, body: Vec<Stmt>) -> Self {
        Function { scope, args, body }
    }

    pub fn decl_from_tokens<I: Iterator<Item = Token>>(
        toks: &mut Peekable<I>,
        scope: Scope,
        super_selector: &Selector,
    ) -> SassResult<(String, Function)> {
        let name = eat_ident(toks, &scope, super_selector)?;
        devour_whitespace(toks);
        let args = match toks.next() {
            Some(Token { kind: '(', .. }) => eat_func_args(toks, &scope, super_selector)?,
            _ => return Err("expected \"(\".".into()),
        };

        devour_whitespace(toks);

        let body = eat_stmts(toks, &mut scope.clone(), super_selector)?;
        devour_whitespace(toks);

        Ok((name, Function::new(scope, args, body)))
    }

    pub fn args(mut self, args: &mut CallArgs) -> SassResult<Function> {
        for (idx, arg) in self.args.0.iter().enumerate() {
            let val = match args.remove_positional(idx) {
                Some(v) => v,
                None => match args.remove_named(arg.name.clone()) {
                    Some(v) => v,
                    None => match &arg.default {
                        Some(v) => v.clone(),
                        None => return Err(format!("Missing argument ${}.", &arg.name).into()),
                    },
                },
            };
            self.scope.insert_var(&arg.name, val)?;
        }
        Ok(self)
    }

    pub fn body(&self) -> Vec<Stmt> {
        self.body.clone()
    }

    pub fn call(&self, super_selector: &Selector, stmts: Vec<Stmt>) -> SassResult<Value> {
        for stmt in stmts {
            match stmt {
                Stmt::AtRule(AtRule::Return(toks)) => {
                    return Value::from_tokens(
                        &mut toks.into_iter().peekable(),
                        &self.scope,
                        super_selector,
                    )
                }
                Stmt::AtRule(AtRule::For(..)) => todo!("@for in function"),
                Stmt::AtRule(AtRule::If(i)) => {
                    if let Ok(v) = self.call(
                        super_selector,
                        i.eval(&mut self.scope.clone(), super_selector)?,
                    ) {
                        return Ok(v);
                    }
                }
                _ => return Err("This at-rule is not allowed here.".into()),
            }
        }
        Err("Function finished without @return.".into())
    }
}
