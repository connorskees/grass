use std::iter::Peekable;

use super::eat_stmts;

use crate::args::{eat_func_args, CallArgs, FuncArgs};
use crate::atrule::AtRule;
use crate::common::Symbol;
use crate::error::SassResult;
use crate::scope::Scope;
use crate::selector::Selector;
use crate::utils::devour_whitespace;
use crate::value::Value;
use crate::{Stmt, Token, TokenKind};

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
        let Token { kind, .. } = toks
            .next()
            .expect("this must exist because we have already peeked");
        devour_whitespace(toks);
        let name = match kind {
            TokenKind::Ident(s) => s,
            _ => return Err("Expected identifier.".into()),
        };
        devour_whitespace(toks);
        let args = match toks.next() {
            Some(Token {
                kind: TokenKind::Symbol(Symbol::OpenParen),
                ..
            }) => eat_func_args(toks, &scope, super_selector)?,
            _ => return Err("expected \"(\".".into()),
        };

        let body = eat_stmts(toks, &mut scope.clone(), super_selector)?;
        devour_whitespace(toks);

        Ok((name, Function::new(scope, args, body)))
    }

    pub fn args(mut self, args: &mut CallArgs) -> SassResult<Function> {
        for (idx, arg) in self.args.0.iter().enumerate() {
            let val = match args.remove(&format!("{}", idx)) {
                Some(v) => v,
                None => match args.remove(&arg.name) {
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
                        &mut toks.clone().into_iter().peekable(),
                        &self.scope,
                        super_selector,
                    )
                }
                Stmt::AtRule(AtRule::For(..)) => todo!("@for in function"),
                Stmt::AtRule(AtRule::If(i)) => {
                    match self.call(
                        super_selector,
                        i.eval(&mut self.scope.clone(), super_selector)?,
                    ) {
                        Ok(v) => return Ok(v),
                        Err(..) => {}
                    }
                }
                _ => return Err("This at-rule is not allowed here.".into()),
            }
        }
        Err("Function finished without @return.".into())
    }
}
