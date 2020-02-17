use std::iter::Peekable;

use crate::args::{eat_func_args, CallArgs, FuncArgs};
use crate::atrule::AtRule;
use crate::common::{Scope, Symbol};
use crate::error::SassResult;
use crate::utils::devour_whitespace;
use crate::value::Value;
use crate::{Token, TokenKind};

#[derive(Debug, Clone)]
pub(crate) struct Function {
    scope: Scope,
    args: FuncArgs,
    body: Vec<AtRule>,
}

impl Function {
    pub fn new(scope: Scope, args: FuncArgs, body: Vec<AtRule>) -> Self {
        Function { scope, args, body }
    }

    pub fn decl_from_tokens<I: Iterator<Item = Token>>(
        toks: &mut Peekable<I>,
        scope: &Scope,
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
            }) => eat_func_args(toks, scope),
            _ => return Err("expected \"(\".".into()),
        };

        let mut nesting = 1;
        let mut body: Vec<AtRule> = Vec::new();

        while nesting > 0 {
            if let Some(tok) = toks.next() {
                match &tok.kind {
                    TokenKind::AtRule(rule) => {
                        body.push(AtRule::from_tokens(rule, tok.pos, toks, scope)?)
                    }
                    TokenKind::Symbol(Symbol::CloseCurlyBrace) => nesting -= 1,
                    _ => {}
                }
            } else {
                return Err("unexpected EOF (TODO: better error message)".into());
            }
        }

        Ok((name, Function::new(scope.clone(), args, body)))
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
            self.scope.insert_var(&arg.name, val);
        }
        Ok(self)
    }

    pub fn call(&self) -> Value {
        for rule in &self.body {
            match rule {
                AtRule::Return(toks) => {
                    return Value::from_tokens(
                        &mut toks.clone().into_iter().peekable(),
                        &self.scope,
                    )
                    .unwrap()
                }
                _ => todo!("unimplemented at rule in function body"),
            }
        }
        todo!()
    }
}
