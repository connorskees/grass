use std::iter::Peekable;
use std::vec::IntoIter;

use crate::common::{Pos, Scope};
use crate::function::{CallArgs, FuncArgs};
use crate::selector::Selector;
use crate::{eat_expr, Expr, RuleSet, Stmt, Token};

#[derive(Debug, Clone)]
pub struct Mixin {
    scope: Scope,
    args: FuncArgs,
    body: Peekable<IntoIter<Token>>,
    nesting: u32,
}

impl Mixin {
    pub fn new(scope: Scope, args: FuncArgs, body: Vec<Token>) -> Self {
        let body = body.into_iter().peekable();
        Mixin {
            scope,
            args,
            body,
            nesting: 0,
        }
    }

    pub fn args(&mut self, args: &CallArgs) -> &mut Mixin {
        for (idx, arg) in args.0.iter().enumerate() {
            if arg.is_named() {
                todo!("keyword args")
            } else {
                self.scope.vars.insert(
                    self.args
                        .0
                        .get(idx)
                        .expect("too many args passed to mixin")
                        .name
                        .clone(),
                    arg.val.clone(),
                );
            }
        }
        self
    }

    pub fn call(&mut self, super_selector: &Selector) -> Result<Vec<Stmt>, (Pos, &'static str)> {
        self.eval(super_selector, &mut self.scope.clone())
    }

    pub fn eval(
        &mut self,
        super_selector: &Selector,
        scope: &mut Scope,
    ) -> Result<Vec<Stmt>, (Pos, &'static str)> {
        let mut stmts = Vec::new();
        while let Some(expr) = eat_expr(&mut self.body, scope, super_selector)? {
            match expr {
                Expr::Style(s) => stmts.push(Stmt::Style(s)),
                Expr::Include(_)
                | Expr::MixinDecl(_, _) => todo!(),
                Expr::Selector(s) => {
                    self.nesting += 1;
                    let rules = self.eval(&super_selector.clone().zip(s.clone()), scope)?;
                    stmts.push(Stmt::RuleSet(RuleSet {
                        super_selector: super_selector.clone(),
                        selector: s,
                        rules,
                    }));
                    self.nesting -= 1;
                }
                Expr::VariableDecl(name, val) => {
                    if self.nesting == 0 {
                        scope.vars.insert(name.clone(), val.clone());
                        self.scope.vars.insert(name, val);
                    } else {
                        scope.vars.insert(name, val);
                    }
                }
                Expr::MultilineComment(s) => stmts.push(Stmt::MultilineComment(s)),
            }
        }
        Ok(stmts)
    }
}
