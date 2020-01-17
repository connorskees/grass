use crate::common::Scope;
use crate::selector::Selector;
use crate::{eat_expr, Expr, RuleSet, Stmt, Token};
use std::iter::Peekable;
use std::vec::IntoIter;

#[derive(Debug, Clone)]
pub struct Mixin {
    scope: Scope,
    args: FuncArgs,
    body: Peekable<IntoIter<Token>>,
    nesting: u32,
}

impl Mixin {
    pub fn new(scope: Scope, args: FuncArgs, body: Vec<Token>) -> Self {
        let body = body.clone().into_iter().peekable();
        Mixin {
            scope,
            args,
            body,
            nesting: 0,
        }
    }

    pub fn call_with_args(&mut self, args: &CallArgs) -> &mut Mixin {
        for (idx, arg) in args.0.iter().enumerate() {
            if arg.is_named() {
                todo!("keyword args")
            } else {
                // dbg!(&self.args.0[idx].name.clone());
                self.scope.vars.insert(self.args.0[idx].name.clone(), arg.val.clone());
            }
        }
        self
    }

    pub fn eval(&mut self, super_selector: &Selector, scope: &mut Scope) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        // dbg!(&scope);
        while let Ok(expr) = eat_expr(&mut self.body, scope, super_selector) {
            match expr {
                Expr::Style(s) => stmts.push(Stmt::Style(s)),
                Expr::Include(_) => todo!(),
                Expr::MixinDecl(_, _) => todo!(),
                Expr::Selector(s) => {
                    self.nesting += 1;
                    let rules = self.eval(&super_selector.clone().zip(s.clone()), scope);
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
        stmts
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FuncArgs(pub Vec<FuncArg>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FuncArg {
    pub name: String,
    pub default: Option<Vec<Token>>,
}

impl FuncArgs {
    pub const fn new() -> Self {
        FuncArgs(Vec::new())
    }
}

#[derive(Debug, Clone)]
pub struct CallArgs(pub Vec<CallArg>);

#[derive(Debug, Clone)]
pub struct CallArg {
    pub name: Option<String>,
    pub val: Vec<Token>,
}

impl CallArg {
    pub fn is_named(&self) -> bool {
        self.name.is_some()
    }
}

impl CallArgs {
    pub const fn new() -> Self {
        CallArgs(Vec::new())
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}