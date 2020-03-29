use std::iter::Peekable;
use std::vec::IntoIter;

use super::eat_stmts;

use crate::args::{eat_call_args, eat_func_args, CallArgs, FuncArgs};
use crate::atrule::AtRule;
use crate::error::SassResult;
use crate::scope::Scope;
use crate::selector::Selector;
use crate::utils::{
    devour_whitespace, devour_whitespace_or_comment, eat_ident, read_until_closing_curly_brace,
};
use crate::{eat_expr, Expr, RuleSet, Stmt, Token};

#[derive(Debug, Clone)]
pub(crate) struct Mixin {
    scope: Scope,
    args: FuncArgs,
    body: Peekable<IntoIter<Token>>,
    content: Vec<Stmt>,
}

impl Mixin {
    pub fn new(scope: Scope, args: FuncArgs, body: Vec<Token>, content: Vec<Stmt>) -> Self {
        let body = body.into_iter().peekable();
        Mixin {
            scope,
            args,
            body,
            content,
        }
    }

    pub fn decl_from_tokens<I: Iterator<Item = Token>>(
        toks: &mut Peekable<I>,
        scope: &Scope,
        super_selector: &Selector,
    ) -> SassResult<(String, Mixin)> {
        devour_whitespace(toks);
        let name = eat_ident(toks, scope, super_selector)?;
        devour_whitespace(toks);
        let args = match toks.next() {
            Some(Token { kind: '(', .. }) => eat_func_args(toks, scope, super_selector)?,
            Some(Token { kind: '{', .. }) => FuncArgs::new(),
            _ => return Err("expected \"{\".".into()),
        };

        devour_whitespace(toks);

        let mut body = read_until_closing_curly_brace(toks);
        body.push(toks.next().unwrap());

        Ok((name, Mixin::new(scope.clone(), args, body, Vec::new())))
    }

    pub fn content(mut self, content: Vec<Stmt>) -> Mixin {
        self.content = content;
        self
    }

    pub fn args(mut self, args: &mut CallArgs) -> SassResult<Mixin> {
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

    pub fn call(mut self, super_selector: &Selector) -> SassResult<Vec<Stmt>> {
        self.eval(super_selector)
    }

    fn eval(&mut self, super_selector: &Selector) -> SassResult<Vec<Stmt>> {
        let mut stmts = Vec::new();
        while let Some(expr) = eat_expr(&mut self.body, &mut self.scope, super_selector)? {
            match expr {
                Expr::AtRule(a) => match a {
                    AtRule::Content => stmts.extend(self.content.clone()),
                    _ => stmts.push(Stmt::AtRule(a)),
                },
                Expr::Style(s) => stmts.push(Stmt::Style(s)),
                Expr::Styles(s) => stmts.extend(s.into_iter().map(Box::new).map(Stmt::Style)),
                Expr::Include(s) => stmts.extend(s),
                Expr::FunctionDecl(..) => {
                    return Err("Mixins may not contain function declarations.".into())
                }
                Expr::MixinDecl(..) => {
                    return Err("Mixins may not contain mixin declarations.".into())
                }
                Expr::Debug(..) | Expr::Warn(..) => todo!(),
                Expr::Selector(selector) => {
                    let rules = self.eval(&super_selector.zip(&selector))?;
                    stmts.push(Stmt::RuleSet(RuleSet {
                        super_selector: super_selector.clone(),
                        selector,
                        rules,
                    }));
                }
                Expr::VariableDecl(name, val) => {
                    self.scope.insert_var(&name, *val)?;
                }
                Expr::MultilineComment(s) => stmts.push(Stmt::MultilineComment(s)),
            }
        }
        Ok(stmts)
    }
}

pub(crate) fn eat_include<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<Vec<Stmt>> {
    devour_whitespace_or_comment(toks)?;
    let name = eat_ident(toks, scope, super_selector)?;

    devour_whitespace_or_comment(toks)?;

    let mut has_include = false;

    let mut args = if let Some(tok) = toks.next() {
        match tok.kind {
            ';' => CallArgs::new(),
            '(' => {
                let tmp = eat_call_args(toks, scope, super_selector)?;
                devour_whitespace_or_comment(toks)?;
                if let Some(tok) = toks.next() {
                    match tok.kind {
                        ';' => {}
                        '{' => has_include = true,
                        _ => todo!(),
                    }
                }
                tmp
            }
            '{' => {
                has_include = true;
                CallArgs::new()
            }
            _ => return Err("expected \"{\".".into()),
        }
    } else {
        return Err("unexpected EOF".into());
    };

    devour_whitespace(toks);

    let content = if let Some(tok) = toks.peek() {
        if tok.kind == '{' {
            toks.next();
            eat_stmts(toks, &mut scope.clone(), super_selector)?
        } else if has_include {
            eat_stmts(toks, &mut scope.clone(), super_selector)?
        } else {
            Vec::new()
        }
    } else {
        Vec::new()
    };

    let mixin = scope.get_mixin(&name)?.clone();

    let rules = mixin
        .args(&mut args)?
        .content(content)
        .call(super_selector)?;
    Ok(rules)
}
