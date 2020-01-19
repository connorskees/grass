use std::iter::Peekable;
use std::vec::IntoIter;

use crate::common::{Pos, Printer, Scope, Symbol};
use crate::function::{eat_func_args, CallArgs, FuncArgs};
use crate::selector::Selector;
use crate::utils::devour_whitespace;
use crate::{eat_expr, Expr, RuleSet, Stmt, Token, TokenKind};

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

    pub fn from_tokens<I: Iterator<Item = Token>>(
        toks: &mut Peekable<I>,
        scope: &Scope,
    ) -> Result<(String, Mixin), Printer> {
        let Token { pos, .. } = toks
            .next()
            .expect("this must exist because we have already peeked");
        devour_whitespace(toks);
        let name = if let Some(Token {
            kind: TokenKind::Ident(s),
            ..
        }) = toks.next()
        {
            s
        } else {
            return Err(Printer::Error(
                pos,
                String::from("expected identifier after mixin declaration"),
            ));
        };
        devour_whitespace(toks);
        let args = match toks.next() {
            Some(Token {
                kind: TokenKind::Symbol(Symbol::OpenParen),
                ..
            }) => eat_func_args(toks),
            Some(Token {
                kind: TokenKind::Symbol(Symbol::OpenCurlyBrace),
                ..
            }) => FuncArgs::new(),
            _ => return Err(Printer::Error(pos, String::from("expected `(` or `{`"))),
        };

        let mut nesting = 1;
        let mut body = Vec::new();

        while nesting > 0 {
            if let Some(tok) = toks.next() {
                match &tok.kind {
                TokenKind::Symbol(Symbol::OpenCurlyBrace)
                // interpolation token eats the opening brace but not the closing
                | TokenKind::Interpolation => nesting += 1,
                TokenKind::Symbol(Symbol::CloseCurlyBrace) => nesting -= 1,
                _ => {}
            }
                body.push(tok)
            } else {
                return Err(Printer::Error(pos, String::from("unexpected EOF")));
            }
        }

        Ok((name, Mixin::new(scope.clone(), args, body)))
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
                Expr::Include(_) | Expr::MixinDecl(_, _) => todo!(),
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
