use std::iter::Peekable;
use std::vec::IntoIter;

use crate::args::{eat_call_args, eat_func_args, CallArgs, FuncArgs};
use crate::common::{Pos, Scope, Symbol};
use crate::selector::Selector;
use crate::utils::devour_whitespace;
use crate::{eat_expr, Expr, RuleSet, Stmt, Token, TokenKind};

#[derive(Debug, Clone)]
pub(crate) struct Mixin {
    scope: Scope,
    args: FuncArgs,
    body: Peekable<IntoIter<Token>>,
}

impl Mixin {
    pub fn new(scope: Scope, args: FuncArgs, body: Vec<Token>) -> Self {
        let body = body.into_iter().peekable();
        Mixin { scope, args, body }
    }

    pub fn decl_from_tokens<I: Iterator<Item = Token>>(
        toks: &mut Peekable<I>,
        scope: &Scope,
    ) -> Result<(String, Mixin), (Pos, String)> {
        let Token { pos, kind } = toks
            .next()
            .expect("this must exist because we have already peeked");
        devour_whitespace(toks);
        let name = match kind {
            TokenKind::Ident(s) => s,
            _ => {
                return Err((
                    pos,
                    String::from("expected identifier after mixin declaration"),
                ))
            }
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
            _ => return Err((pos, String::from("expected `(` or `{`"))),
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
                return Err((pos, String::from("unexpected EOF")));
            }
        }

        Ok((name, Mixin::new(scope.clone(), args, body)))
    }

    pub fn args(mut self, args: &CallArgs) -> Mixin {
        for (idx, arg) in self.args.0.iter().enumerate() {
            let val = match args.get(&format!("{}", idx)) {
                Some(v) => v.clone(),
                None => match args.get(&arg.name) {
                    Some(v) => v.clone(),
                    None => arg.default.clone().expect("missing variable!"),
                },
            };
            self.scope.vars.insert(arg.name.clone(), val);
        }
        self
    }

    pub fn call(mut self, super_selector: &Selector) -> Result<Vec<Stmt>, (Pos, String)> {
        self.eval(super_selector)
    }

    fn eval(&mut self, super_selector: &Selector) -> Result<Vec<Stmt>, (Pos, String)> {
        let mut stmts = Vec::new();
        while let Some(expr) = eat_expr(&mut self.body, &self.scope, super_selector)? {
            match expr {
                Expr::Style(s) => stmts.push(Stmt::Style(s)),
                Expr::Include(..)
                | Expr::MixinDecl(..)
                | Expr::FunctionDecl(..)
                | Expr::Debug(..)
                | Expr::Warn(..) => todo!(),
                Expr::Selector(selector) => {
                    let rules = self.eval(&super_selector.zip(&selector))?;
                    stmts.push(Stmt::RuleSet(RuleSet {
                        super_selector: super_selector.clone(),
                        selector,
                        rules,
                    }));
                }
                Expr::VariableDecl(name, val) => {
                    self.scope.vars.insert(name, val);
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
) -> Result<Vec<Stmt>, (Pos, String)> {
    toks.next();
    devour_whitespace(toks);
    let Token { kind, pos } = toks
        .next()
        .expect("this must exist because we have already peeked");
    let name = match kind {
        TokenKind::Ident(s) => s,
        _ => return Err((pos, String::from("expected identifier"))),
    };

    devour_whitespace(toks);

    let args = if let Some(tok) = toks.next() {
        match tok.kind {
            TokenKind::Symbol(Symbol::SemiColon) => CallArgs::new(),
            TokenKind::Symbol(Symbol::OpenParen) => eat_call_args(toks),
            _ => return Err((pos, String::from("expected `(` or `;`"))),
        }
    } else {
        return Err((pos, String::from("unexpected EOF")));
    };

    devour_whitespace(toks);

    if !args.is_empty() {
        if let Some(tok) = toks.next() {
            assert_eq!(tok.kind, TokenKind::Symbol(Symbol::SemiColon));
        }
    }

    devour_whitespace(toks);

    let mixin = match scope.mixins.get(&name) {
        Some(m) => m.clone(),
        _ => return Err((pos, String::from("expected identifier"))),
    };

    let rules = mixin.args(&args).call(super_selector)?;
    Ok(rules)
}
