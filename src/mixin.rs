use std::iter::Peekable;
use std::vec::IntoIter;

use crate::args::{eat_call_args, eat_func_args, CallArgs, FuncArgs};
use crate::common::{Scope, Symbol};
use crate::error::{SassError, SassResult};
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
    ) -> SassResult<(String, Mixin)> {
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
            Some(Token {
                kind: TokenKind::Symbol(Symbol::OpenCurlyBrace),
                ..
            }) => FuncArgs::new(),
            _ => return Err("expected \"{\"".into()),
        };

        devour_whitespace(toks);

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
                return Err("unexpected EOF (TODO: better message)".into());
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
            self.scope.insert_var(&arg.name, val);
        }
        self
    }

    pub fn call(mut self, super_selector: &Selector) -> SassResult<Vec<Stmt>> {
        self.eval(super_selector)
    }

    fn eval(&mut self, super_selector: &Selector) -> SassResult<Vec<Stmt>> {
        let mut stmts = Vec::new();
        while let Some(expr) = eat_expr(&mut self.body, &self.scope, super_selector)? {
            match expr {
                Expr::Style(s) => stmts.push(Stmt::Style(s)),
                Expr::Styles(s) => stmts.extend(s.into_iter().map(Stmt::Style)),
                Expr::Include(s) => stmts.extend(s),
                Expr::MixinDecl(..) | Expr::FunctionDecl(..) | Expr::Debug(..) | Expr::Warn(..) => {
                    todo!()
                }
                Expr::Selector(selector) => {
                    let rules = self.eval(&super_selector.zip(&selector))?;
                    stmts.push(Stmt::RuleSet(RuleSet {
                        super_selector: super_selector.clone(),
                        selector,
                        rules,
                    }));
                }
                Expr::VariableDecl(name, val) => {
                    self.scope.insert_var(&name, *val);
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
    toks.next();
    devour_whitespace(toks);
    let Token { kind, pos } = toks
        .next()
        .expect("this must exist because we have already peeked");
    let name = match kind {
        TokenKind::Ident(s) => s,
        _ => return Err(SassError::new("Expected identifier.", pos)),
    };

    devour_whitespace(toks);

    let args = if let Some(tok) = toks.next() {
        match tok.kind {
            TokenKind::Symbol(Symbol::SemiColon) => CallArgs::new(),
            TokenKind::Symbol(Symbol::OpenParen) => {
                let tmp = eat_call_args(toks, scope)?;
                devour_whitespace(toks);
                if let Some(tok) = toks.next() {
                    assert_eq!(tok.kind, TokenKind::Symbol(Symbol::SemiColon));
                }
                tmp
            }
            _ => return Err(SassError::new("expected `(` or `;`", pos)),
        }
    } else {
        return Err(SassError::new("unexpected EOF", pos));
    };

    devour_whitespace(toks);

    let mixin = scope.get_mixin(&name)?.clone();

    let rules = mixin.args(&args).call(super_selector)?;
    Ok(rules)
}
