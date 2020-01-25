use std::iter::Peekable;
use std::vec::IntoIter;

use crate::common::{Pos, Scope, Symbol};
use crate::args::{eat_func_args, FuncArgs};
use crate::utils::devour_whitespace;
use crate::{Token, TokenKind};

#[derive(Debug, Clone)]
pub(crate) struct Function {
    scope: Scope,
    args: FuncArgs,
    body: Peekable<IntoIter<Token>>,
}


impl Function {
    pub fn new(scope: Scope, args: FuncArgs, body: Vec<Token>) -> Self {
        let body = body.into_iter().peekable();
        Function { scope, args, body }
    }

    pub fn decl_from_tokens<I: Iterator<Item = Token>>(
        toks: &mut Peekable<I>,
        scope: &Scope,
    ) -> Result<(String, Function), (Pos, String)> {
        let Token { pos, kind } = toks
            .next()
            .expect("this must exist because we have already peeked");
        devour_whitespace(toks);
        let name = match kind {
            TokenKind::Ident(s) => s,
            _ => {
                return Err((
                    pos,
                    String::from("expected identifier after function declaration"),
                ))
            }
        };
        devour_whitespace(toks);
        let args = match toks.next() {
            Some(Token {
                kind: TokenKind::Symbol(Symbol::OpenParen),
                ..
            }) => eat_func_args(toks),
            _ => return Err((pos, String::from("expected `(` after function declaration"))),
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

        Ok((name, Function::new(scope.clone(), args, body)))
    }

}