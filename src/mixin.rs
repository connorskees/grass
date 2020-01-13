use crate::common::Scope;
use crate::common::Symbol;
use crate::style::Style;
use crate::{Token, TokenKind};

#[derive(Debug, Clone)]
pub struct Mixin {
    scope: Scope,
    args: FuncArgs,
    body: Vec<Token>,
}

impl Mixin {
    pub fn new(scope: Scope, args: FuncArgs, body: Vec<Token>) -> Self {
        Mixin { scope, args, body }
    }

    pub fn eval(&mut self) -> Vec<Style> {
        let mut toks = self.body.iter().peekable();
        let mut styles = Vec::new();
        let mut value = Vec::new();
        while let Some(tok) = toks.peek() {
            match tok.kind {
                TokenKind::Symbol(Symbol::SemiColon) => {
                    if let Ok(s) = Style::from_tokens(&value, &self.scope) {
                        styles.push(s);
                    } else {
                        return styles;
                    }
                }
                // TokenKind::Variable(_) => {
                //     let tok = toks.next().unwrap();
                //     let name = if let TokenKind::Variable(n) = tok.kind {
                //         n
                //     } else {
                //         unsafe { std::hint::unreachable_unchecked() }
                //     };
                //     if let TokenKind::Symbol(Symbol::Colon) = toks
                //         .peek()
                //         .expect("expected something after variable")
                //         .kind
                //     {
                //         toks.next();
                //         devour_whitespace(&mut toks);
                //         return Ok(Expr::VariableDecl(name, self.eat_variable_value()));
                //     } else {
                //         values.push(Token {
                //             kind: TokenKind::Variable(name),
                //             pos: tok.pos,
                //         });
                //     }
                // }
                _ => {
                    if let Some(tok) = toks.next() {
                        value.push(tok.clone())
                    } else {
                        unsafe { std::hint::unreachable_unchecked() }
                    }
                }
            }

            while let Some(Token {
                kind: TokenKind::Whitespace(_),
                ..
            }) = toks.peek()
            {
                toks.next();
            }
        }
        styles
    }
}

#[derive(Debug, Clone)]
pub struct FuncArgs(pub Vec<(Option<String>, Vec<Token>)>);

impl FuncArgs {
    pub const fn new() -> Self {
        FuncArgs(Vec::new())
    }
}

#[derive(Debug, Clone)]
pub struct CallArgs(Vec<(Option<String>, Vec<Token>)>);
