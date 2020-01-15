use crate::common::{Pos, Scope, Symbol};
use crate::style::Style;
use crate::utils::{devour_whitespace, eat_variable_value_ref};
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
        dbg!(&self.body);
        while let Some(tok) = &toks.peek() {
            match &tok.kind {
                TokenKind::Symbol(Symbol::SemiColon)
                | TokenKind::Symbol(Symbol::CloseCurlyBrace) => {
                    toks.next();
                    if let Ok(s) = Style::from_tokens(&value, &self.scope) {
                        styles.push(s);
                        value.clear();
                    } else {
                        return styles;
                    }
                }
                TokenKind::Variable(ref name) => {
                    toks.next();
                    if let TokenKind::Symbol(Symbol::Colon) =
                        toks.peek().expect("expected something after variable").kind
                    {
                        toks.next();
                        devour_whitespace(&mut toks);
                        self.scope.vars.insert(
                            name.clone(),
                            eat_variable_value_ref(&mut toks, &self.scope).unwrap(),
                        );
                    } else {
                        value.push(Token {
                            kind: TokenKind::Variable(name.clone()),
                            pos: Pos::new(),
                        });
                    }
                }
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
