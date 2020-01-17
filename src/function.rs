use std::iter::Peekable;

use crate::common::Symbol;
use crate::utils::devour_whitespace;
use crate::{Token, TokenKind};

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

pub fn eat_func_args<I: Iterator<Item = Token>>(toks: &mut Peekable<I>) -> FuncArgs {
    let mut args: Vec<FuncArg> = Vec::new();

    devour_whitespace(toks);
    while let Some(Token { kind, .. }) = toks.next() {
        let name = match kind {
            TokenKind::Variable(v) => v,
            TokenKind::Symbol(Symbol::CloseParen) => break,
            _ => todo!(),
        };
        devour_whitespace(toks);
        let kind = if let Some(Token { kind, .. }) = toks.next() {
            kind
        } else {
            todo!()
        };
        match kind {
            TokenKind::Symbol(Symbol::Colon) => {
                todo!("handle default values")
                // let mut val: Vec<Token> = Vec::new();
                // while let Some(tok) = toks.next() {
                //     match &kind {
                //         _ => val.push(tok),
                //     }
                // }
            }
            TokenKind::Symbol(Symbol::Period) => todo!("handle varargs"),
            TokenKind::Symbol(Symbol::CloseParen) => {
                args.push(FuncArg {
                    name,
                    default: None,
                });
                break;
            }
            TokenKind::Symbol(Symbol::Comma) => args.push(FuncArg {
                name,
                default: None,
            }),
            _ => {}
        }
        devour_whitespace(toks);
    }
    devour_whitespace(toks);
    if let Some(Token {
        kind: TokenKind::Symbol(Symbol::OpenCurlyBrace),
        ..
    }) = toks.next()
    {
    } else {
        todo!("expected `{{` after mixin args")
    }
    FuncArgs(args)
}

pub fn eat_call_args<I: Iterator<Item = Token>>(toks: &mut Peekable<I>) -> CallArgs {
    let mut args: Vec<CallArg> = Vec::new();
    devour_whitespace(toks);
    let mut name: Option<String> = None;
    let mut val = Vec::new();
    while let Some(Token { kind, pos }) = toks.next() {
        match kind {
            TokenKind::Variable(v) => name = Some(v),
            TokenKind::Symbol(Symbol::Colon) => {
                todo!("handle default values")
                // let mut val: Vec<Token> = Vec::new();
                // while let Some(Token { kind, .. }) = toks.next() {
                //     match &kind {
                //         _ => {}
                //     }
                // }
            }
            TokenKind::Symbol(Symbol::CloseParen) => {
                args.push(CallArg {
                    name: name.clone(),
                    val: val.clone(),
                });
                break;
            }
            TokenKind::Symbol(Symbol::Comma) => {
                args.push(CallArg {
                    name: name.clone(),
                    val: val.clone(),
                });
                if let Some(ref mut s) = name {
                    s.clear();
                }
                val.clear();
            }
            _ => val.push(Token { kind, pos }),
        }
        devour_whitespace(toks);
    }
    CallArgs(args)
}
