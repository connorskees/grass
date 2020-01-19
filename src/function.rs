use std::collections::BTreeMap;
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

#[derive(Debug, Clone, std::default::Default)]
pub struct CallArgs(pub BTreeMap<String, CallArg>);

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
    pub fn new() -> Self {
        CallArgs(BTreeMap::new())
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn get(&self, val: &str) -> Option<&CallArg> {
        self.0.get(val)
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
        let mut default: Vec<Token> = Vec::new();
        devour_whitespace(toks);
        let kind = match toks.next() {
            Some(Token { kind, .. }) => kind,
            _ => todo!("unexpected eof"),
        };
        match kind {
            TokenKind::Symbol(Symbol::Colon) => {
                devour_whitespace(toks);
                while let Some(tok) = toks.peek() {
                    match &tok.kind {
                        TokenKind::Symbol(Symbol::Comma) => {
                            toks.next();
                            args.push(FuncArg {
                                name,
                                default: Some(default),
                            });
                            break;
                        }
                        TokenKind::Symbol(Symbol::CloseParen) => {
                            args.push(FuncArg {
                                name,
                                default: Some(default),
                            });
                            break;
                        }
                        _ => {
                            let tok = toks.next().expect("we know this exists!");
                            default.push(tok)
                        }
                    }
                }
            }
            TokenKind::Symbol(Symbol::Period) => todo!("handle varargs"),
            TokenKind::Symbol(Symbol::CloseParen) => {
                args.push(FuncArg {
                    name,
                    default: if default.is_empty() {
                        None
                    } else {
                        Some(default)
                    },
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
    let mut args: BTreeMap<String, CallArg> = BTreeMap::new();
    devour_whitespace(toks);
    let mut name: Option<String> = None;
    let mut val = Vec::new();
    while let Some(Token { kind, pos }) = toks.next() {
        match kind {
            TokenKind::Variable(v) => name = Some(v),
            TokenKind::Symbol(Symbol::Colon) => {
                devour_whitespace(toks);
                while let Some(tok) = toks.peek() {
                    match &tok.kind {
                        TokenKind::Symbol(Symbol::Comma) => {
                            toks.next();
                            args.insert(
                                name.clone().unwrap(),
                                CallArg {
                                    name: name.clone(),
                                    val: val.clone(),
                                },
                            );
                            if let Some(ref mut s) = name {
                                s.clear();
                            }
                            val.clear();
                            break;
                        }
                        TokenKind::Symbol(Symbol::CloseParen) => {
                            args.insert(
                                name.clone().unwrap(),
                                CallArg {
                                    name: name.clone(),
                                    val: val.clone(),
                                },
                            );
                            if let Some(ref mut s) = name {
                                s.clear();
                            }
                            val.clear();
                            break;
                        }
                        _ => {
                            let tok = toks.next().expect("we know this exists!");
                            val.push(tok)
                        }
                    }
                }
            }
            TokenKind::Symbol(Symbol::CloseParen) => {
                if name.is_some() {
                    args.insert(name.clone().unwrap(), CallArg { name, val });
                } else {
                    args.insert(format!("{}", args.len()), CallArg { name, val });
                }
                break;
            }
            TokenKind::Symbol(Symbol::Comma) => {
                if let Some(ref name) = name {
                    args.insert(
                        name.clone(),
                        CallArg {
                            name: Some(name.clone()),
                            val: val.clone(),
                        },
                    );
                } else {
                    args.insert(
                        format!("{}", args.len()),
                        CallArg {
                            name: name.clone(),
                            val: val.clone(),
                        },
                    );
                }
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
