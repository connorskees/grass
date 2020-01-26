use std::collections::BTreeMap;
use std::iter::Peekable;

use crate::common::{Scope, Symbol};
use crate::utils::devour_whitespace;
use crate::value::Value;
use crate::{Token, TokenKind};

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct FuncArgs(pub Vec<FuncArg>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct FuncArg {
    pub name: String,
    pub default: Option<Value>,
}

impl FuncArgs {
    pub const fn new() -> Self {
        FuncArgs(Vec::new())
    }
}

#[derive(Debug, Clone, std::default::Default)]
pub(crate) struct CallArgs(pub BTreeMap<String, Value>);

impl CallArgs {
    pub fn new() -> Self {
        CallArgs(BTreeMap::new())
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn get(&self, val: &str) -> Option<&Value> {
        self.0.get(val)
    }
}

pub(crate) fn eat_func_args<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    scope: &Scope,
) -> FuncArgs {
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
                                default: Some(
                                    Value::from_tokens(&mut default.into_iter().peekable(), scope)
                                        .unwrap(),
                                ),
                            });
                            break;
                        }
                        TokenKind::Symbol(Symbol::CloseParen) => {
                            args.push(FuncArg {
                                name,
                                default: Some(
                                    Value::from_tokens(&mut default.into_iter().peekable(), scope)
                                        .unwrap(),
                                ),
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
                        Some(
                            Value::from_tokens(&mut default.into_iter().peekable(), scope).unwrap(),
                        )
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
        todo!("expected `{{` after args")
    }
    FuncArgs(args)
}

pub(crate) fn eat_call_args<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    scope: &Scope,
) -> CallArgs {
    let mut args: BTreeMap<String, Value> = BTreeMap::new();
    devour_whitespace(toks);
    let mut name: Option<String> = None;
    let mut val = Vec::new();
    while let Some(Token { kind, pos }) = toks.next() {
        match kind {
            TokenKind::Variable(v) => {
                devour_whitespace(toks);
                match toks.peek() {
                    Some(Token {
                        kind: TokenKind::Symbol(Symbol::Colon),
                        ..
                    }) => name = Some(v),
                    Some(Token {
                        kind: TokenKind::Symbol(Symbol::Comma),
                        ..
                    }) => {
                        toks.next();
                        match name {
                            Some(ref name) => {
                                args.insert(name.clone(), scope.vars.get(&v).unwrap().clone())
                            }
                            None => args.insert(
                                format!("{}", args.len()),
                                scope.vars.get(&v).unwrap().clone(),
                            ),
                        };
                        if let Some(ref mut s) = name {
                            s.clear();
                        }
                        val.clear();
                    }
                    Some(Token {
                        kind: TokenKind::Symbol(Symbol::CloseParen),
                        ..
                    }) => {
                        toks.next();
                        match name {
                            Some(name) => args.insert(name, scope.vars.get(&v).unwrap().clone()),
                            None => args.insert(
                                format!("{}", args.len()),
                                scope.vars.get(&v).unwrap().clone(),
                            ),
                        };
                        break;
                    }
                    _ => todo!("unexpected token after variable in call args"),
                }
            }
            TokenKind::Symbol(Symbol::Colon) => {
                devour_whitespace(toks);
                while let Some(tok) = toks.peek() {
                    match &tok.kind {
                        TokenKind::Symbol(Symbol::Comma) => {
                            toks.next();
                            args.insert(
                                name.clone().unwrap(),
                                Value::from_tokens(&mut val.clone().into_iter().peekable(), scope)
                                    .unwrap(),
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
                                Value::from_tokens(&mut val.clone().into_iter().peekable(), scope)
                                    .unwrap(),
                            );
                            break;
                        }
                        _ => val.push(toks.next().expect("we know this exists!")),
                    }
                }
            }
            TokenKind::Symbol(Symbol::CloseParen) => {
                if val.is_empty() {
                    break;
                }
                match name {
                    Some(name) => args.insert(
                        name,
                        Value::from_tokens(&mut val.into_iter().peekable(), scope).unwrap(),
                    ),
                    None => args.insert(
                        format!("{}", args.len()),
                        Value::from_tokens(&mut val.into_iter().peekable(), scope).unwrap(),
                    ),
                };
                break;
            }
            TokenKind::Symbol(Symbol::Comma) => {
                match name {
                    Some(ref name) => args.insert(
                        name.clone(),
                        Value::from_tokens(&mut val.clone().into_iter().peekable(), scope).unwrap(),
                    ),
                    None => args.insert(
                        format!("{}", args.len()),
                        Value::from_tokens(&mut val.clone().into_iter().peekable(), scope).unwrap(),
                    ),
                };
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
