use std::collections::BTreeMap;
use std::iter::Peekable;

use crate::common::{Scope, Symbol};
use crate::error::SassResult;
use crate::utils::{devour_whitespace, devour_whitespace_or_comment};
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

    pub fn get(&self, val: &str) -> Option<&Value> {
        self.0.get(val)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn remove(&mut self, s: &str) -> Option<Value> {
        self.0.remove(s)
    }
}

pub(crate) fn eat_func_args<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    scope: &Scope,
) -> SassResult<FuncArgs> {
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
                                default: Some(Value::from_tokens(
                                    &mut default.into_iter().peekable(),
                                    scope,
                                )?),
                            });
                            break;
                        }
                        TokenKind::Symbol(Symbol::CloseParen) => {
                            args.push(FuncArg {
                                name,
                                default: Some(Value::from_tokens(
                                    &mut default.into_iter().peekable(),
                                    scope,
                                )?),
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
                        Some(Value::from_tokens(
                            &mut default.into_iter().peekable(),
                            scope,
                        )?)
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
    Ok(FuncArgs(args))
}

pub(crate) fn eat_call_args<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    scope: &Scope,
) -> SassResult<CallArgs> {
    let mut args: BTreeMap<String, Value> = BTreeMap::new();
    devour_whitespace_or_comment(toks);
    let mut name: String;
    let mut val: Vec<Token> = Vec::new();
    loop {
        match toks.peek().unwrap().kind {
            TokenKind::Variable(_) => {
                let v = toks.next().unwrap();
                devour_whitespace_or_comment(toks);
                if toks.next().unwrap().is_symbol(Symbol::Colon) {
                    name = v.kind.to_string();
                } else {
                    val.push(v);
                    name = args.len().to_string();
                }
            }
            TokenKind::Symbol(Symbol::CloseParen) => {
                toks.next();
                return Ok(CallArgs(args));
            }
            _ => name = args.len().to_string(),
        }
        devour_whitespace_or_comment(toks);

        while let Some(tok) = toks.next() {
            match tok.kind {
                TokenKind::Symbol(Symbol::CloseParen) => {
                    args.insert(
                        name,
                        Value::from_tokens(&mut val.into_iter().peekable(), scope)?,
                    );
                    return Ok(CallArgs(args));
                }
                TokenKind::Symbol(Symbol::Comma) => break,
                TokenKind::Symbol(Symbol::OpenParen) => {
                    val.push(tok);
                    val.extend(read_until_close_paren(toks));
                }
                _ => val.push(tok),
            }
        }

        args.insert(
            name,
            Value::from_tokens(&mut val.clone().into_iter().peekable(), scope)?,
        );
        val.clear();
        devour_whitespace_or_comment(toks);

        if toks.peek().is_none() {
            return Ok(CallArgs(args));
        }
    }
}

fn read_until_close_paren<I: Iterator<Item = Token>>(toks: &mut Peekable<I>) -> Vec<Token> {
    let mut v = Vec::new();
    let mut scope = 0;
    for tok in toks {
        match tok.kind {
            TokenKind::Symbol(Symbol::CloseParen) => {
                if scope <= 1 {
                    v.push(tok);
                    return v;
                } else {
                    scope -= 1;
                }
            }
            TokenKind::Symbol(Symbol::OpenParen) => scope += 1,
            _ => {}
        }
        v.push(tok)
    }
    v
}
