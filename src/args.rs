use std::collections::BTreeMap;
use std::iter::Peekable;

use crate::common::Pos;
use crate::error::SassResult;
use crate::scope::Scope;
use crate::selector::Selector;
use crate::utils::{devour_whitespace, devour_whitespace_or_comment, eat_ident};
use crate::value::Value;
use crate::Token;

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

    pub fn is_empty(&self) -> bool {
        self.0.len() == 0
    }

    pub fn remove(&mut self, s: &str) -> Option<Value> {
        self.0.remove(s)
    }
}

pub(crate) fn eat_func_args<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<FuncArgs> {
    let mut args: Vec<FuncArg> = Vec::new();

    devour_whitespace(toks);
    while let Some(Token { kind, .. }) = toks.next() {
        let name = match kind {
            '$' => eat_ident(toks, scope, super_selector)?,
            ')' => break,
            _ => todo!(),
        };
        let mut default: Vec<Token> = Vec::new();
        devour_whitespace(toks);
        let kind = match toks.next() {
            Some(Token { kind, .. }) => kind,
            _ => todo!("unexpected eof"),
        };
        match kind {
            ':' => {
                devour_whitespace(toks);
                while let Some(tok) = toks.peek() {
                    match &tok.kind {
                        ',' => {
                            toks.next();
                            args.push(FuncArg {
                                name,
                                default: Some(Value::from_tokens(
                                    &mut default.into_iter().peekable(),
                                    scope,
                                    super_selector,
                                )?),
                            });
                            break;
                        }
                        ')' => {
                            args.push(FuncArg {
                                name,
                                default: Some(Value::from_tokens(
                                    &mut default.into_iter().peekable(),
                                    scope,
                                    super_selector,
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
            '.' => todo!("handle varargs"),
            ')' => {
                args.push(FuncArg {
                    name,
                    default: if default.is_empty() {
                        None
                    } else {
                        Some(Value::from_tokens(
                            &mut default.into_iter().peekable(),
                            scope,
                            super_selector,
                        )?)
                    },
                });
                break;
            }
            ',' => args.push(FuncArg {
                name,
                default: None,
            }),
            _ => {}
        }
        devour_whitespace(toks);
    }
    devour_whitespace(toks);
    if let Some(Token { kind: '{', .. }) = toks.next() {
    } else {
        todo!("expected `{{` after args")
    }
    Ok(FuncArgs(args))
}

pub(crate) fn eat_call_args<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<CallArgs> {
    let mut args: BTreeMap<String, Value> = BTreeMap::new();
    devour_whitespace_or_comment(toks)?;
    let mut name: String;
    let mut val: Vec<Token> = Vec::new();
    loop {
        match toks.peek().unwrap().kind {
            '$' => {
                toks.next();
                let v = eat_ident(toks, scope, super_selector)?;
                devour_whitespace_or_comment(toks)?;
                if toks.peek().unwrap().kind == ':' {
                    toks.next();
                    name = v;
                } else {
                    val.push(Token::new(Pos::new(), '$'));
                    val.extend(v.chars().map(|x| Token::new(Pos::new(), x)));
                    name = args.len().to_string();
                }
            }
            ')' => {
                toks.next();
                return Ok(CallArgs(args));
            }
            _ => name = args.len().to_string(),
        }
        devour_whitespace_or_comment(toks)?;

        while let Some(tok) = toks.next() {
            match tok.kind {
                ')' => {
                    args.insert(
                        name,
                        Value::from_tokens(&mut val.into_iter().peekable(), scope, super_selector)?,
                    );
                    return Ok(CallArgs(args));
                }
                ',' => break,
                '[' => {
                    val.push(tok);
                    val.extend(read_until_close_square_brace(toks));
                }
                '(' => {
                    val.push(tok);
                    val.extend(read_until_close_paren(toks));
                }
                _ => val.push(tok),
            }
        }

        args.insert(
            name,
            Value::from_tokens(
                &mut val.clone().into_iter().peekable(),
                scope,
                super_selector,
            )?,
        );
        val.clear();
        devour_whitespace(toks);

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
            ')' => {
                if scope <= 1 {
                    v.push(tok);
                    return v;
                } else {
                    scope -= 1;
                }
            }
            '(' => scope += 1,
            _ => {}
        }
        v.push(tok)
    }
    v
}

fn read_until_close_square_brace<I: Iterator<Item = Token>>(toks: &mut Peekable<I>) -> Vec<Token> {
    let mut v = Vec::new();
    let mut scope = 0;
    for tok in toks {
        match tok.kind {
            ']' => {
                if scope <= 1 {
                    v.push(tok);
                    return v;
                } else {
                    scope -= 1;
                }
            }
            '[' => scope += 1,
            _ => {}
        }
        v.push(tok)
    }
    v
}
