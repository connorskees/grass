use std::{collections::HashMap, mem};

use codemap::{Span, Spanned};

use crate::{
    args::{CallArg, CallArgs, FuncArg, FuncArgs},
    error::SassResult,
    scope::Scope,
    utils::{read_until_closing_paren, read_until_closing_quote, read_until_closing_square_brace},
    value::Value,
    Token,
};

use super::Parser;

impl<'a> Parser<'a> {
    pub(super) fn parse_func_args(&mut self) -> SassResult<FuncArgs> {
        let mut args: Vec<FuncArg> = Vec::new();
        let mut close_paren_span: Span = match self.toks.peek() {
            Some(Token { pos, .. }) => *pos,
            None => return Err(("expected \")\".", self.span_before).into()),
        };

        self.whitespace();
        while let Some(Token { kind, pos }) = self.toks.next() {
            let name = match kind {
                '$' => self.parse_identifier_no_interpolation(false)?,
                ')' => {
                    close_paren_span = pos;
                    break;
                }
                _ => return Err(("expected \")\".", pos).into()),
            };
            let mut default: Vec<Token> = Vec::new();
            let mut is_variadic = false;
            self.whitespace();
            let (kind, span) = match self.toks.next() {
                Some(Token { kind, pos }) => (kind, pos),
                None => return Err(("expected \")\".", pos).into()),
            };
            match kind {
                ':' => {
                    self.whitespace();
                    while let Some(tok) = self.toks.peek() {
                        match &tok.kind {
                            ',' => {
                                self.toks.next();
                                args.push(FuncArg {
                                    name: name.node.into(),
                                    default: Some(default),
                                    is_variadic,
                                });
                                break;
                            }
                            ')' => {
                                args.push(FuncArg {
                                    name: name.node.into(),
                                    default: Some(default),
                                    is_variadic,
                                });
                                close_paren_span = tok.pos();
                                break;
                            }
                            '(' => {
                                default.push(self.toks.next().unwrap());
                                default.extend(read_until_closing_paren(self.toks)?);
                            }
                            _ => default.push(self.toks.next().unwrap()),
                        }
                    }
                }
                '.' => {
                    let next = self.toks.next().ok_or(("expected \".\".", span))?;
                    if next.kind != '.' {
                        return Err(("expected \".\".", next.pos()).into());
                    }
                    let next = self.toks.next().ok_or(("expected \".\".", next.pos()))?;
                    if next.kind != '.' {
                        return Err(("expected \".\".", next.pos()).into());
                    }
                    self.whitespace();
                    let next = self.toks.next().ok_or(("expected \")\".", next.pos()))?;
                    if next.kind != ')' {
                        return Err(("expected \")\".", next.pos()).into());
                    }

                    is_variadic = true;

                    args.push(FuncArg {
                        name: name.node.into(),
                        default: Some(default),
                        is_variadic,
                    });
                    break;
                }
                ')' => {
                    close_paren_span = span;
                    args.push(FuncArg {
                        name: name.node.into(),
                        default: if default.is_empty() {
                            None
                        } else {
                            Some(default)
                        },
                        is_variadic,
                    });
                    break;
                }
                ',' => args.push(FuncArg {
                    name: name.node.into(),
                    default: None,
                    is_variadic,
                }),
                _ => {}
            }
            self.whitespace();
        }
        self.whitespace();
        // TODO: this should NOT eat the opening curly brace
        match self.toks.next() {
            Some(v) if v.kind == '{' => {}
            Some(..) | None => return Err(("expected \"{\".", close_paren_span).into()),
        };
        Ok(FuncArgs(args))
    }

    pub(super) fn parse_call_args(&mut self) -> SassResult<CallArgs> {
        let mut args = HashMap::new();
        self.whitespace_or_comment();
        let mut name = String::new();
        let mut val: Vec<Token> = Vec::new();
        let mut span = self
            .toks
            .peek()
            .ok_or(("expected \")\".", self.span_before))?
            .pos();
        loop {
            match self.toks.peek().cloned() {
                Some(Token { kind: '$', pos }) => {
                    span = span.merge(pos);
                    self.toks.next();
                    let v = self.parse_identifier_no_interpolation(false)?;
                    let whitespace = self.whitespace_or_comment();
                    if let Some(Token { kind: ':', .. }) = self.toks.peek() {
                        self.toks.next();
                        name = v.node;
                    } else {
                        val.push(Token::new(pos, '$'));
                        let mut current_pos = 0;
                        val.extend(v.chars().map(|x| {
                            let len = x.len_utf8() as u64;
                            let tok = Token::new(v.span.subspan(current_pos, current_pos + len), x);
                            current_pos += len;
                            tok
                        }));
                        if whitespace {
                            val.push(Token::new(pos, ' '));
                        }
                        name.clear();
                    }
                }
                Some(Token { kind: ')', .. }) => {
                    self.toks.next();
                    return Ok(CallArgs(args, span));
                }
                Some(..) | None => name.clear(),
            }
            self.whitespace_or_comment();

            let mut is_splat = false;

            while let Some(tok) = self.toks.next() {
                match tok.kind {
                    ')' => {
                        args.insert(
                            if name.is_empty() {
                                CallArg::Positional(args.len())
                            } else {
                                CallArg::Named(mem::take(&mut name).into())
                            },
                            self.parse_value_from_vec(val),
                        );
                        span = span.merge(tok.pos());
                        return Ok(CallArgs(args, span));
                    }
                    ',' => break,
                    '[' => {
                        val.push(tok);
                        val.extend(read_until_closing_square_brace(self.toks)?);
                    }
                    '(' => {
                        val.push(tok);
                        val.extend(read_until_closing_paren(self.toks)?);
                    }
                    '"' | '\'' => {
                        val.push(tok);
                        val.extend(read_until_closing_quote(self.toks, tok.kind)?);
                    }
                    '.' => {
                        if let Some(Token { kind: '.', pos }) = self.toks.peek().cloned() {
                            if !name.is_empty() {
                                return Err(("expected \")\".", pos).into());
                            }
                            self.toks.next();
                            if let Some(Token { kind: '.', .. }) = self.toks.peek() {
                                self.toks.next();
                                is_splat = true;
                                break;
                            } else {
                                return Err(("expected \".\".", pos).into());
                            }
                        } else {
                            val.push(tok);
                        }
                    }
                    _ => val.push(tok),
                }
            }

            if is_splat {
                let val = self.parse_value_from_vec(mem::take(&mut val))?;
                match val.node {
                    Value::ArgList(v) => {
                        for arg in v {
                            args.insert(CallArg::Positional(args.len()), Ok(arg));
                        }
                    }
                    Value::List(v, ..) => {
                        for arg in v {
                            args.insert(CallArg::Positional(args.len()), Ok(arg.span(val.span)));
                        }
                    }
                    Value::Map(v) => {
                        // NOTE: we clone the map here because it is used
                        // later for error reporting. perhaps there is
                        // some way around this?
                        for (name, arg) in v.clone().entries() {
                            let name = match name {
                                Value::String(s, ..) => s,
                                _ => {
                                    return Err((
                                        format!(
                                            "{} is not a string in {}.",
                                            name.inspect(val.span)?,
                                            Value::Map(v).inspect(val.span)?
                                        ),
                                        val.span,
                                    )
                                        .into())
                                }
                            };
                            args.insert(CallArg::Named(name.into()), Ok(arg.span(val.span)));
                        }
                    }
                    _ => {
                        args.insert(CallArg::Positional(args.len()), Ok(val));
                    }
                }
            } else {
                args.insert(
                    if name.is_empty() {
                        CallArg::Positional(args.len())
                    } else {
                        CallArg::Named(mem::take(&mut name).into())
                    },
                    self.parse_value_from_vec(mem::take(&mut val)),
                );
            }

            self.whitespace();

            if self.toks.peek().is_none() {
                return Err(("expected \")\".", span).into());
            }
        }
    }
}

impl<'a> Parser<'a> {
    #[allow(clippy::unused_self)]
    pub fn arg(
        &self,
        args: &mut CallArgs,
        position: usize,
        name: &'static str,
    ) -> SassResult<Value> {
        Ok(args.get_err(position, name)?.node)
    }

    #[allow(clippy::unused_self)]
    pub fn default_arg(
        &self,
        args: &mut CallArgs,
        position: usize,
        name: &'static str,
        default: Value,
    ) -> SassResult<Value> {
        Ok(match args.get(position, name) {
            Some(val) => val?.node,
            None => default,
        })
    }

    #[allow(clippy::unused_self)]
    pub fn positional_arg(
        &self,
        args: &mut CallArgs,
        position: usize,
    ) -> Option<SassResult<Spanned<Value>>> {
        args.get_positional(position)
    }

    #[allow(dead_code, clippy::unused_self)]
    fn named_arg(
        &self,
        args: &mut CallArgs,
        name: &'static str,
    ) -> Option<SassResult<Spanned<Value>>> {
        args.get_named(name)
    }

    #[allow(clippy::unused_self)]
    pub fn default_named_arg(
        &self,
        args: &mut CallArgs,
        name: &'static str,
        default: Value,
    ) -> SassResult<Value> {
        Ok(match args.get_named(name) {
            Some(val) => val?.node,
            None => default,
        })
    }

    #[allow(clippy::unused_self)]
    pub fn variadic_args(&self, args: CallArgs) -> SassResult<Vec<Spanned<Value>>> {
        let mut vals = Vec::new();
        let mut args = match args
            .0
            .into_iter()
            .map(|(a, v)| Ok((a.position()?, v)))
            .collect::<Result<Vec<(usize, SassResult<Spanned<Value>>)>, String>>()
        {
            Ok(v) => v,
            Err(e) => return Err((format!("No argument named ${}.", e), args.1).into()),
        };
        args.sort_by(|(a1, _), (a2, _)| a1.cmp(a2));
        for arg in args {
            vals.push(arg.1?);
        }
        Ok(vals)
    }

    pub(super) fn eval_args(
        &mut self,
        mut fn_args: FuncArgs,
        mut args: CallArgs,
    ) -> SassResult<Scope> {
        let mut scope = Scope::new();
        if fn_args.0.is_empty() {
            args.max_args(0)?;
            return Ok(scope);
        }
        self.scopes.enter_new_scope();
        for (idx, arg) in fn_args.0.iter_mut().enumerate() {
            if arg.is_variadic {
                let span = args.span();
                let arg_list = Value::ArgList(self.variadic_args(args)?);
                scope.insert_var(
                    arg.name.clone(),
                    Spanned {
                        node: arg_list,
                        span,
                    },
                );
                break;
            }
            let val = match args.get(idx, arg.name.clone()) {
                Some(v) => v,
                None => match arg.default.as_mut() {
                    Some(v) => self.parse_value_from_vec(mem::take(v)),
                    None => {
                        return Err(
                            (format!("Missing argument ${}.", &arg.name), args.span()).into()
                        )
                    }
                },
            }?;
            self.scopes.insert_var(arg.name.clone(), val.clone());
            scope.insert_var(mem::take(&mut arg.name), val);
        }
        self.scopes.exit_scope();
        Ok(scope)
    }
}
