use std::{collections::HashMap, mem};

use codemap::{Span, Spanned};

use crate::{
    args::{CallArg, CallArgs, FuncArg, FuncArgs},
    error::SassResult,
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
        let mut args: HashMap<CallArg, Vec<Token>> = HashMap::new();
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

            while let Some(tok) = self.toks.next() {
                match tok.kind {
                    ')' => {
                        args.insert(
                            if name.is_empty() {
                                CallArg::Positional(args.len())
                            } else {
                                CallArg::Named(name.into())
                            },
                            val,
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
                    _ => val.push(tok),
                }
            }

            args.insert(
                if name.is_empty() {
                    CallArg::Positional(args.len())
                } else {
                    CallArg::Named(name.as_str().into())
                },
                mem::take(&mut val),
            );
            self.whitespace();

            if self.toks.peek().is_none() {
                return Ok(CallArgs(args, span));
            }
        }
    }
}

impl<'a> Parser<'a> {
    pub fn arg(
        &mut self,
        args: &mut CallArgs,
        position: usize,
        name: &'static str,
    ) -> SassResult<Value> {
        Ok(self
            .parse_value_from_vec(args.get_err(position, name)?)?
            .node
            .eval(args.span())?
            .node)
    }

    pub fn default_arg(
        &mut self,
        args: &mut CallArgs,
        position: usize,
        name: &'static str,
        default: Value,
    ) -> SassResult<Value> {
        Ok(match args.get(position, name) {
            Some(toks) => {
                self.parse_value_from_vec(toks)?
                    .node
                    .eval(args.span())?
                    .node
            }
            None => default,
        })
    }

    pub fn positional_arg(
        &mut self,
        args: &mut CallArgs,
        position: usize,
    ) -> Option<SassResult<Spanned<Value>>> {
        Some(self.parse_value_from_vec(args.get_positional(position)?))
    }

    #[allow(dead_code)]
    fn named_arg(
        &mut self,
        args: &mut CallArgs,
        name: &'static str,
    ) -> Option<SassResult<Spanned<Value>>> {
        Some(self.parse_value_from_vec(args.get_named(name)?))
    }

    pub fn default_named_arg(
        &mut self,
        args: &mut CallArgs,
        name: &'static str,
        default: Value,
    ) -> SassResult<Value> {
        Ok(match args.get_named(name) {
            Some(toks) => {
                self.parse_value_from_vec(toks)?
                    .node
                    .eval(args.span())?
                    .node
            }
            None => default,
        })
    }

    pub fn variadic_args(&mut self, args: CallArgs) -> SassResult<Vec<Spanned<Value>>> {
        let mut vals = Vec::new();
        let span = args.span();
        let mut args = match args
            .0
            .into_iter()
            .map(|(a, v)| Ok((a.position()?, v)))
            .collect::<Result<Vec<(usize, Vec<Token>)>, String>>()
        {
            Ok(v) => v,
            Err(e) => return Err((format!("No argument named ${}.", e), args.1).into()),
        };
        args.sort_by(|(a1, _), (a2, _)| a1.cmp(a2));
        for arg in args {
            vals.push(self.parse_value_from_vec(arg.1)?.node.eval(span)?);
        }
        Ok(vals)
    }
}
