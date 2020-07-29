use std::{collections::HashMap, mem};

use codemap::Span;

use crate::{
    args::{CallArg, CallArgs, FuncArg, FuncArgs},
    common::QuoteKind,
    error::SassResult,
    scope::Scope,
    utils::{peek_ident_no_interpolation, peek_whitespace_or_comment, read_until_closing_paren},
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

        self.whitespace_or_comment();
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
            self.whitespace_or_comment();
            let (kind, span) = match self.toks.next() {
                Some(Token { kind, pos }) => (kind, pos),
                None => return Err(("expected \")\".", pos).into()),
            };
            match kind {
                ':' => {
                    self.whitespace_or_comment();
                    while let Some(tok) = self.toks.peek() {
                        match &tok.kind {
                            ',' => {
                                self.toks.next();
                                self.whitespace_or_comment();
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
                    self.whitespace_or_comment();
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
            self.whitespace_or_comment();
        }
        self.whitespace_or_comment();
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

        let mut span = self
            .toks
            .peek()
            .ok_or(("expected \")\".", self.span_before))?
            .pos();

        loop {
            self.whitespace_or_comment();

            if matches!(self.toks.peek(), Some(Token { kind: ')', .. })) {
                self.toks.next();
                return Ok(CallArgs(args, span));
            }

            if let Some(Token { kind: '$', pos }) = self.toks.peek() {
                span = span.merge(*pos);
                self.toks.advance_cursor();

                let v = peek_ident_no_interpolation(self.toks, false, self.span_before)?;

                peek_whitespace_or_comment(self.toks);

                if let Some(Token { kind: ':', .. }) = self.toks.peek() {
                    self.toks.truncate_iterator_to_cursor();
                    self.toks.next();
                    name = v.node;
                } else {
                    self.toks.reset_cursor();
                    name.clear();
                }
            } else {
                name.clear();
            }

            self.whitespace_or_comment();

            let value = self.parse_value(true, &|c| match c.peek() {
                Some(Token { kind: ')', .. }) | Some(Token { kind: ',', .. }) => true,
                Some(Token { kind: '.', .. }) => {
                    if matches!(c.peek_next(), Some(Token { kind: '.', .. })) {
                        c.reset_cursor();
                        true
                    } else {
                        c.reset_cursor();
                        false
                    }
                }
                Some(Token { kind: '=', .. }) => {
                    if matches!(c.peek_next(), Some(Token { kind: '=', .. })) {
                        c.reset_cursor();
                        false
                    } else {
                        c.reset_cursor();
                        true
                    }
                }
                Some(..) | None => false,
            });

            match self.toks.peek() {
                Some(Token { kind: ')', .. }) => {
                    self.toks.next();
                    args.insert(
                        if name.is_empty() {
                            CallArg::Positional(args.len())
                        } else {
                            CallArg::Named(mem::take(&mut name).into())
                        },
                        value,
                    );
                    return Ok(CallArgs(args, span));
                }
                Some(Token { kind: ',', .. }) => {
                    self.toks.next();
                    args.insert(
                        if name.is_empty() {
                            CallArg::Positional(args.len())
                        } else {
                            CallArg::Named(mem::take(&mut name).into())
                        },
                        value,
                    );
                    self.whitespace_or_comment();
                    continue;
                }
                Some(Token { kind: '.', pos }) => {
                    let pos = *pos;
                    self.toks.next();

                    if let Some(Token { kind: '.', pos }) = self.toks.peek().cloned() {
                        if !name.is_empty() {
                            return Err(("expected \")\".", pos).into());
                        }
                        self.toks.next();
                        if let Some(Token { kind: '.', .. }) = self.toks.peek() {
                            self.toks.next();
                        } else {
                            return Err(("expected \".\".", pos).into());
                        }
                    } else {
                        return Err(("expected \")\".", pos).into());
                    }

                    let val = value?;
                    match val.node {
                        Value::ArgList(v) => {
                            for arg in v {
                                args.insert(CallArg::Positional(args.len()), Ok(arg));
                            }
                        }
                        Value::List(v, ..) => {
                            for arg in v {
                                args.insert(
                                    CallArg::Positional(args.len()),
                                    Ok(arg.span(val.span)),
                                );
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
                }
                Some(Token { kind: '=', .. }) => {
                    self.toks.next();
                    let left = value?;

                    let right = self.parse_value(true, &|c| match c.peek() {
                        Some(Token { kind: ')', .. }) | Some(Token { kind: ',', .. }) => true,
                        Some(Token { kind: '.', .. }) => {
                            if matches!(c.peek_next(), Some(Token { kind: '.', .. })) {
                                c.reset_cursor();
                                true
                            } else {
                                c.reset_cursor();
                                false
                            }
                        }
                        Some(..) | None => false,
                    })?;

                    let value_span = left.span.merge(right.span);
                    span = span.merge(value_span);

                    let value = format!(
                        "{}={}",
                        left.node.to_css_string(left.span)?,
                        right.node.to_css_string(right.span)?
                    );

                    args.insert(
                        if name.is_empty() {
                            CallArg::Positional(args.len())
                        } else {
                            CallArg::Named(mem::take(&mut name).into())
                        },
                        Ok(Value::String(value, QuoteKind::None).span(value_span)),
                    );

                    match self.toks.peek() {
                        Some(Token { kind: ')', .. }) => {
                            self.toks.next();
                            return Ok(CallArgs(args, span));
                        }
                        Some(Token { kind: ',', pos }) => {
                            span = span.merge(*pos);
                            self.toks.next();
                            self.whitespace_or_comment();
                            continue;
                        }
                        Some(Token { kind: '.', pos }) => {
                            let pos = *pos;
                            self.toks.next();

                            if let Some(Token { kind: '.', pos }) = self.toks.peek().cloned() {
                                if !name.is_empty() {
                                    return Err(("expected \")\".", pos).into());
                                }
                                self.toks.next();
                                if let Some(Token { kind: '.', .. }) = self.toks.peek() {
                                    self.toks.next();
                                } else {
                                    return Err(("expected \".\".", pos).into());
                                }
                            } else {
                                return Err(("expected \")\".", pos).into());
                            }
                        }
                        Some(..) => unreachable!(),
                        None => return Err(("expected \")\".", span).into()),
                    }
                }
                Some(..) => {
                    value?;
                    unreachable!()
                }
                None => return Err(("expected \")\".", span).into()),
            }
        }
    }
}

impl<'a> Parser<'a> {
    pub(super) fn eval_args(&mut self, fn_args: FuncArgs, mut args: CallArgs) -> SassResult<Scope> {
        let mut scope = Scope::new();
        if fn_args.0.is_empty() {
            args.max_args(0)?;
            return Ok(scope);
        }
        self.scopes.enter_new_scope();
        for (idx, mut arg) in fn_args.0.into_iter().enumerate() {
            if arg.is_variadic {
                let arg_list = Value::ArgList(args.get_variadic()?);
                scope.insert_var(arg.name, arg_list);
                break;
            }
            let val = match args.get(idx, arg.name) {
                Some(v) => v,
                None => match arg.default.as_mut() {
                    Some(v) => self.parse_value_from_vec(mem::take(v), true),
                    None => {
                        return Err(
                            (format!("Missing argument ${}.", &arg.name), args.span()).into()
                        )
                    }
                },
            }?
            .node;
            self.scopes.insert_var(arg.name, val.clone());
            scope.insert_var(arg.name, val);
        }
        self.scopes.exit_scope();
        Ok(scope)
    }
}
