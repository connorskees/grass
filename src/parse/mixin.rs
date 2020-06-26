use std::mem;

use codemap::Spanned;

use crate::{
    args::{CallArgs, FuncArgs},
    atrule::Mixin,
    error::SassResult,
    utils::read_until_closing_curly_brace,
    value::Value,
    Token,
};

use super::{NeverEmptyVec, Parser, Stmt};

impl<'a> Parser<'a> {
    pub(super) fn parse_mixin(&mut self) -> SassResult<()> {
        self.whitespace();
        let Spanned { node: name, span } = self.parse_identifier()?;
        self.whitespace();
        let args = match self.toks.next() {
            Some(Token { kind: '(', .. }) => self.parse_func_args()?,
            Some(Token { kind: '{', .. }) => FuncArgs::new(),
            Some(t) => return Err(("expected \"{\".", t.pos()).into()),
            None => return Err(("expected \"{\".", span).into()),
        };

        self.whitespace();

        let mut body = read_until_closing_curly_brace(self.toks)?;
        body.push(match self.toks.next() {
            Some(tok) => tok,
            None => return Err(("expected \"}\".", self.span_before).into()),
        });

        // todo: `@include` can only give content when `@content` is present within the body
        // if `@content` is *not* present and `@include` attempts to give a body, we throw an error
        // `Error: Mixin doesn't accept a content block.`
        //
        // this is blocked on figuring out just how to check for this. presumably we could have a check
        // not when parsing initially, but rather when `@include`ing to see if an `@content` was found.

        let mixin = Mixin::new(self.scopes.last().clone(), args, body, false);

        if self.at_root {
            self.global_scope.insert_mixin(name, mixin);
        } else {
            self.scopes.last_mut().insert_mixin(name, mixin);
        }
        Ok(())
    }

    pub(super) fn parse_include(&mut self) -> SassResult<Vec<Stmt>> {
        self.whitespace_or_comment();
        let name = self.parse_identifier()?;

        self.whitespace_or_comment();

        let mut has_content = false;

        let args = match self.toks.next() {
            Some(Token { kind: ';', .. }) => CallArgs::new(name.span),
            Some(Token { kind: '(', .. }) => {
                let tmp = self.parse_call_args()?;
                self.whitespace_or_comment();
                if let Some(tok) = self.toks.peek() {
                    match tok.kind {
                        ';' => {
                            self.toks.next();
                        }
                        '{' => {
                            self.toks.next();
                            has_content = true
                        }
                        _ => {}
                    }
                }
                tmp
            }
            Some(Token { kind: '{', .. }) => {
                has_content = true;
                CallArgs::new(name.span)
            }
            Some(Token { pos, .. }) => return Err(("expected \"{\".", pos).into()),
            None => return Err(("expected \"{\".", name.span).into()),
        };

        self.whitespace();

        let content = if has_content {
            Some(self.parse_content()?)
        } else {
            None
        };

        let mut mixin = self.scopes.last().get_mixin(name, self.global_scope)?;
        self.eval_mixin_args(&mut mixin, args)?;

        let body = Parser {
            toks: &mut mixin.body,
            map: self.map,
            path: self.path,
            scopes: &mut NeverEmptyVec::new(mixin.scope),
            global_scope: self.global_scope,
            super_selectors: self.super_selectors,
            span_before: self.span_before,
            in_mixin: true,
            in_function: self.in_function,
            in_control_flow: self.in_control_flow,
            content: content.as_deref(),
            at_root: false,
            at_root_has_selector: self.at_root_has_selector,
            extender: self.extender,
        }
        .parse()?;

        Ok(body)
    }

    pub(super) fn parse_content(&mut self) -> SassResult<Vec<Stmt>> {
        self.parse_stmt()
    }

    fn eval_mixin_args(&mut self, mixin: &mut Mixin, mut args: CallArgs) -> SassResult<()> {
        self.scopes.push(self.scopes.last().clone());
        for (idx, arg) in mixin.args.0.iter_mut().enumerate() {
            if arg.is_variadic {
                let span = args.span();
                // todo: does this get the most recent scope?
                let arg_list = Value::ArgList(self.variadic_args(args)?);
                mixin.scope.insert_var(
                    arg.name.clone(),
                    Spanned {
                        node: arg_list,
                        span,
                    },
                )?;
                break;
            }
            let val = match args.get(idx, arg.name.clone()) {
                Some(v) => self.parse_value_from_vec(v)?,
                None => match arg.default.as_mut() {
                    Some(v) => self.parse_value_from_vec(mem::take(v))?,
                    None => {
                        return Err(
                            (format!("Missing argument ${}.", &arg.name), args.span()).into()
                        )
                    }
                },
            };
            self.scopes
                .last_mut()
                .insert_var(arg.name.clone(), val.clone())?;
            mixin.scope.insert_var(mem::take(&mut arg.name), val)?;
        }
        self.scopes.pop();
        Ok(())
    }
}
