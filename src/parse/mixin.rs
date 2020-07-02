use codemap::Spanned;

use peekmore::PeekMore;

use crate::{
    args::{CallArgs, FuncArgs},
    atrule::{Content, Mixin},
    error::SassResult,
    utils::read_until_closing_curly_brace,
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

        let args = if let Some(Token { kind: '(', .. }) = self.toks.peek() {
            self.toks.next();
            self.parse_call_args()?
        } else {
            CallArgs::new(name.span)
        };

        self.whitespace_or_comment();

        let content_args = if let Some(Token { kind: 'u', .. }) | Some(Token { kind: 'U', .. }) =
            self.toks.peek()
        {
            let mut ident = self.parse_identifier_no_interpolation(false)?;
            ident.node.make_ascii_lowercase();
            if ident.node == "using" {
                self.whitespace_or_comment();
                if !matches!(self.toks.next(), Some(Token { kind: '(', .. })) {
                    return Err(("expected \"(\".", ident.span).into());
                }

                Some(self.parse_func_args()?)
            } else {
                return Err(("expected keyword \"using\".", ident.span).into());
            }
        } else {
            None
        };

        self.whitespace_or_comment();

        let content = if content_args.is_some()
            || matches!(self.toks.peek(), Some(Token { kind: '{', .. }))
        {
            if matches!(self.toks.peek(), Some(Token { kind: '{', .. })) {
                self.toks.next();
            }
            let mut toks = read_until_closing_curly_brace(self.toks)?;
            if let Some(tok) = self.toks.peek() {
                toks.push(*tok);
                self.toks.next();
            }
            Some(toks)
        } else {
            None
        };

        if let Some(Token { kind: ';', .. }) = self.toks.peek() {
            self.toks.next();
        }

        let Mixin {
            mut scope,
            body,
            args: fn_args,
            ..
        } = self.scopes.last().get_mixin(name, self.global_scope)?;
        self.eval_args(fn_args, args, &mut scope)?;

        self.content.push(Content {
            content,
            content_args,
            scope: self.scopes.last().clone(),
        });

        let body = Parser {
            toks: &mut body.into_iter().peekmore(),
            map: self.map,
            path: self.path,
            scopes: &mut NeverEmptyVec::new(scope),
            global_scope: self.global_scope,
            super_selectors: self.super_selectors,
            span_before: self.span_before,
            in_mixin: true,
            in_function: self.in_function,
            in_control_flow: self.in_control_flow,
            content: self.content,
            at_root: false,
            at_root_has_selector: self.at_root_has_selector,
            extender: self.extender,
        }
        .parse()?;

        self.content.pop();

        Ok(body)
    }

    pub(super) fn parse_content_rule(&mut self) -> SassResult<Vec<Stmt>> {
        if self.in_mixin {
            let mut scope = self
                .content
                .last()
                .cloned()
                .unwrap_or_else(Content::new)
                .scope;
            if let Some(Token { kind: '(', .. }) = self.toks.peek() {
                self.toks.next();
                let args = self.parse_call_args()?;
                if let Some(Some(content_args)) =
                    self.content.last().map(|v| v.content_args.clone())
                {
                    args.max_args(content_args.len())?;

                    self.eval_args(content_args, args, &mut scope)?;
                } else {
                    args.max_args(0)?;
                }
            }

            Ok(if let Some(content) = &self.content.pop() {
                let stmts = if let Some(body) = content.content.clone() {
                    Parser {
                        toks: &mut body.into_iter().peekmore(),
                        map: self.map,
                        path: self.path,
                        scopes: &mut NeverEmptyVec::new(scope),
                        global_scope: self.global_scope,
                        super_selectors: self.super_selectors,
                        span_before: self.span_before,
                        in_mixin: self.in_mixin,
                        in_function: self.in_function,
                        in_control_flow: self.in_control_flow,
                        content: self.content,
                        at_root: self.at_root,
                        at_root_has_selector: self.at_root_has_selector,
                        extender: self.extender,
                    }
                    .parse()?
                } else {
                    Vec::new()
                };
                self.content.push(content.clone());
                stmts
            } else {
                Vec::new()
            })
        } else {
            Err((
                "@content is only allowed within mixin declarations.",
                self.span_before,
            )
                .into())
        }
    }
}
