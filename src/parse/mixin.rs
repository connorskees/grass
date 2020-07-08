use codemap::Spanned;

use peekmore::PeekMore;

use crate::{
    args::{CallArgs, FuncArgs},
    atrule::{Content, Mixin},
    error::SassResult,
    scope::Scopes,
    utils::read_until_closing_curly_brace,
    Token,
};

use super::{common::ContextFlags, Parser, Stmt};

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

        let mixin = Mixin::new(args, body, false, self.at_root);

        if self.at_root {
            self.global_scope.insert_mixin(name, mixin);
        } else {
            self.scopes.insert_mixin(name.into(), mixin);
        }
        Ok(())
    }

    pub(super) fn parse_include(&mut self) -> SassResult<Vec<Stmt>> {
        self.whitespace_or_comment();
        let name = self.parse_identifier()?.map_node(Into::into);

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
            body,
            args: fn_args,
            declared_at_root,
            ..
        } = self.scopes.get_mixin(
            {
                let Spanned { ref node, span } = name;
                Spanned { node, span }
            },
            self.global_scope,
        )?;

        let scope = self.eval_args(fn_args, args)?;

        let mut new_scope = Scopes::new();
        let mut entered_scope = false;
        if declared_at_root {
            new_scope.enter_scope(scope);
        } else {
            entered_scope = true;
            self.scopes.enter_scope(scope);
        };

        self.content.push(Content {
            content,
            content_args,
            scopes: self.scopes.clone(),
        });

        let body = Parser {
            toks: &mut body.into_iter().peekmore(),
            map: self.map,
            path: self.path,
            scopes: if declared_at_root {
                &mut new_scope
            } else {
                self.scopes
            },
            global_scope: self.global_scope,
            super_selectors: self.super_selectors,
            span_before: self.span_before,
            flags: self.flags | ContextFlags::IN_MIXIN,
            content: self.content,
            at_root: false,
            at_root_has_selector: self.at_root_has_selector,
            extender: self.extender,
        }
        .parse()?;

        self.content.pop();
        if entered_scope {
            self.scopes.exit_scope();
        }

        Ok(body)
    }

    pub(super) fn parse_content_rule(&mut self) -> SassResult<Vec<Stmt>> {
        if self.flags.in_mixin() {
            let mut scope = self
                .content
                .last()
                .cloned()
                .unwrap_or_else(Content::new)
                .scopes;
            if let Some(Token { kind: '(', .. }) = self.toks.peek() {
                self.toks.next();
                let args = self.parse_call_args()?;
                if let Some(Some(content_args)) =
                    self.content.last().map(|v| v.content_args.clone())
                {
                    args.max_args(content_args.len())?;

                    scope.merge(self.eval_args(content_args, args)?);
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
                        scopes: &mut scope,
                        global_scope: self.global_scope,
                        super_selectors: self.super_selectors,
                        span_before: self.span_before,
                        flags: self.flags,
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
                self.scopes.exit_scope();
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
