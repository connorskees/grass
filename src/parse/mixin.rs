use std::mem;

use codemap::Spanned;

use peekmore::PeekMore;

use crate::{
    args::{CallArgs, FuncArgs},
    atrule::mixin::{Content, Mixin, UserDefinedMixin},
    error::SassResult,
    scope::Scopes,
    utils::read_until_closing_curly_brace,
    Token,
};

use super::{common::ContextFlags, Parser, Stmt};

impl<'a> Parser<'a> {
    pub(super) fn parse_mixin(&mut self) -> SassResult<()> {
        self.whitespace();
        let Spanned { node: name, span } = self.parse_identifier_no_interpolation(false)?;

        if self.flags.in_mixin() {
            return Err(("Mixins may not contain mixin declarations.", span).into());
        }

        if self.flags.in_function() {
            return Err(("This at-rule is not allowed here.", span).into());
        }

        if self.flags.in_control_flow() {
            return Err(("Mixins may not be declared in control directives.", span).into());
        }

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

        let mixin = Mixin::new_user_defined(args, body, false, self.at_root);

        if self.at_root {
            self.global_scope.insert_mixin(name, mixin);
        } else {
            self.scopes.insert_mixin(name.into(), mixin);
        }
        Ok(())
    }

    pub(super) fn parse_include(&mut self) -> SassResult<Vec<Stmt>> {
        if self.flags.in_function() {
            return Err(("This at-rule is not allowed here.", self.span_before).into());
        }

        self.whitespace_or_comment();
        let name = self.parse_identifier()?.map_node(Into::into);

        let mixin = if let Some(Token { kind: '.', .. }) = self.toks.peek() {
            self.toks.next();

            let module = name;
            let name = self.parse_identifier()?.map_node(Into::into);

            self.modules
                .get(module.node, module.span)?
                .get_mixin(name)?
        } else {
            self.scopes.get_mixin(name, self.global_scope)?
        };

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
                self.expect_char('(')?;

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
            self.consume_char_if_exists('{');

            let mut toks = read_until_closing_curly_brace(self.toks)?;
            if let Some(tok) = self.toks.peek() {
                toks.push(*tok);
                self.toks.next();
            }
            Some(toks)
        } else {
            None
        };

        // todo: self.consume_if_exists
        if let Some(Token { kind: ';', .. }) = self.toks.peek() {
            self.toks.next();
        }

        let UserDefinedMixin {
            body,
            args: fn_args,
            declared_at_root,
            ..
        } = match mixin {
            Mixin::UserDefined(u) => u,
            Mixin::Builtin(b) => {
                return b(args, self);
            }
        };

        let scope = self.eval_args(fn_args, args)?;

        let scope_len = self.scopes.len();

        if declared_at_root {
            mem::swap(self.scopes, self.content_scopes);
        }

        self.scopes.enter_scope(scope);

        self.content.push(Content {
            content,
            content_args,
            scope_len,
            declared_at_root,
        });

        let body = Parser {
            toks: &mut body.into_iter().peekmore(),
            map: self.map,
            path: self.path,
            scopes: self.scopes,
            global_scope: self.global_scope,
            super_selectors: self.super_selectors,
            span_before: self.span_before,
            flags: self.flags | ContextFlags::IN_MIXIN,
            content: self.content,
            at_root: false,
            at_root_has_selector: self.at_root_has_selector,
            extender: self.extender,
            content_scopes: self.content_scopes,
            options: self.options,
            modules: self.modules,
            module_config: self.module_config,
        }
        .parse_stmt()?;

        self.content.pop();
        self.scopes.exit_scope();

        if declared_at_root {
            mem::swap(self.scopes, self.content_scopes);
        }

        Ok(body)
    }

    pub(super) fn parse_content_rule(&mut self) -> SassResult<Vec<Stmt>> {
        if !self.flags.in_mixin() {
            return Err((
                "@content is only allowed within mixin declarations.",
                self.span_before,
            )
                .into());
        }

        Ok(if let Some(content) = self.content.pop() {
            let (mut scope_at_decl, mixin_scope) = if content.declared_at_root {
                (mem::take(self.content_scopes), Scopes::new())
            } else {
                mem::take(self.scopes).split_off(content.scope_len)
            };

            let mut entered_scope = false;

            let call_args = if self.consume_char_if_exists('(') {
                self.parse_call_args()?
            } else {
                CallArgs::new(self.span_before)
            };

            if let Some(ref content_args) = content.content_args {
                call_args.max_args(content_args.len())?;

                let scope = self.eval_args(content_args.clone(), call_args)?;
                scope_at_decl.enter_scope(scope);
                entered_scope = true;
            } else {
                call_args.max_args(0)?;
            }

            let stmts = if let Some(body) = content.content.clone() {
                Parser {
                    toks: &mut body.into_iter().peekmore(),
                    map: self.map,
                    path: self.path,
                    scopes: &mut scope_at_decl,
                    global_scope: self.global_scope,
                    super_selectors: self.super_selectors,
                    span_before: self.span_before,
                    flags: self.flags,
                    content: self.content,
                    at_root: self.at_root,
                    at_root_has_selector: self.at_root_has_selector,
                    extender: self.extender,
                    content_scopes: self.scopes,
                    options: self.options,
                    modules: self.modules,
                    module_config: self.module_config,
                }
                .parse_stmt()?
            } else {
                Vec::new()
            };

            if entered_scope {
                scope_at_decl.exit_scope();
            }

            scope_at_decl.merge(mixin_scope);

            if content.declared_at_root {
                *self.content_scopes = scope_at_decl;
            } else {
                *self.scopes = scope_at_decl;
            }

            self.content.push(content);

            stmts
        } else {
            Vec::new()
        })
    }
}
