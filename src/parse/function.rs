use codemap::Spanned;

use crate::{
    args::CallArgs,
    atrule::Function,
    common::{unvendor, Identifier},
    error::SassResult,
    lexer::Lexer,
    scope::Scopes,
    utils::{read_until_closing_curly_brace, read_until_semicolon_or_closing_curly_brace},
    value::{SassFunction, Value},
    Token,
};

use super::{common::ContextFlags, Parser, Stmt};

/// Names that functions are not allowed to have
const RESERVED_IDENTIFIERS: [&str; 7] =
    ["calc", "element", "expression", "url", "and", "or", "not"];

impl<'a> Parser<'a> {
    pub(super) fn parse_function(&mut self) -> SassResult<()> {
        self.whitespace_or_comment();
        let Spanned { node: name, span } = self.parse_identifier()?;

        if self.flags.in_mixin() {
            return Err(("Mixins may not contain function declarations.", span).into());
        }

        if self.flags.in_control_flow() {
            return Err(("Functions may not be declared in control directives.", span).into());
        }

        if self.flags.in_function() {
            return Err(("This at-rule is not allowed here.", self.span_before).into());
        }

        if RESERVED_IDENTIFIERS.contains(&unvendor(&name)) {
            return Err(("Invalid function name.", span).into());
        }

        self.whitespace_or_comment();
        self.expect_char('(')?;

        let args = self.parse_func_args()?;

        self.whitespace();

        let mut body = read_until_closing_curly_brace(self.toks)?;
        body.push(match self.toks.next() {
            Some(tok) => tok,
            None => return Err(("expected \"}\".", self.span_before).into()),
        });
        self.whitespace();

        let function = Function::new(args, body, self.at_root, span);

        let name_as_ident = Identifier::from(name);

        if self.at_root {
            self.global_scope.insert_fn(
                name_as_ident,
                SassFunction::UserDefined(Box::new(function), name_as_ident),
            );
        } else {
            self.scopes.insert_fn(
                name_as_ident,
                SassFunction::UserDefined(Box::new(function), name_as_ident),
            );
        }
        Ok(())
    }

    pub(super) fn parse_return(&mut self) -> SassResult<Box<Value>> {
        let toks = read_until_semicolon_or_closing_curly_brace(self.toks)?;
        let v = self.parse_value_from_vec(toks, true)?;
        if let Some(Token { kind: ';', .. }) = self.toks.peek() {
            self.toks.next();
        }
        Ok(Box::new(v.node))
    }

    pub fn eval_function(&mut self, function: Function, args: CallArgs) -> SassResult<Value> {
        let Function {
            body,
            args: fn_args,
            declared_at_root,
            ..
        } = function;

        let scope = self.eval_args(fn_args, args)?;

        let mut new_scope = Scopes::new();
        let mut entered_scope = false;
        if declared_at_root {
            new_scope.enter_scope(scope);
        } else {
            entered_scope = true;
            self.scopes.enter_scope(scope);
        };

        let mut return_value = Parser {
            toks: &mut Lexer::new(body),
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
            content: self.content,
            flags: self.flags | ContextFlags::IN_FUNCTION,
            at_root: false,
            at_root_has_selector: self.at_root_has_selector,
            extender: self.extender,
            content_scopes: self.content_scopes,
            options: self.options,
            modules: self.modules,
            module_config: self.module_config,
        }
        .parse_stmt()?;

        if entered_scope {
            self.scopes.exit_scope();
        }

        debug_assert!(return_value.len() <= 1);
        match return_value
            .pop()
            .ok_or(("Function finished without @return.", self.span_before))?
        {
            Stmt::Return(v) => Ok(*v),
            _ => todo!("should be unreachable"),
        }
    }
}
