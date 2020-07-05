use codemap::Spanned;
use peekmore::PeekMore;

use crate::{
    args::CallArgs,
    atrule::Function,
    common::unvendor,
    error::SassResult,
    utils::{read_until_closing_curly_brace, read_until_semicolon_or_closing_curly_brace},
    value::Value,
    Token,
};

use super::{Flags, NeverEmptyVec, Parser, Stmt};

/// Names that functions are not allowed to have
const FORBIDDEN_IDENTIFIERS: [&str; 7] =
    ["calc", "element", "expression", "url", "and", "or", "not"];

impl<'a> Parser<'a> {
    pub(super) fn parse_function(&mut self) -> SassResult<()> {
        self.whitespace_or_comment();
        let Spanned { node: name, span } = self.parse_identifier()?;

        if self.flags.contains(Flags::IN_MIXIN) {
            return Err(("Mixins may not contain function declarations.", span).into());
        }

        if self.flags.contains(Flags::IN_CONTROL_FLOW) {
            return Err(("Functions may not be declared in control directives.", span).into());
        }

        if FORBIDDEN_IDENTIFIERS.contains(&unvendor(&name)) {
            return Err(("Invalid function name.", span).into());
        }

        self.whitespace_or_comment();
        let args = match self.toks.next() {
            Some(Token { kind: '(', .. }) => self.parse_func_args()?,
            Some(Token { pos, .. }) => return Err(("expected \"(\".", pos).into()),
            None => return Err(("expected \"(\".", span).into()),
        };

        self.whitespace();

        let mut body = read_until_closing_curly_brace(self.toks)?;
        body.push(match self.toks.next() {
            Some(tok) => tok,
            None => return Err(("expected \"}\".", self.span_before).into()),
        });
        self.whitespace();

        let function = Function::new(self.scopes.last().clone(), args, body, span);

        if self.at_root {
            self.global_scope.insert_fn(name, function);
        } else {
            self.scopes.last_mut().insert_fn(name, function);
        }
        Ok(())
    }

    pub(super) fn parse_return(&mut self) -> SassResult<Box<Value>> {
        let toks = read_until_semicolon_or_closing_curly_brace(self.toks)?;
        let v = self.parse_value_from_vec(toks)?;
        if let Some(Token { kind: ';', .. }) = self.toks.peek() {
            self.toks.next();
        }
        Ok(Box::new(v.node))
    }

    pub fn eval_function(&mut self, function: Function, args: CallArgs) -> SassResult<Value> {
        let Function {
            mut scope,
            body,
            args: fn_args,
            ..
        } = function;

        self.eval_args(fn_args, args, &mut scope)?;

        let mut return_value = Parser {
            toks: &mut body.into_iter().peekmore(),
            map: self.map,
            path: self.path,
            scopes: &mut NeverEmptyVec::new(scope),
            global_scope: self.global_scope,
            super_selectors: self.super_selectors,
            span_before: self.span_before,
            content: self.content,
            flags: self.flags | Flags::IN_FUNCTION,
            at_root: false,
            at_root_has_selector: self.at_root_has_selector,
            extender: self.extender,
        }
        .parse()?;

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
