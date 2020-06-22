use std::mem;

use codemap::Spanned;
use peekmore::PeekMore;

use crate::{
    args::CallArgs,
    atrule::Function,
    error::SassResult,
    utils::{read_until_closing_curly_brace, read_until_semicolon_or_closing_curly_brace},
    value::Value,
    Token,
};

use super::{NeverEmptyVec, Parser, Stmt};

/// Names that functions are not allowed to have
const FORBIDDEN_IDENTIFIERS: [&str; 7] =
    ["calc", "element", "expression", "url", "and", "or", "not"];

fn unvendor(name: &str) -> &str {
    let mut chars = name.chars();
    if !matches!(chars.next(), Some('-')) {
        return name;
    }
    if matches!(chars.next(), Some('-')) {
        return name;
    }
    if name.chars().count() < 2 {
        return name;
    }
    let mut pos = 2;
    for c in chars {
        if c == '-' {
            return &name[pos..];
        }
        pos += 1;
    }
    name
}

impl<'a> Parser<'a> {
    pub(super) fn parse_function(&mut self) -> SassResult<()> {
        self.whitespace_or_comment();
        let Spanned { node: name, span } = self.parse_identifier()?;

        if self.in_mixin {
            return Err(("Mixins may not contain function declarations.", span).into());
        }

        if self.in_control_flow {
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

    pub(super) fn parse_return(&mut self) -> SassResult<Value> {
        let toks = read_until_semicolon_or_closing_curly_brace(self.toks)?;
        let v = self.parse_value_from_vec(toks)?;
        if let Some(Token { kind: ';', .. }) = self.toks.peek() {
            self.toks.next();
        }
        Ok(v.node)
    }

    pub fn eval_function(&mut self, mut function: Function, args: CallArgs) -> SassResult<Value> {
        self.eval_fn_args(&mut function, args)?;

        let mut return_value = Parser {
            toks: &mut function.body.into_iter().peekmore(),
            map: self.map,
            path: self.path,
            scopes: &mut NeverEmptyVec::new(function.scope),
            global_scope: self.global_scope,
            super_selectors: self.super_selectors,
            span_before: self.span_before,
            content: self.content.clone(),
            in_mixin: self.in_mixin,
            in_function: true,
            in_control_flow: self.in_control_flow,
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
            Stmt::Return(v) => Ok(v),
            _ => todo!("should be unreachable"),
        }
    }

    fn eval_fn_args(&mut self, function: &mut Function, mut args: CallArgs) -> SassResult<()> {
        self.scopes.push(self.scopes.last().clone());
        for (idx, arg) in function.args.0.iter_mut().enumerate() {
            if arg.is_variadic {
                let span = args.span();
                let arg_list = Value::ArgList(self.variadic_args(args)?);
                function.scope.insert_var(
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
            function.scope.insert_var(mem::take(&mut arg.name), val)?;
        }
        self.scopes.pop();
        Ok(())
    }
}
