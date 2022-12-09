// use codemap::Spanned;

// use crate::{
//     args::CallArgs,
//     atrule::Function,
//     common::{unvendor, Identifier},
//     error::SassResult,
//     lexer::Lexer,
//     scope::Scopes,
//     utils::read_until_closing_curly_brace,
//     value::{SassFunction, Value},
// };

// use super::{common::ContextFlags, Parser, Stmt};

/// Names that functions are not allowed to have
pub(super) const RESERVED_IDENTIFIERS: [&str; 8] = [
    "calc",
    "element",
    "expression",
    "url",
    "and",
    "or",
    "not",
    "clamp",
];

// impl<'a, 'b> Parser<'a, 'b> {
// pub(super) fn parse_function(&mut self) -> SassResult<()> {
//     self.whitespace_or_comment();
//     let Spanned { node: name, span } = self.parse_identifier()?;

//     if self.flags.in_mixin() {
//         return Err(("Mixins may not contain function declarations.", span).into());
//     }

//     if self.flags.in_control_flow() {
//         return Err(("Functions may not be declared in control directives.", span).into());
//     }

//     if self.flags.in_function() {
//         return Err(("This at-rule is not allowed here.", self.span_before).into());
//     }

//     if RESERVED_IDENTIFIERS.contains(&unvendor(&name)) {
//         return Err(("Invalid function name.", span).into());
//     }

//     self.whitespace_or_comment();
//     self.expect_char('(')?;

//     let args = self.parse_func_args()?;

//     self.whitespace();

//     let mut body = read_until_closing_curly_brace(self.toks)?;
//     body.push(match self.toks.next() {
//         Some(tok) => tok,
//         None => return Err(("expected \"}\".", self.span_before).into()),
//     });
//     self.whitespace();

//     let function = Function::new(args, body, self.at_root, span);

//     let name_as_ident = Identifier::from(name);

//     // let sass_function = SassFunction::UserDefined {
//     //     function: Box::new(function),
//     //     name: name_as_ident,
//     // };

//     // if self.at_root {
//     //     self.global_scope.insert_fn(name_as_ident, sass_function);
//     // } else {
//     //     self.scopes.insert_fn(name_as_ident, sass_function);
//     // }

//     todo!()

//     // Ok(())
// }

// pub(super) fn parse_return(&mut self) -> SassResult<Box<Value>> {
//     let v = self.parse_value(true, &|_| false)?;

//     self.consume_char_if_exists(';');

//     Ok(Box::new(v.node))
// }

// pub fn eval_function(
//     &mut self,
//     function: Function,
//     args: CallArgs,
//     module: Option<Spanned<Identifier>>,
// ) -> SassResult<Value> {
//     let Function {
//         body,
//         args: fn_args,
//         declared_at_root,
//         ..
//     } = function;

//     let scope = self.eval_args(&fn_args, args)?;

//     let mut new_scope = Scopes::new();
//     let mut entered_scope = false;
//     if declared_at_root {
//         new_scope.enter_scope(scope);
//     } else {
//         entered_scope = true;
//         self.scopes.enter_scope(scope);
//     };

//     if let Some(module) = module {
//         let module = self.modules.get(module.node, module.span)?;

//         if declared_at_root {
//             new_scope.enter_scope(module.scope.clone());
//         } else {
//             self.scopes.enter_scope(module.scope.clone());
//         }
//     }

//     let mut return_value = Parser {
//         toks: &mut Lexer::new(body),
//         map: self.map,
//         path: self.path,
//         scopes: if declared_at_root {
//             &mut new_scope
//         } else {
//             self.scopes
//         },
//         global_scope: self.global_scope,
//         super_selectors: self.super_selectors,
//         span_before: self.span_before,
//         content: self.content,
//         flags: self.flags | ContextFlags::IN_FUNCTION,
//         at_root: false,
//         at_root_has_selector: self.at_root_has_selector,
//         extender: self.extender,
//         content_scopes: self.content_scopes,
//         options: self.options,
//         modules: self.modules,
//         module_config: self.module_config,
//     }
//     .parse_stmt()?;

//     if entered_scope {
//         self.scopes.exit_scope();
//     }

//     if module.is_some() {
//         self.scopes.exit_scope();
//     }

//     debug_assert!(
//         return_value.len() <= 1,
//         "we expect there to be only one return value"
//     );
//     match return_value
//         .pop()
//         .ok_or(("Function finished without @return.", self.span_before))?
//     {
//         Stmt::Return(v) => Ok(*v),
//         _ => todo!("should be unreachable"),
//     }
// }
// }
