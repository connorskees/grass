use std::{
    collections::{BTreeMap, BTreeSet},
    iter::Iterator,
    mem,
};

use codemap::{Span, Spanned};

use crate::{
    common::{Identifier, ListSeparator},
    error::SassResult,
    value::Value,
};

use super::AstExpr;

#[derive(Debug, Clone)]
pub(crate) struct Argument {
    pub name: Identifier,
    pub default: Option<AstExpr>,
}

#[derive(Debug, Clone)]
pub(crate) struct ArgumentDeclaration {
    pub args: Vec<Argument>,
    pub rest: Option<Identifier>,
}

impl ArgumentDeclaration {
    pub fn empty() -> Self {
        Self {
            args: Vec::new(),
            rest: None,
        }
    }

    pub fn verify<T>(
        &self,
        num_positional: usize,
        names: &BTreeMap<Identifier, T>,
    ) -> SassResult<()> {
        let mut named_used = 0;

        for i in 0..self.args.len() {
            let argument = &self.args[i];

            if i < num_positional {
                if names.contains_key(&argument.name) {
                    todo!("Argument ${{_originalArgumentName(argument.name)}} was passed both by position and by name.")
                }
            } else if names.contains_key(&argument.name) {
                named_used += 1;
            } else if argument.default.is_none() {
                todo!("Missing argument ${{_originalArgumentName(argument.name)}}.")
            }
        }

        if self.rest.is_some() {
            return Ok(());
        }

        if num_positional > self.args.len() {
            todo!("Only ${{arguments.length}} ${{names.isEmpty ? '' : 'positional '}}${{pluralize('argument', arguments.length)}} allowed, but $positional ${{pluralize('was', positional, plural: 'were')}} passed.")
        }

        if named_used < names.len() {
            todo!()
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ArgumentInvocation {
    pub positional: Vec<AstExpr>,
    pub named: BTreeMap<Identifier, AstExpr>,
    pub rest: Option<AstExpr>,
    pub keyword_rest: Option<AstExpr>,
    pub span: Span,
}

impl ArgumentInvocation {
    pub fn empty(span: Span) -> Self {
        Self {
            positional: Vec::new(),
            named: BTreeMap::new(),
            rest: None,
            keyword_rest: None,
            span,
        }
    }
}

// todo: hack for builtin `call`
#[derive(Debug, Clone)]
pub(crate) enum MaybeEvaledArguments {
    Invocation(ArgumentInvocation),
    Evaled(ArgumentResult),
}

#[derive(Debug, Clone)]
pub(crate) struct ArgumentResult {
    pub positional: Vec<Value>,
    pub named: BTreeMap<Identifier, Value>,
    pub separator: ListSeparator,
    pub span: Span,
    // todo: hack
    pub touched: BTreeSet<usize>,
}

impl ArgumentResult {
    // pub fn new(span: Span) -> Self {
    //     // CallArgs(HashMap::new(), span)
    //     todo!()
    // }

    // pub fn to_css_string(self, is_compressed: bool) -> SassResult<Spanned<String>> {
    // let mut string = String::with_capacity(2 + self.len() * 10);
    // string.push('(');
    // let mut span = self.1;

    // if self.is_empty() {
    //     return Ok(Spanned {
    //         node: "()".to_owned(),
    //         span,
    //     });
    // }

    // let args = match self.get_variadic() {
    //     Ok(v) => v,
    //     Err(..) => {
    //         return Err(("Plain CSS functions don't support keyword arguments.", span).into())
    //     }
    // };

    // string.push_str(
    //     &args
    //         .iter()
    //         .map(|a| {
    //             span = span.merge(a.span);
    //             a.node.to_css_string(a.span, is_compressed)
    //         })
    //         .collect::<SassResult<Vec<Cow<'static, str>>>>()?
    //         .join(", "),
    // );
    // string.push(')');
    // Ok(Spanned { node: string, span })
    // todo!()
    // }

    /// Get argument by name
    ///
    /// Removes the argument
    pub fn get_named<T: Into<Identifier>>(&mut self, val: T) -> Option<Spanned<Value>> {
        self.named.remove(&val.into()).map(|n| Spanned {
            node: n,
            span: self.span,
        })
        // self.0.remove(&CallArg::Named(val.into()))
        // todo!()
    }

    /// Get a positional argument by 0-indexed position
    ///
    /// Replaces argument with `Value::Null` gravestone
    pub fn get_positional(&mut self, idx: usize) -> Option<Spanned<Value>> {
        let val = match self.positional.get_mut(idx) {
            Some(v) => Some(Spanned {
                node: mem::replace(v, Value::Null),
                span: self.span,
            }),
            None => None,
        };

        self.touched.insert(idx);
        val
        // self.0.remove(&CallArg::Positional(val))
        // todo!()
    }

    pub fn get<T: Into<Identifier>>(&mut self, position: usize, name: T) -> Option<Spanned<Value>> {
        match self.get_named(name) {
            Some(v) => Some(v),
            None => self.get_positional(position),
        }
    }

    pub fn get_err(&mut self, position: usize, name: &'static str) -> SassResult<Value> {
        match self.get_named(name) {
            Some(v) => Ok(v.node),
            None => match self.get_positional(position) {
                Some(v) => Ok(v.node),
                None => Err((format!("Missing argument ${}.", name), self.span()).into()),
            },
        }
        // todo!()
    }

    // / Decrement all positional arguments by 1
    // /
    // / This is used by builtin function `call` to pass
    // / positional arguments to the other function
    // pub fn decrement(self) -> CallArgs {
    //     // CallArgs(
    //     //     self.0
    //     //         .into_iter()
    //     //         .map(|(k, v)| (k.decrement(), v))
    //     //         .collect(),
    //     //     self.1,
    //     // )
    //     todo!()
    // }

    pub const fn span(&self) -> Span {
        self.span
    }

    pub fn len(&self) -> usize {
        self.positional.len() + self.named.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn min_args(&self, min: usize) -> SassResult<()> {
        let len = self.len();
        if len < min {
            if min == 1 {
                return Err(("At least one argument must be passed.", self.span()).into());
            }
            todo!("min args greater than one")
        }
        Ok(())
    }

    pub fn max_args(&self, max: usize) -> SassResult<()> {
        let len = self.len();
        if len > max {
            let mut err = String::with_capacity(50);
            #[allow(clippy::format_push_string)]
            err.push_str(&format!("Only {} argument", max));
            if max != 1 {
                err.push('s');
            }
            err.push_str(" allowed, but ");
            err.push_str(&len.to_string());
            err.push(' ');
            if len == 1 {
                err.push_str("was passed.");
            } else {
                err.push_str("were passed.");
            }
            return Err((err, self.span()).into());
        }
        Ok(())
        // todo!()
    }

    pub fn default_arg(&mut self, position: usize, name: &'static str, default: Value) -> Value {
        match self.get(position, name) {
            Some(val) => val.node,
            None => default,
        }
    }

    pub fn positional_arg(&mut self, position: usize) -> Option<Spanned<Value>> {
        self.get_positional(position)
    }

    pub fn remove_positional(&mut self, position: usize) -> Option<Value> {
        if self.positional.len() > position {
            Some(self.positional.remove(position))
        } else {
            None
        }
    }

    pub fn default_named_arg(&mut self, name: &'static str, default: Value) -> Value {
        match self.get_named(name) {
            Some(val) => val.node,
            None => default,
        }
    }

    // args: ArgumentDeclaration
    pub fn get_variadic(self) -> SassResult<Vec<Spanned<Value>>> {
        // todo: i think we do give a proper error here
        assert!(self.named.is_empty());

        let Self {
            positional,
            span,
            touched,
            ..
        } = self;

        // todo: complete hack, we shouldn't have the `touched` set
        let args = positional
            .into_iter()
            .enumerate()
            .filter(|(idx, _)| !touched.contains(idx))
            .map(|(_, a)| Spanned {
                node: a,
                span: span,
            })
            .collect();

        // let mut vals = Vec::new();
        // let mut args = match self
        //     .0
        //     .into_iter()
        //     .map(|(a, v)| Ok((a.position()?, v)))
        //     .collect::<Result<Vec<(usize, SassResult<Spanned<Value>>)>, String>>()
        // {
        //     Ok(v) => v,
        //     Err(e) => return Err((format!("No argument named ${}.", e), self.1).into()),
        // };

        // args.sort_by(|(a1, _), (a2, _)| a1.cmp(a2));

        // for (_, arg) in args {
        //     vals.push(arg?);
        // }

        // Ok(vals)
        // todo!()

        Ok(args)
        // Ok(args
        //     .into_iter()
        //     .map(|a| Spanned { node: a, span })
        //     .collect())
    }
}
