use std::{
    collections::{BTreeMap, BTreeSet},
    iter::Iterator,
    mem,
};

use codemap::{Span, Spanned};

use crate::{
    common::{Identifier, ListSeparator},
    error::SassResult,
    utils::to_sentence,
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
        span: Span,
    ) -> SassResult<()> {
        let mut named_used = 0;

        for i in 0..self.args.len() {
            let argument = &self.args[i];

            if i < num_positional {
                if names.contains_key(&argument.name) {
                    // todo: _originalArgumentName
                    return Err((
                        format!(
                            "Argument ${} was passed both by position and by name.",
                            argument.name
                        ),
                        span,
                    )
                        .into());
                }
            } else if names.contains_key(&argument.name) {
                named_used += 1;
            } else if argument.default.is_none() {
                // todo: _originalArgumentName
                return Err((format!("Missing argument ${}.", argument.name), span).into());
            }
        }

        if self.rest.is_some() {
            return Ok(());
        }

        if num_positional > self.args.len() {
            return Err((
                format!(
                    "Only {} {}{} allowed, but {num_positional} {} passed.",
                    self.args.len(),
                    if names.is_empty() { "" } else { "positional " },
                    if self.args.len() == 1 {
                        "argument"
                    } else {
                        "arguments"
                    },
                    if num_positional == 1 { "was" } else { "were" }
                ),
                span,
            )
                .into());
        }

        if named_used < names.len() {
            let mut unknown_names = names.keys().copied().collect::<BTreeSet<_>>();

            for arg in &self.args {
                unknown_names.remove(&arg.name);
            }

            if unknown_names.len() == 1 {
                return Err((
                    format!(
                        "No argument named ${}.",
                        unknown_names.iter().next().unwrap()
                    ),
                    span,
                )
                    .into());
            }

            if unknown_names.len() > 1 {
                return Err((
                    format!(
                        "No arguments named {}.",
                        to_sentence(
                            unknown_names
                                .into_iter()
                                .map(|name| format!("${name}"))
                                .collect(),
                            "or"
                        )
                    ),
                    span,
                )
                    .into());
            }
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
    /// Get argument by name
    ///
    /// Removes the argument
    pub fn get_named<T: Into<Identifier>>(&mut self, val: T) -> Option<Spanned<Value>> {
        self.named.remove(&val.into()).map(|n| Spanned {
            node: n,
            span: self.span,
        })
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
    }

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
    }

    pub fn default_arg(&mut self, position: usize, name: &'static str, default: Value) -> Value {
        match self.get(position, name) {
            Some(val) => val.node,
            None => default,
        }
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

    pub fn get_variadic(self) -> SassResult<Vec<Spanned<Value>>> {
        if let Some((name, _)) = self.named.iter().next() {
            return Err((format!("No argument named ${}.", name), self.span).into());
        }

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
            .map(|(_, node)| Spanned {
                node,
                span,
            })
            .collect();

        Ok(args)
    }
}
