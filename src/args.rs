use std::collections::HashMap;

use codemap::{Span, Spanned};

use crate::{
    common::Identifier,
    error::SassResult,
    value::Value,
    {Cow, Token},
};

#[derive(Debug, Clone)]
pub(crate) struct FuncArgs(pub Vec<FuncArg>);

#[derive(Debug, Clone)]
pub(crate) struct FuncArg {
    pub name: Identifier,
    pub default: Option<Vec<Token>>,
    pub is_variadic: bool,
}

impl FuncArgs {
    pub const fn new() -> Self {
        FuncArgs(Vec::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[allow(dead_code)]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct CallArgs(pub HashMap<CallArg, SassResult<Spanned<Value>>>, pub Span);

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub(crate) enum CallArg {
    Named(Identifier),
    Positional(usize),
}

impl CallArg {
    pub fn position(&self) -> Result<usize, String> {
        match self {
            Self::Named(ref name) => Err(name.to_string()),
            Self::Positional(p) => Ok(*p),
        }
    }

    pub fn decrement(self) -> CallArg {
        match self {
            Self::Named(..) => self,
            Self::Positional(p) => Self::Positional(p - 1),
        }
    }
}

impl CallArgs {
    pub fn new(span: Span) -> Self {
        CallArgs(HashMap::new(), span)
    }

    pub fn to_css_string(self) -> SassResult<Spanned<String>> {
        let mut string = String::with_capacity(2 + self.len() * 10);
        string.push('(');
        let mut span = self.1;

        if self.is_empty() {
            return Ok(Spanned {
                node: "()".to_owned(),
                span,
            });
        }

        let args = match self.get_variadic() {
            Ok(v) => v,
            Err(..) => {
                return Err(("Plain CSS functions don't support keyword arguments.", span).into())
            }
        };

        string.push_str(
            &args
                .iter()
                .map(|a| {
                    span = span.merge(a.span);
                    a.node.to_css_string(a.span)
                })
                .collect::<SassResult<Vec<Cow<'static, str>>>>()?
                .join(", "),
        );
        string.push(')');
        Ok(Spanned { node: string, span })
    }

    /// Get argument by name
    ///
    /// Removes the argument
    pub fn get_named<T: Into<Identifier>>(&mut self, val: T) -> Option<SassResult<Spanned<Value>>> {
        self.0.remove(&CallArg::Named(val.into()))
    }

    /// Get a positional argument by 0-indexed position
    ///
    /// Removes the argument
    pub fn get_positional(&mut self, val: usize) -> Option<SassResult<Spanned<Value>>> {
        self.0.remove(&CallArg::Positional(val))
    }

    pub fn get<T: Into<Identifier>>(
        &mut self,
        position: usize,
        name: T,
    ) -> Option<SassResult<Spanned<Value>>> {
        match self.get_named(name) {
            Some(v) => Some(v),
            None => self.get_positional(position),
        }
    }

    pub fn get_err(&mut self, position: usize, name: &'static str) -> SassResult<Value> {
        match self.get_named(name) {
            Some(v) => Ok(v?.node),
            None => match self.get_positional(position) {
                Some(v) => Ok(v?.node),
                None => Err((format!("Missing argument ${}.", name), self.span()).into()),
            },
        }
    }

    /// Decrement all positional arguments by 1
    ///
    /// This is used by builtin function `call` to pass
    /// positional arguments to the other function
    pub fn decrement(self) -> Self {
        CallArgs(
            self.0
                .into_iter()
                .map(|(k, v)| (k.decrement(), v))
                .collect(),
            self.1,
        )
    }

    pub const fn span(&self) -> Span {
        self.1
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
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

    pub fn default_arg(
        &mut self,
        position: usize,
        name: &'static str,
        default: Value,
    ) -> SassResult<Value> {
        Ok(match self.get(position, name) {
            Some(val) => val?.node,
            None => default,
        })
    }

    pub fn positional_arg(&mut self, position: usize) -> Option<SassResult<Spanned<Value>>> {
        self.get_positional(position)
    }

    #[allow(dead_code, clippy::unused_self)]
    fn named_arg(&mut self, name: &'static str) -> Option<SassResult<Spanned<Value>>> {
        self.get_named(name)
    }

    pub fn default_named_arg(&mut self, name: &'static str, default: Value) -> SassResult<Value> {
        Ok(match self.get_named(name) {
            Some(val) => val?.node,
            None => default,
        })
    }

    pub fn get_variadic(self) -> SassResult<Vec<Spanned<Value>>> {
        let mut vals = Vec::new();
        let mut args = match self
            .0
            .into_iter()
            .map(|(a, v)| Ok((a.position()?, v)))
            .collect::<Result<Vec<(usize, SassResult<Spanned<Value>>)>, String>>()
        {
            Ok(v) => v,
            Err(e) => return Err((format!("No argument named ${}.", e), self.1).into()),
        };

        args.sort_by(|(a1, _), (a2, _)| a1.cmp(a2));

        for (_, arg) in args {
            vals.push(arg?);
        }

        Ok(vals)
    }
}
