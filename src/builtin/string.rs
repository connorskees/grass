use super::{Builtin, GlobalFunctionMap};

use num_bigint::BigInt;
use num_traits::{Signed, ToPrimitive, Zero};

#[cfg(feature = "random")]
use rand::{distributions::Alphanumeric, thread_rng, Rng};

use crate::args::CallArgs;
use crate::common::QuoteKind;
use crate::error::SassResult;
use crate::scope::Scope;
use crate::selector::Selector;
use crate::unit::Unit;
use crate::value::{Number, Value};

fn to_upper_case(
    mut args: CallArgs,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<Value> {
    args.max_args(1)?;
    match arg!(args, scope, super_selector, 0, "string") {
        Value::Ident(mut i, q) => {
            i.make_ascii_uppercase();
            Ok(Value::Ident(i, q))
        }
        v => Err((
            format!(
                "$string: {} is not a string.",
                v.to_css_string(args.span())?
            ),
            args.span(),
        )
            .into()),
    }
}

fn to_lower_case(
    mut args: CallArgs,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<Value> {
    args.max_args(1)?;
    match arg!(args, scope, super_selector, 0, "string") {
        Value::Ident(mut i, q) => {
            i.make_ascii_lowercase();
            Ok(Value::Ident(i, q))
        }
        v => Err((
            format!(
                "$string: {} is not a string.",
                v.to_css_string(args.span())?
            ),
            args.span(),
        )
            .into()),
    }
}

fn str_length(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    args.max_args(1)?;
    match arg!(args, scope, super_selector, 0, "string") {
        Value::Ident(i, _) => Ok(Value::Dimension(
            Number::from(i.chars().count()),
            Unit::None,
        )),
        v => Err((
            format!(
                "$string: {} is not a string.",
                v.to_css_string(args.span())?
            ),
            args.span(),
        )
            .into()),
    }
}

fn quote(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    args.max_args(1)?;
    match arg!(args, scope, super_selector, 0, "string") {
        Value::Ident(i, _) => Ok(Value::Ident(i, QuoteKind::Quoted)),
        v => Err((
            format!(
                "$string: {} is not a string.",
                v.to_css_string(args.span())?
            ),
            args.span(),
        )
            .into()),
    }
}

fn unquote(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    args.max_args(1)?;
    match arg!(args, scope, super_selector, 0, "string") {
        i @ Value::Ident(..) => Ok(i.unquote()),
        v => Err((
            format!(
                "$string: {} is not a string.",
                v.to_css_string(args.span())?
            ),
            args.span(),
        )
            .into()),
    }
}

fn str_slice(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    args.max_args(3)?;
    let (string, quotes) = match arg!(args, scope, super_selector, 0, "string") {
        Value::Ident(s, q) => (s, q),
        v => {
            return Err((
                format!(
                    "$string: {} is not a string.",
                    v.to_css_string(args.span())?
                ),
                args.span(),
            )
                .into())
        }
    };
    let str_len = string.chars().count();
    let start = match arg!(args, scope, super_selector, 1, "start-at") {
        Value::Dimension(n, Unit::None) if n.is_decimal() => {
            return Err((format!("{} is not an int.", n), args.span()).into())
        }
        Value::Dimension(n, Unit::None) if n.is_positive() => {
            n.to_integer().to_usize().unwrap_or(str_len + 1)
        }
        Value::Dimension(n, Unit::None) if n.is_zero() => 1_usize,
        Value::Dimension(n, Unit::None) if n < -Number::from(str_len) => 1_usize,
        Value::Dimension(n, Unit::None) => (n.to_integer() + BigInt::from(str_len + 1))
            .to_usize()
            .unwrap(),
        v @ Value::Dimension(..) => {
            return Err((
                format!(
                    "$start: Expected {} to have no units.",
                    v.to_css_string(args.span())?
                ),
                args.span(),
            )
                .into())
        }
        v => {
            return Err((
                format!(
                    "$start-at: {} is not a number.",
                    v.to_css_string(args.span())?
                ),
                args.span(),
            )
                .into())
        }
    };
    let mut end = match arg!(args, scope, super_selector, 2, "end-at" = Value::Null) {
        Value::Dimension(n, Unit::None) if n.is_decimal() => {
            return Err((format!("{} is not an int.", n), args.span()).into())
        }
        Value::Dimension(n, Unit::None) if n.is_positive() => {
            n.to_integer().to_usize().unwrap_or(str_len + 1)
        }
        Value::Dimension(n, Unit::None) if n.is_zero() => 0_usize,
        Value::Dimension(n, Unit::None) if n < -Number::from(str_len) => 0_usize,
        Value::Dimension(n, Unit::None) => (n.to_integer() + BigInt::from(str_len + 1))
            .to_usize()
            .unwrap_or(str_len + 1),
        v @ Value::Dimension(..) => {
            return Err((
                format!(
                    "$end: Expected {} to have no units.",
                    v.to_css_string(args.span())?
                ),
                args.span(),
            )
                .into())
        }
        Value::Null => str_len,
        v => {
            return Err((
                format!(
                    "$end-at: {} is not a number.",
                    v.to_css_string(args.span())?
                ),
                args.span(),
            )
                .into())
        }
    };

    if end > str_len {
        end = str_len;
    }

    if start > end || start > str_len {
        Ok(Value::Ident(String::new(), quotes))
    } else {
        Ok(Value::Ident(
            string
                .chars()
                .skip(start - 1)
                .take(end - start + 1)
                .collect(),
            quotes,
        ))
    }
}

fn str_index(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    args.max_args(2)?;
    let s1 = match arg!(args, scope, super_selector, 0, "string") {
        Value::Ident(i, _) => i,
        v => {
            return Err((
                format!(
                    "$string: {} is not a string.",
                    v.to_css_string(args.span())?
                ),
                args.span(),
            )
                .into())
        }
    };

    let substr = match arg!(args, scope, super_selector, 1, "substring") {
        Value::Ident(i, _) => i,
        v => {
            return Err((
                format!(
                    "$substring: {} is not a string.",
                    v.to_css_string(args.span())?
                ),
                args.span(),
            )
                .into())
        }
    };

    Ok(match s1.find(&substr) {
        Some(v) => Value::Dimension(Number::from(v + 1), Unit::None),
        None => Value::Null,
    })
}

fn str_insert(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    args.max_args(3)?;
    let (s1, quotes) = match arg!(args, scope, super_selector, 0, "string") {
        Value::Ident(i, q) => (i, q),
        v => {
            return Err((
                format!(
                    "$string: {} is not a string.",
                    v.to_css_string(args.span())?
                ),
                args.span(),
            )
                .into())
        }
    };

    let substr = match arg!(args, scope, super_selector, 1, "insert") {
        Value::Ident(i, _) => i,
        v => {
            return Err((
                format!(
                    "$insert: {} is not a string.",
                    v.to_css_string(args.span())?
                ),
                args.span(),
            )
                .into())
        }
    };

    let index = match arg!(args, scope, super_selector, 2, "index") {
        Value::Dimension(n, Unit::None) if n.is_decimal() => {
            return Err((format!("$index: {} is not an int.", n), args.span()).into())
        }
        Value::Dimension(n, Unit::None) => n,
        v @ Value::Dimension(..) => {
            return Err((
                format!(
                    "$index: Expected {} to have no units.",
                    v.to_css_string(args.span())?
                ),
                args.span(),
            )
                .into())
        }
        v => {
            return Err((
                format!("$index: {} is not a number.", v.to_css_string(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    if s1.is_empty() {
        return Ok(Value::Ident(substr, quotes));
    }

    let len = s1.chars().count();

    // Insert substring at char position, rather than byte position
    let insert = |idx, s1: String, s2| {
        s1.chars()
            .enumerate()
            .map(|(i, c)| {
                if i + 1 == idx {
                    c.to_string() + s2
                } else if idx == 0 && i == 0 {
                    s2.to_string() + &c.to_string()
                } else {
                    c.to_string()
                }
            })
            .collect::<String>()
    };

    let string = if index.is_positive() {
        insert(
            index
                .to_integer()
                .to_usize()
                .unwrap_or(len + 1)
                .min(len + 1)
                - 1,
            s1,
            &substr,
        )
    } else if index.is_zero() {
        insert(0, s1, &substr)
    } else {
        let idx = index.abs().to_integer().to_usize().unwrap_or(len + 1);
        if idx > len {
            insert(0, s1, &substr)
        } else {
            insert(len - idx + 1, s1, &substr)
        }
    };

    Ok(Value::Ident(string, quotes))
}

#[cfg(feature = "random")]
fn unique_id(args: CallArgs, _: &Scope, _: &Selector) -> SassResult<Value> {
    args.max_args(0)?;
    let mut rng = thread_rng();
    let string = std::iter::repeat(())
        .map(|()| rng.sample(Alphanumeric))
        .take(7)
        .collect();
    Ok(Value::Ident(string, QuoteKind::None))
}

pub(crate) fn declare(f: &mut GlobalFunctionMap) {
    f.insert("to-upper-case", Builtin::new(to_upper_case));
    f.insert("to-lower-case", Builtin::new(to_lower_case));
    f.insert("str-length", Builtin::new(str_length));
    f.insert("quote", Builtin::new(quote));
    f.insert("unquote", Builtin::new(unquote));
    f.insert("str-slice", Builtin::new(str_slice));
    f.insert("str-index", Builtin::new(str_index));
    f.insert("str-insert", Builtin::new(str_insert));
    #[cfg(feature = "random")]
    f.insert("unique-id", Builtin::new(unique_id));
}
