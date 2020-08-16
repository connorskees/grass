use super::{Builtin, GlobalFunctionMap};

use num_traits::{Signed, ToPrimitive, Zero};

use crate::{
    args::CallArgs,
    common::{Brackets, ListSeparator, QuoteKind},
    error::SassResult,
    parse::Parser,
    unit::Unit,
    value::{Number, Value},
};

pub(crate) fn length(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    Ok(Value::Dimension(
        Some(Number::from(args.get_err(0, "list")?.as_list().len())),
        Unit::None,
        true,
    ))
}

pub(crate) fn nth(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(2)?;
    let mut list = args.get_err(0, "list")?.as_list();
    let (n, unit) = match args.get_err(1, "n")? {
        Value::Dimension(Some(num), unit, ..) => (num, unit),
        Value::Dimension(None, u, ..) => {
            return Err((format!("$n: NaN{} is not an int.", u), args.span()).into())
        }
        v => {
            return Err((
                format!("$n: {} is not a number.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    if n.is_zero() {
        return Err(("$n: List index may not be 0.", args.span()).into());
    }

    if n.abs() > Number::from(list.len()) {
        return Err((
            format!(
                "$n: Invalid index {}{} for a list with {} elements.",
                n,
                unit,
                list.len()
            ),
            args.span(),
        )
            .into());
    }

    if n.is_decimal() {
        return Err((format!("$n: {} is not an int.", n), args.span()).into());
    }

    Ok(list.remove(if n.is_positive() {
        n.to_integer().to_usize().unwrap_or(std::usize::MAX) - 1
    } else {
        list.len() - n.abs().to_integer().to_usize().unwrap_or(std::usize::MAX)
    }))
}

pub(crate) fn list_separator(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    Ok(Value::String(
        match args.get_err(0, "list")? {
            Value::List(_, sep, ..) => sep.name(),
            _ => ListSeparator::Space.name(),
        }
        .to_owned(),
        QuoteKind::None,
    ))
}

pub(crate) fn set_nth(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(3)?;
    let (mut list, sep, brackets) = match args.get_err(0, "list")? {
        Value::List(v, sep, b) => (v, sep, b),
        Value::Map(m) => (m.as_list(), ListSeparator::Comma, Brackets::None),
        v => (vec![v], ListSeparator::Space, Brackets::None),
    };
    let (n, unit) = match args.get_err(1, "n")? {
        Value::Dimension(Some(num), unit, ..) => (num, unit),
        Value::Dimension(None, u, ..) => {
            return Err((format!("$n: NaN{} is not an int.", u), args.span()).into())
        }
        v => {
            return Err((
                format!("$n: {} is not a number.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    if n.is_zero() {
        return Err(("$n: List index may not be 0.", args.span()).into());
    }

    let len = list.len();

    if n.abs() > Number::from(len) {
        return Err((
            format!(
                "$n: Invalid index {}{} for a list with {} elements.",
                n, unit, len
            ),
            args.span(),
        )
            .into());
    }

    if n.is_decimal() {
        return Err((format!("$n: {} is not an int.", n), args.span()).into());
    }

    let val = args.get_err(2, "value")?;

    if n.is_positive() {
        list[n.to_integer().to_usize().unwrap_or(std::usize::MAX) - 1] = val;
    } else {
        list[len - n.abs().to_integer().to_usize().unwrap_or(std::usize::MAX)] = val;
    }

    Ok(Value::List(list, sep, brackets))
}

pub(crate) fn append(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(3)?;
    let (mut list, sep, brackets) = match args.get_err(0, "list")? {
        Value::List(v, sep, b) => (v, sep, b),
        v => (vec![v], ListSeparator::Space, Brackets::None),
    };
    let val = args.get_err(1, "val")?;
    let sep = match args.default_arg(
        2,
        "separator",
        Value::String("auto".to_owned(), QuoteKind::None),
    )? {
        Value::String(s, ..) => match s.as_str() {
            "auto" => sep,
            "comma" => ListSeparator::Comma,
            "space" => ListSeparator::Space,
            _ => {
                return Err((
                    "$separator: Must be \"space\", \"comma\", or \"auto\".",
                    args.span(),
                )
                    .into())
            }
        },
        v => {
            return Err((
                format!("$separator: {} is not a string.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    list.push(val);

    Ok(Value::List(list, sep, brackets))
}

pub(crate) fn join(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(4)?;
    let (mut list1, sep1, brackets) = match args.get_err(0, "list1")? {
        Value::List(v, sep, brackets) => (v, sep, brackets),
        Value::Map(m) => (m.as_list(), ListSeparator::Comma, Brackets::None),
        v => (vec![v], ListSeparator::Space, Brackets::None),
    };
    let (list2, sep2) = match args.get_err(1, "list2")? {
        Value::List(v, sep, ..) => (v, sep),
        Value::Map(m) => (m.as_list(), ListSeparator::Comma),
        v => (vec![v], ListSeparator::Space),
    };
    let sep = match args.default_arg(
        2,
        "separator",
        Value::String("auto".to_owned(), QuoteKind::None),
    )? {
        Value::String(s, ..) => match s.as_str() {
            "auto" => {
                if list1.is_empty() || (list1.len() == 1 && sep1 == ListSeparator::Space) {
                    sep2
                } else {
                    sep1
                }
            }
            "comma" => ListSeparator::Comma,
            "space" => ListSeparator::Space,
            _ => {
                return Err((
                    "$separator: Must be \"space\", \"comma\", or \"auto\".",
                    args.span(),
                )
                    .into())
            }
        },
        v => {
            return Err((
                format!("$separator: {} is not a string.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    let brackets = match args.default_arg(
        3,
        "bracketed",
        Value::String("auto".to_owned(), QuoteKind::None),
    )? {
        Value::String(s, ..) => match s.as_str() {
            "auto" => brackets,
            _ => Brackets::Bracketed,
        },
        v => {
            if v.is_true() {
                Brackets::Bracketed
            } else {
                Brackets::None
            }
        }
    };

    list1.extend(list2);

    Ok(Value::List(list1, sep, brackets))
}

pub(crate) fn is_bracketed(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    Ok(Value::bool(match args.get_err(0, "list")? {
        Value::List(.., brackets) => match brackets {
            Brackets::Bracketed => true,
            Brackets::None => false,
        },
        _ => false,
    }))
}

pub(crate) fn index(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(2)?;
    let list = args.get_err(0, "list")?.as_list();
    let value = args.get_err(1, "value")?;
    let index = match list.into_iter().position(|v| v == value) {
        Some(v) => Number::from(v + 1),
        None => return Ok(Value::Null),
    };
    Ok(Value::Dimension(Some(index), Unit::None, true))
}

pub(crate) fn zip(args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    let lists = args
        .get_variadic()?
        .into_iter()
        .map(|x| x.node.as_list())
        .collect::<Vec<Vec<Value>>>();

    let len = lists.iter().map(Vec::len).min().unwrap_or(0);

    if len == 0 {
        return Ok(Value::List(
            Vec::new(),
            ListSeparator::Comma,
            Brackets::None,
        ));
    }

    let result = (0..len)
        .map(|i| {
            let items = lists.iter().map(|v| v[i].clone()).collect();
            Value::List(items, ListSeparator::Space, Brackets::None)
        })
        .collect();

    Ok(Value::List(result, ListSeparator::Comma, Brackets::None))
}

pub(crate) fn declare(f: &mut GlobalFunctionMap) {
    f.insert("length", Builtin::new(length));
    f.insert("nth", Builtin::new(nth));
    f.insert("list-separator", Builtin::new(list_separator));
    f.insert("set-nth", Builtin::new(set_nth));
    f.insert("append", Builtin::new(append));
    f.insert("join", Builtin::new(join));
    f.insert("is-bracketed", Builtin::new(is_bracketed));
    f.insert("index", Builtin::new(index));
    f.insert("zip", Builtin::new(zip));
}
