use crate::builtin::builtin_imports::*;

pub(crate) fn length(mut args: ArgumentResult, visitor: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;

    let len = args.get_err(0, "list")?.as_list().len();

    Ok(Value::Dimension(SassNumber::new_unitless(len)))
}

pub(crate) fn nth(mut args: ArgumentResult, visitor: &mut Visitor) -> SassResult<Value> {
    args.max_args(2)?;
    let mut list = args.get_err(0, "list")?.as_list();
    let (n, unit) = match args.get_err(1, "n")? {
        Value::Dimension(SassNumber {
            num: n, unit: u, ..
        }) if n.is_nan() => {
            return Err((format!("$n: NaN{} is not an int.", u), args.span()).into())
        }
        Value::Dimension(SassNumber { num, unit, .. }) => (num, unit),
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
                n.inspect(),
                unit,
                list.len()
            ),
            args.span(),
        )
            .into());
    }

    Ok(list.remove(if n.is_positive() {
        let index = n.assert_int_with_name("n", args.span())? - 1;
        debug_assert!(index > -1);
        index as usize
    } else {
        list.len() - n.abs().assert_int_with_name("n", args.span())? as usize
    }))
}

pub(crate) fn list_separator(mut args: ArgumentResult, visitor: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    Ok(Value::String(
        match args.get_err(0, "list")? {
            Value::List(_, sep, ..) => sep.name(),
            Value::Map(..) | Value::ArgList(..) => ListSeparator::Comma.name(),
            _ => ListSeparator::Space.name(),
        }
        .to_owned(),
        QuoteKind::None,
    ))
}

pub(crate) fn set_nth(mut args: ArgumentResult, visitor: &mut Visitor) -> SassResult<Value> {
    args.max_args(3)?;
    let (mut list, sep, brackets) = match args.get_err(0, "list")? {
        Value::List(v, sep, b) => (v, sep, b),
        Value::ArgList(v) => (
            v.elems.into_iter().collect(),
            ListSeparator::Comma,
            Brackets::None,
        ),
        Value::Map(m) => (m.as_list(), ListSeparator::Comma, Brackets::None),
        v => (vec![v], ListSeparator::Undecided, Brackets::None),
    };
    let (n, unit) = match args.get_err(1, "n")? {
        Value::Dimension(SassNumber {
            num: n, unit: u, ..
        }) if n.is_nan() => {
            return Err((format!("$n: NaN{} is not an int.", u), args.span()).into())
        }
        Value::Dimension(SassNumber { num, unit, .. }) => (num, unit),
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
                n.inspect(),
                unit,
                len
            ),
            args.span(),
        )
            .into());
    }

    if n.is_decimal() {
        return Err((format!("$n: {} is not an int.", n.inspect()), args.span()).into());
    }

    let val = args.get_err(2, "value")?;

    if n.is_positive() {
        list[n.assert_int_with_name("n", args.span())? as usize - 1] = val;
    } else {
        list[len - n.abs().assert_int_with_name("n", args.span())? as usize] = val;
    }

    Ok(Value::List(list, sep, brackets))
}

pub(crate) fn append(mut args: ArgumentResult, visitor: &mut Visitor) -> SassResult<Value> {
    args.max_args(3)?;
    let (mut list, sep, brackets) = match args.get_err(0, "list")? {
        Value::List(v, sep, b) => (v, sep, b),
        v => (vec![v], ListSeparator::Undecided, Brackets::None),
    };
    let val = args.get_err(1, "val")?;
    let sep = match args.default_arg(
        2,
        "separator",
        Value::String("auto".to_owned(), QuoteKind::None),
    ) {
        Value::String(s, ..) => match s.as_str() {
            "auto" => {
                if sep == ListSeparator::Undecided {
                    ListSeparator::Space
                } else {
                    sep
                }
            }
            "comma" => ListSeparator::Comma,
            "space" => ListSeparator::Space,
            "slash" => ListSeparator::Slash,
            _ => {
                return Err((
                    "$separator: Must be \"space\", \"comma\", \"slash\", or \"auto\".",
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

pub(crate) fn join(mut args: ArgumentResult, visitor: &mut Visitor) -> SassResult<Value> {
    args.max_args(4)?;
    let (mut list1, sep1, brackets) = match args.get_err(0, "list1")? {
        Value::List(v, sep, brackets) => (v, sep, brackets),
        Value::Map(m) => (m.as_list(), ListSeparator::Comma, Brackets::None),
        v => (vec![v], ListSeparator::Undecided, Brackets::None),
    };
    let (list2, sep2) = match args.get_err(1, "list2")? {
        Value::List(v, sep, ..) => (v, sep),
        Value::Map(m) => (m.as_list(), ListSeparator::Comma),
        v => (vec![v], ListSeparator::Undecided),
    };
    let sep = match args.default_arg(
        2,
        "separator",
        Value::String("auto".to_owned(), QuoteKind::None),
    ) {
        Value::String(s, ..) => match s.as_str() {
            "auto" => {
                if sep1 != ListSeparator::Undecided {
                    sep1
                } else if sep2 != ListSeparator::Undecided {
                    sep2
                } else {
                    ListSeparator::Space
                }
            }
            "comma" => ListSeparator::Comma,
            "space" => ListSeparator::Space,
            "slash" => ListSeparator::Slash,
            _ => {
                return Err((
                    "$separator: Must be \"space\", \"comma\", \"slash\", or \"auto\".",
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
    ) {
        Value::String(s, ..) => match s.as_str() {
            "auto" => brackets,
            _ => Brackets::Bracketed,
        },
        v => {
            if v.is_truthy() {
                Brackets::Bracketed
            } else {
                Brackets::None
            }
        }
    };

    list1.extend(list2);

    Ok(Value::List(list1, sep, brackets))
}

pub(crate) fn is_bracketed(mut args: ArgumentResult, visitor: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    Ok(Value::bool(match args.get_err(0, "list")? {
        Value::List(.., brackets) => match brackets {
            Brackets::Bracketed => true,
            Brackets::None => false,
        },
        _ => false,
    }))
}

pub(crate) fn index(mut args: ArgumentResult, visitor: &mut Visitor) -> SassResult<Value> {
    args.max_args(2)?;
    let list = args.get_err(0, "list")?.as_list();
    let value = args.get_err(1, "value")?;
    let index = match list.into_iter().position(|v| v == value) {
        Some(v) => v + 1,
        None => return Ok(Value::Null),
    };
    Ok(Value::Dimension(SassNumber::new_unitless(index)))
}

pub(crate) fn zip(args: ArgumentResult, visitor: &mut Visitor) -> SassResult<Value> {
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
