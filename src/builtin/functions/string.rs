use crate::builtin::builtin_imports::*;

pub(crate) fn to_upper_case(mut args: ArgumentResult, visitor: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    match args.get_err(0, "string")? {
        Value::String(mut i, q) => {
            i.make_ascii_uppercase();
            Ok(Value::String(i, q))
        }
        v => Err((
            format!("$string: {} is not a string.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn to_lower_case(mut args: ArgumentResult, visitor: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    match args.get_err(0, "string")? {
        Value::String(mut i, q) => {
            i.make_ascii_lowercase();
            Ok(Value::String(i, q))
        }
        v => Err((
            format!("$string: {} is not a string.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn str_length(mut args: ArgumentResult, visitor: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    match args.get_err(0, "string")? {
        Value::String(i, _) => Ok(Value::Dimension(SassNumber {
            num: (Number::from(i.chars().count())),
            unit: Unit::None,
            as_slash: None,
        })),
        v => Err((
            format!("$string: {} is not a string.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn quote(mut args: ArgumentResult, visitor: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    match args.get_err(0, "string")? {
        Value::String(i, _) => Ok(Value::String(i, QuoteKind::Quoted)),
        v => Err((
            format!("$string: {} is not a string.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn unquote(mut args: ArgumentResult, visitor: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    match args.get_err(0, "string")? {
        i @ Value::String(..) => Ok(i.unquote()),
        v => Err((
            format!("$string: {} is not a string.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn str_slice(mut args: ArgumentResult, visitor: &mut Visitor) -> SassResult<Value> {
    args.max_args(3)?;

    let span = args.span();

    let (string, quotes) = args
        .get_err(0, "string")?
        .assert_string_with_name("string", args.span())?;

    let str_len = string.chars().count();

    let start = args
        .get_err(1, "start-at")?
        .assert_number_with_name("start-at", span)?;
    start.assert_no_units("start-at", span)?;

    let start = start.num.assert_int(span)?;

    let start = if start == 0 {
        1
    } else if start > 0 {
        (start as usize).min(str_len + 1)
    } else {
        (start + str_len as i64 + 1).max(1) as usize
    };

    let end = args
        .default_arg(
            2,
            "end-at",
            Value::Dimension(SassNumber {
                num: Number(-1.0),
                unit: Unit::None,
                as_slash: None,
            }),
        )
        .assert_number_with_name("end-at", span)?;

    end.assert_no_units("end-at", span)?;

    let mut end = end.num.assert_int(span)?;

    if end < 0 {
        end += str_len as i64 + 1;
    }

    let end = (end.max(0) as usize).min(str_len + 1);

    if start > end || start > str_len {
        Ok(Value::String(String::new(), quotes))
    } else {
        Ok(Value::String(
            string
                .chars()
                .skip(start - 1)
                .take(end - start + 1)
                .collect(),
            quotes,
        ))
    }
}

pub(crate) fn str_index(mut args: ArgumentResult, visitor: &mut Visitor) -> SassResult<Value> {
    args.max_args(2)?;
    let s1 = args
        .get_err(0, "string")?
        .assert_string_with_name("string", args.span())?
        .0;

    let substr = args
        .get_err(1, "substring")?
        .assert_string_with_name("substring", args.span())?
        .0;

    let char_position = match s1.find(&substr) {
        Some(i) => s1[0..i].chars().count() + 1,
        None => return Ok(Value::Null),
    };

    Ok(Value::Dimension(SassNumber {
        num: Number::from(char_position),
        unit: Unit::None,
        as_slash: None,
    }))
}

pub(crate) fn str_insert(mut args: ArgumentResult, visitor: &mut Visitor) -> SassResult<Value> {
    args.max_args(3)?;
    let span = args.span();

    let (s1, quotes) = args
        .get_err(0, "string")?
        .assert_string_with_name("string", args.span())?;

    let substr = args
        .get_err(1, "insert")?
        .assert_string_with_name("insert", args.span())?
        .0;

    let index = args
        .get_err(2, "index")?
        .assert_number_with_name("index", span)?;
    index.assert_no_units("index", span)?;
    let index_int = index.num.assert_int_with_name("index", span)?;

    if s1.is_empty() {
        return Ok(Value::String(substr, quotes));
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
                    s2.to_owned() + &c.to_string()
                } else {
                    c.to_string()
                }
            })
            .collect::<String>()
    };

    let string = if index_int > 0 {
        insert((index_int as usize - 1).min(len), s1, &substr)
    } else if index_int == 0 {
        insert(0, s1, &substr)
    } else {
        let idx = (len as i64 + index_int + 1).max(0) as usize;
        insert(idx, s1, &substr)
    };

    Ok(Value::String(string, quotes))
}

#[cfg(feature = "random")]
#[allow(clippy::needless_pass_by_value)]
pub(crate) fn unique_id(args: ArgumentResult, _: &mut Visitor) -> SassResult<Value> {
    args.max_args(0)?;
    let mut rng = thread_rng();
    let string: String = std::iter::repeat(())
        .map(|()| rng.sample(Alphanumeric))
        .map(char::from)
        .take(12)
        .collect();
    Ok(Value::String(format!("id-{}", string), QuoteKind::None))
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
