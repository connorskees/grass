use std::mem;

use super::{Builtin, GlobalFunctionMap};

use crate::{
    args::CallArgs,
    common::{Brackets, ListSeparator},
    error::SassResult,
    parse::Parser,
    value::{SassMap, Value},
};

pub(crate) fn map_get(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(2)?;
    let key = args.get_err(1, "key")?;
    let map = match args.get_err(0, "map")? {
        Value::Map(m) => m,
        Value::List(v, ..) if v.is_empty() => SassMap::new(),
        Value::ArgList(v) if v.is_empty() => SassMap::new(),
        v => {
            return Err((
                format!("$map: {} is not a map.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    Ok(map.get(&key).unwrap_or(Value::Null))
}

pub(crate) fn map_has_key(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(2)?;
    let key = args.get_err(1, "key")?;
    let map = match args.get_err(0, "map")? {
        Value::Map(m) => m,
        Value::List(v, ..) if v.is_empty() => SassMap::new(),
        Value::ArgList(v) if v.is_empty() => SassMap::new(),
        v => {
            return Err((
                format!("$map: {} is not a map.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::bool(map.get(&key).is_some()))
}

pub(crate) fn map_keys(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    let map = match args.get_err(0, "map")? {
        Value::Map(m) => m,
        Value::List(v, ..) if v.is_empty() => SassMap::new(),
        Value::ArgList(v) if v.is_empty() => SassMap::new(),
        v => {
            return Err((
                format!("$map: {} is not a map.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::List(
        map.keys(),
        ListSeparator::Comma,
        Brackets::None,
    ))
}

pub(crate) fn map_values(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    let map = match args.get_err(0, "map")? {
        Value::Map(m) => m,
        Value::List(v, ..) if v.is_empty() => SassMap::new(),
        Value::ArgList(v) if v.is_empty() => SassMap::new(),
        v => {
            return Err((
                format!("$map: {} is not a map.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::List(
        map.values(),
        ListSeparator::Comma,
        Brackets::None,
    ))
}

pub(crate) fn map_merge(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    if args.len() == 1 {
        return Err(("Expected $args to contain a key.", args.span()).into());
    }

    let last_position = args.len().saturating_sub(1);

    let mut map1 = match args.get_err(0, "map1")? {
        Value::Map(m) => m,
        Value::List(v, ..) if v.is_empty() => SassMap::new(),
        Value::ArgList(v) if v.is_empty() => SassMap::new(),
        v => {
            return Err((
                format!("$map1: {} is not a map.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    let mut map2 = match args.get_err(last_position, "map2")? {
        Value::Map(m) => m,
        Value::List(v, ..) if v.is_empty() => SassMap::new(),
        Value::ArgList(v) if v.is_empty() => SassMap::new(),
        v => {
            return Err((
                format!("$map2: {} is not a map.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    let mut keys = args.get_variadic()?;

    if keys.is_empty() {
        map1.merge(map2);
    } else {
        while let Some(key) = keys.pop() {
            let mut new_map = SassMap::new();
            new_map.insert(key.node, Value::Map(mem::take(&mut map2)));
            map2 = new_map;
        }

        for (key, value) in map2 {
            // if they are two maps sharing a key, merge the keys
            if let (Some(Value::Map(map1)), Value::Map(map2)) = (map1.get_mut(&key), &value) {
                map1.merge(map2.clone());
            } else {
                map1.insert(key, value);
            }
        }
    }

    Ok(Value::Map(map1))
}

pub(crate) fn map_remove(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    let mut map = match args.get_err(0, "map")? {
        Value::Map(m) => m,
        Value::List(v, ..) if v.is_empty() => SassMap::new(),
        Value::ArgList(v) if v.is_empty() => SassMap::new(),
        v => {
            return Err((
                format!("$map: {} is not a map.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    let keys = args.get_variadic()?;
    for key in keys {
        map.remove(&key);
    }
    Ok(Value::Map(map))
}

pub(crate) fn declare(f: &mut GlobalFunctionMap) {
    f.insert("map-get", Builtin::new(map_get));
    f.insert("map-has-key", Builtin::new(map_has_key));
    f.insert("map-keys", Builtin::new(map_keys));
    f.insert("map-values", Builtin::new(map_values));
    f.insert("map-merge", Builtin::new(map_merge));
    f.insert("map-remove", Builtin::new(map_remove));
}
