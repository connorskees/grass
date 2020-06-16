use super::{Builtin, GlobalFunctionMap};

use crate::args::CallArgs;
use crate::common::{Brackets, ListSeparator};
use crate::error::SassResult;
use crate::parse::Parser;
use crate::value::{SassMap, Value};

fn map_get(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(2)?;
    let key = parser.arg(&mut args, 1, "key")?;
    let map = match parser.arg(&mut args, 0, "map")? {
        Value::Map(m) => m,
        Value::List(v, ..) if v.is_empty() => SassMap::new(),
        v => {
            return Err((
                format!("$map: {} is not a map.", v.to_css_string(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    Ok(map.get(&key, args.span())?.unwrap_or(Value::Null))
}

fn map_has_key(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(2)?;
    let key = parser.arg(&mut args, 1, "key")?;
    let map = match parser.arg(&mut args, 0, "map")? {
        Value::Map(m) => m,
        Value::List(v, ..) if v.is_empty() => SassMap::new(),
        v => {
            return Err((
                format!("$map: {} is not a map.", v.to_css_string(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::bool(map.get(&key, args.span())?.is_some()))
}

fn map_keys(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    let map = match parser.arg(&mut args, 0, "map")? {
        Value::Map(m) => m,
        Value::List(v, ..) if v.is_empty() => SassMap::new(),
        v => {
            return Err((
                format!("$map: {} is not a map.", v.to_css_string(args.span())?),
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

fn map_values(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    let map = match parser.arg(&mut args, 0, "map")? {
        Value::Map(m) => m,
        Value::List(v, ..) if v.is_empty() => SassMap::new(),
        v => {
            return Err((
                format!("$map: {} is not a map.", v.to_css_string(args.span())?),
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

fn map_merge(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(2)?;
    let mut map1 = match parser.arg(&mut args, 0, "map1")? {
        Value::Map(m) => m,
        Value::List(v, ..) if v.is_empty() => SassMap::new(),
        v => {
            return Err((
                format!("$map1: {} is not a map.", v.to_css_string(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    let map2 = match parser.arg(&mut args, 1, "map2")? {
        Value::Map(m) => m,
        Value::List(v, ..) if v.is_empty() => SassMap::new(),
        v => {
            return Err((
                format!("$map2: {} is not a map.", v.to_css_string(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    map1.merge(map2);
    Ok(Value::Map(map1))
}

fn map_remove(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    let mut map = match parser.arg(&mut args, 0, "map")? {
        Value::Map(m) => m,
        Value::List(v, ..) if v.is_empty() => SassMap::new(),
        v => {
            return Err((
                format!("$map: {} is not a map.", v.to_css_string(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    let keys = parser.variadic_args(args)?;
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
