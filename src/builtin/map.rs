use super::GlobalFunctionMap;

use super::Builtin;
use crate::args::CallArgs;
use crate::common::{Brackets, ListSeparator};
use crate::error::SassResult;
use crate::scope::Scope;
use crate::selector::Selector;
use crate::value::{SassMap, Value};

pub(crate) fn register(f: &mut GlobalFunctionMap) {
    fn map_get(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
        args.max_args(2)?;
        let key = arg!(args, scope, super_selector, 1, "key");
        let map = match arg!(args, scope, super_selector, 0, "map") {
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

    fn map_has_key(
        mut args: CallArgs,
        scope: &Scope,
        super_selector: &Selector,
    ) -> SassResult<Value> {
        args.max_args(2)?;
        let key = arg!(args, scope, super_selector, 1, "key");
        let map = match arg!(args, scope, super_selector, 0, "map") {
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

    fn map_keys(mut args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
        args.max_args(1)?;
        let map = match arg!(args, scope, super_selector, 0, "map") {
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

    fn map_values(
        mut args: CallArgs,
        scope: &Scope,
        super_selector: &Selector,
    ) -> SassResult<Value> {
        args.max_args(1)?;
        let map = match arg!(args, scope, super_selector, 0, "map") {
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

    fn map_merge(
        mut args: CallArgs,
        scope: &Scope,
        super_selector: &Selector,
    ) -> SassResult<Value> {
        args.max_args(2)?;
        let mut map1 = match arg!(args, scope, super_selector, 0, "map1") {
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
        let map2 = match arg!(args, scope, super_selector, 1, "map2") {
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

    fn map_remove(
        mut args: CallArgs,
        scope: &Scope,
        super_selector: &Selector,
    ) -> SassResult<Value> {
        let mut map = match arg!(args, scope, super_selector, 0, "map") {
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
        let keys = args.get_variadic(scope, super_selector)?;
        for key in keys {
            map.remove(&key);
        }
        Ok(Value::Map(map))
    }

    f.insert("map-get", Builtin::new(map_get));
    f.insert("map-has-key", Builtin::new(map_has_key));
    f.insert("map-keys", Builtin::new(map_keys));
    f.insert("map-values", Builtin::new(map_values));
    f.insert("map-merge", Builtin::new(map_merge));
    f.insert("map-remove", Builtin::new(map_remove));
}
