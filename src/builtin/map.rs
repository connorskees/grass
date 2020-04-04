use std::collections::HashMap;

use super::Builtin;
use crate::common::{Brackets, ListSeparator};
use crate::value::{SassMap, Value};

pub(crate) fn register(f: &mut HashMap<String, Builtin>) {
    f.insert(
        "map-get".to_owned(),
        Builtin::new(|mut args, scope, super_selector| {
            max_args!(args, 2);
            let key = arg!(args, scope, super_selector, 1, "key");
            let map = match arg!(args, scope, super_selector, 0, "map") {
                Value::Map(m) => m,
                Value::List(v, ..) if v.is_empty() => SassMap::new(),
                v => return Err(format!("$map: {} is not a map.", v).into()),
            };
            Ok(map.get(&key)?.unwrap_or(Value::Null))
        }),
    );
    f.insert(
        "map-has-key".to_owned(),
        Builtin::new(|mut args, scope, super_selector| {
            max_args!(args, 2);
            let key = arg!(args, scope, super_selector, 1, "key");
            let map = match arg!(args, scope, super_selector, 0, "map") {
                Value::Map(m) => m,
                Value::List(v, ..) if v.is_empty() => SassMap::new(),
                v => return Err(format!("$map: {} is not a map.", v).into()),
            };
            Ok(Value::bool(map.get(&key)?.is_some()))
        }),
    );
    f.insert(
        "map-keys".to_owned(),
        Builtin::new(|mut args, scope, super_selector| {
            max_args!(args, 1);
            let map = match arg!(args, scope, super_selector, 0, "map") {
                Value::Map(m) => m,
                Value::List(v, ..) if v.is_empty() => SassMap::new(),
                v => return Err(format!("$map: {} is not a map.", v).into()),
            };
            Ok(Value::List(
                map.keys(),
                ListSeparator::Comma,
                Brackets::None,
            ))
        }),
    );
    f.insert(
        "map-values".to_owned(),
        Builtin::new(|mut args, scope, super_selector| {
            max_args!(args, 1);
            let map = match arg!(args, scope, super_selector, 0, "map") {
                Value::Map(m) => m,
                Value::List(v, ..) if v.is_empty() => SassMap::new(),
                v => return Err(format!("$map: {} is not a map.", v).into()),
            };
            Ok(Value::List(
                map.values(),
                ListSeparator::Comma,
                Brackets::None,
            ))
        }),
    );
    f.insert(
        "map-merge".to_owned(),
        Builtin::new(|mut args, scope, super_selector| {
            max_args!(args, 2);
            let mut map1 = match arg!(args, scope, super_selector, 0, "map1") {
                Value::Map(m) => m,
                Value::List(v, ..) if v.is_empty() => SassMap::new(),
                v => return Err(format!("$map1: {} is not a map.", v).into()),
            };
            let map2 = match arg!(args, scope, super_selector, 1, "map2") {
                Value::Map(m) => m,
                Value::List(v, ..) if v.is_empty() => SassMap::new(),
                v => return Err(format!("$map2: {} is not a map.", v).into()),
            };
            map1.merge(map2);
            Ok(Value::Map(map1))
        }),
    );
    f.insert(
        "map-remove".to_owned(),
        Builtin::new(|mut args, scope, super_selector| {
            let mut map = match arg!(args, scope, super_selector, 0, "map") {
                Value::Map(m) => m,
                Value::List(v, ..) if v.is_empty() => SassMap::new(),
                v => return Err(format!("$map: {} is not a map.", v).into()),
            };
            let keys = args.get_variadic(scope, super_selector)?;
            for key in keys {
                map.remove(&key);
            }
            Ok(Value::Map(map))
        }),
    );
}
