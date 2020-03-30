use std::collections::HashMap;

use super::Builtin;
use crate::common::{Brackets, ListSeparator};
use crate::value::Value;

pub(crate) fn register(f: &mut HashMap<String, Builtin>) {
    f.insert(
        "map-get".to_owned(),
        Box::new(|args, _| {
            max_args!(args, 2);
            let map = match arg!(args, 0, "map") {
                Value::Map(m) => m,
                v => return Err(format!("$map: {} is not a map.", v).into()),
            };
            let key = arg!(args, 1, "key");
            Ok(map.get(key)?.unwrap_or(Value::Null).clone())
        }),
    );
    f.insert(
        "map-has-key".to_owned(),
        Box::new(|args, _| {
            max_args!(args, 2);
            let map = match arg!(args, 0, "map") {
                Value::Map(m) => m,
                v => return Err(format!("$map: {} is not a map.", v).into()),
            };
            let key = arg!(args, 1, "key");
            Ok(Value::bool(map.get(key)?.is_some()))
        }),
    );
    f.insert(
        "map-keys".to_owned(),
        Box::new(|args, _| {
            max_args!(args, 1);
            let map = match arg!(args, 0, "map") {
                Value::Map(m) => m,
                v => return Err(format!("$map: {} is not a map.", v).into()),
            };
            Ok(Value::List(
                map.keys(),
                ListSeparator::Space,
                Brackets::None,
            ))
        }),
    );
    f.insert(
        "map-values".to_owned(),
        Box::new(|args, _| {
            max_args!(args, 1);
            let map = match arg!(args, 0, "map") {
                Value::Map(m) => m,
                v => return Err(format!("$map: {} is not a map.", v).into()),
            };
            Ok(Value::List(
                map.values(),
                ListSeparator::Space,
                Brackets::None,
            ))
        }),
    );
    f.insert(
        "map-merge".to_owned(),
        Box::new(|args, _| {
            max_args!(args, 2);
            let mut map1 = match arg!(args, 0, "map1") {
                Value::Map(m) => m,
                v => return Err(format!("$map1: {} is not a map.", v).into()),
            };
            let map2 = match arg!(args, 1, "map2") {
                Value::Map(m) => m,
                v => return Err(format!("$map2: {} is not a map.", v).into()),
            };
            map1.merge(map2);
            Ok(Value::Map(map1))
        }),
    );
}
