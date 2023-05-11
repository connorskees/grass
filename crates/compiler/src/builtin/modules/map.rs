use crate::builtin::builtin_imports::*;

use crate::builtin::{
    map::{map_get, map_has_key, map_keys, map_merge, map_remove, map_set, map_values},
    modules::Module,
};

fn deep_merge_impl(map1: SassMap, map2: SassMap) -> SassMap {
    if map1.is_empty() {
        return map2;
    }
    if map2.is_empty() {
        return map1;
    }

    let mut result = map1;

    for (key, value) in map2 {
        let result_map = result.get_ref(&key.node).and_then(Value::try_map);

        match result_map {
            Some(result_map) => match value.try_map() {
                Some(value_map) => {
                    let merged = deep_merge_impl(result_map, value_map);
                    result.insert(key, Value::Map(merged));
                }
                None => {
                    result.insert(key, value);
                }
            },
            None => {
                result.insert(key, value);
            }
        }
    }

    result
}

fn deep_merge(mut args: ArgumentResult, _: &mut Visitor) -> SassResult<Value> {
    args.max_args(2)?;

    let span = args.span();

    let map1 = args
        .get_err(0, "map1")?
        .assert_map_with_name("map1", span)?;

    let map2 = args
        .get_err(1, "map2")?
        .assert_map_with_name("map2", span)?;

    Ok(Value::Map(deep_merge_impl(map1, map2)))
}

pub(crate) fn declare(f: &mut Module) {
    f.insert_builtin("get", map_get);
    f.insert_builtin("has-key", map_has_key);
    f.insert_builtin("keys", map_keys);
    f.insert_builtin("merge", map_merge);
    f.insert_builtin("remove", map_remove);
    f.insert_builtin("values", map_values);
    f.insert_builtin("set", map_set);
    f.insert_builtin("deep-merge", deep_merge);
}
