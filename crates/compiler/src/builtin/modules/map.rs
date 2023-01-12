use crate::builtin::{
    map::{map_get, map_has_key, map_keys, map_merge, map_remove, map_set, map_values},
    modules::Module,
};

pub(crate) fn declare(f: &mut Module) {
    f.insert_builtin_arc("get", map_get);
    f.insert_builtin("has-key", map_has_key);
    f.insert_builtin("keys", map_keys);
    f.insert_builtin("merge", map_merge);
    f.insert_builtin("remove", map_remove);
    f.insert_builtin("values", map_values);
    f.insert_builtin("set", map_set);
}
