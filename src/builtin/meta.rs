use std::collections::BTreeMap;

use super::Builtin;
use crate::value::Value;

pub(crate) fn register(f: &mut BTreeMap<String, Builtin>) {
    decl!(f "if", |args| {
        let cond: &Value = arg!(args, 0, "condition");
        let if_true = arg!(args, 1, "if-true").clone();
        let if_false = arg!(args, 2, "if-false").clone();
        if cond.is_true() {
            Some(if_true)
        } else {
            Some(if_false)
        }
    });
}