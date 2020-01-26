use std::collections::BTreeMap;

use super::Builtin;
use crate::value::Value;

pub(crate) fn register(f: &mut BTreeMap<String, Builtin>) {
    f.insert("rgb".to_owned(), Box::new(|args| {
        let red = args.get("red");
        todo!()
    }));
}