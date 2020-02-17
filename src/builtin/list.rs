use std::collections::BTreeMap;

use super::Builtin;
use crate::units::Unit;
use crate::value::{Number, Value};

pub(crate) fn register(f: &mut BTreeMap<String, Builtin>) {
    decl!(f "length", |args, _| {
        let len = match arg!(args, 0, "list") {
            Value::List(v, _) => Number::from(v.len()),
            _ => Number::from(1)
        };
        Ok(Value::Dimension(len, Unit::None))
    });
}
