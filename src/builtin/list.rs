use std::collections::HashMap;

use super::Builtin;
use crate::unit::Unit;
use crate::value::{Number, Value};

pub(crate) fn register(f: &mut HashMap<String, Builtin>) {
    f.insert(
        "length".to_owned(),
        Box::new(|args, _| {
            max_args!(args, 1);
            let len = match arg!(args, 0, "list") {
                Value::List(v, _) => Number::from(v.len()),
                _ => Number::from(1),
            };
            Ok(Value::Dimension(len, Unit::None))
        }),
    );
}
