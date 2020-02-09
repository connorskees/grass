use std::collections::BTreeMap;

use super::Builtin;
use crate::units::Unit;
use crate::value::{Number, Value};

pub(crate) fn register(f: &mut BTreeMap<String, Builtin>) {
    decl!(f "percentage", |args, _| {
        let arg = dbg!(arg!(args, 0, "number").eval());
        let num = match arg {
            Value::Dimension(n, Unit::None) => n * Number::from(100),
            _ => todo!("expected unitless number in builtin function `percentage()`")
        };
        Some(Value::Dimension(num, Unit::Percent))
    });
}