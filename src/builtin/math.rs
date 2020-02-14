use std::collections::BTreeMap;

use super::Builtin;
use crate::units::Unit;
use crate::value::{Number, Value};

pub(crate) fn register(f: &mut BTreeMap<String, Builtin>) {
    decl!(f "percentage", |args, _| {
        let num = match arg!(args, 0, "number").eval() {
            Value::Dimension(n, Unit::None) => n * Number::from(100),
            _ => todo!("expected unitless number in builtin function `percentage()`")
        };
        Some(Value::Dimension(num, Unit::Percent))
    });
    decl!(f "round", |args, _| {
        match arg!(args, 0, "number").eval() {
            Value::Dimension(n, u) => Some(Value::Dimension(n.round(), u)),
            _ => todo!("expected number in builtin function `round()`")
        }
    });
    decl!(f "ceil", |args, _| {
        match arg!(args, 0, "number").eval() {
            Value::Dimension(n, u) => Some(Value::Dimension(n.ceil(), u)),
            _ => todo!("expected number in builtin function `ceil()`")
        }
    });
    decl!(f "floor", |args, _| {
        match arg!(args, 0, "number").eval() {
            Value::Dimension(n, u) => Some(Value::Dimension(n.floor(), u)),
            _ => todo!("expected number in builtin function `floor()`")
        }
    });
    decl!(f "abs", |args, _| {
        match arg!(args, 0, "number").eval() {
            Value::Dimension(n, u) => Some(Value::Dimension(n.abs(), u)),
            _ => todo!("expected number in builtin function `abs()`")
        }
    });
}
