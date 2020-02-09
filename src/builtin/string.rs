use std::collections::BTreeMap;

use super::Builtin;
use crate::common::QuoteKind;
use crate::units::Unit;
use crate::value::{Number, Value};

pub(crate) fn register(f: &mut BTreeMap<String, Builtin>) {
    decl!(f "to-upper-case", |args, _| {
        let s: &Value = arg!(args, 0, "string");
        match s.eval() {
            Value::Ident(i, q) => Some(Value::Ident(i.to_ascii_uppercase(), q)),
            _ => todo!("")
        }
    });
    decl!(f "to-lower-case", |args, _| {
        let s: &Value = arg!(args, 0, "string");
        match s.eval() {
            Value::Ident(i, q) => Some(Value::Ident(i.to_ascii_lowercase(), q)),
            _ => todo!("")
        }
    });
    decl!(f "str-length", |args, _| {
        let s: &Value = arg!(args, 0, "string");
        match s.eval() {
            Value::Ident(i, _) => Some(Value::Dimension(Number::from(i.len()), Unit::None)),
            _ => todo!("")
        }
    });
    decl!(f "quote", |args, _| {
        let s = arg!(args, 0, "string").eval();
        match s {
            Value::Ident(i, _) => Some(Value::Ident(i, QuoteKind::Double)),
            _ => todo!("")
        }
    });
    decl!(f "unquote", |args, _| {
        Some(arg!(args, 0, "string").eval().unquote())
    });
}
