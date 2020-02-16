use std::collections::BTreeMap;

use num_bigint::BigInt;
use num_traits::cast::ToPrimitive;
use num_traits::sign::Signed;

use super::Builtin;
use crate::common::QuoteKind;
use crate::units::Unit;
use crate::value::{Number, Value};

pub(crate) fn register(f: &mut BTreeMap<String, Builtin>) {
    decl!(f "to-upper-case", |args, _| {
        let s: &Value = arg!(args, 0, "string");
        match s.eval() {
            Value::Ident(i, q) => Ok(Value::Ident(i.to_ascii_uppercase(), q)),
            _ => todo!("")
        }
    });
    decl!(f "to-lower-case", |args, _| {
        let s: &Value = arg!(args, 0, "string");
        match s.eval() {
            Value::Ident(i, q) => Ok(Value::Ident(i.to_ascii_lowercase(), q)),
            _ => todo!("")
        }
    });
    decl!(f "str-length", |args, _| {
        let s: &Value = arg!(args, 0, "string");
        match s.eval() {
            Value::Ident(i, _) => Ok(Value::Dimension(Number::from(i.len()), Unit::None)),
            _ => todo!("")
        }
    });
    decl!(f "quote", |args, _| {
        let s = arg!(args, 0, "string").eval();
        match s {
            Value::Ident(i, _) => Ok(Value::Ident(i, QuoteKind::Double)),
            _ => todo!("")
        }
    });
    decl!(f "unquote", |args, _| {
        Ok(arg!(args, 0, "string").eval().unquote())
    });
    decl!(f "str-slice", |args, _| {
        let (string, quotes) = match arg!(args, 0, "string").eval() {
            Value::Ident(s, q) => (s, q),
            _ => todo!("____ is not a string")
        };
        let str_len = string.len();
        let start = match arg!(args, 1, "start-at").eval() {
            Value::Dimension(n, Unit::None) if n.to_integer().is_positive() => n.to_integer().to_usize().unwrap(),
            Value::Dimension(n, Unit::None) if n == Number::from(0) => 1_usize,
            Value::Dimension(n, Unit::None) if n < -Number::from(str_len) => 1_usize,
            Value::Dimension(n, Unit::None) => (BigInt::from(str_len + 1) + n.to_integer()).to_usize().unwrap(),
            Value::Dimension(..) => todo!("$start: Expected ___ to have no units."),
            _ => todo!("$start-at: ____ is not a number")
        };
        let mut end = match arg!(args, 2, "end-at"=Value::Null).eval() {
            Value::Dimension(n, Unit::None) if n.to_integer().is_positive() => n.to_integer().to_usize().unwrap(),
            Value::Dimension(n, Unit::None) if n == Number::from(0) => 0_usize,
            Value::Dimension(n, Unit::None) if n < -Number::from(str_len) => 0_usize,
            Value::Dimension(n, Unit::None) => (BigInt::from(str_len + 1) + n.to_integer()).to_usize().unwrap(),
            Value::Dimension(..) => todo!("$end: Expected ___ to have no units."),
            Value::Null => str_len,
            _ => todo!("$end-at: ____ is not a number")
        };

        if end > str_len {
            end = str_len;
        }

        if start > end || start > str_len {
            match quotes {
                QuoteKind::Double | QuoteKind::Single => Ok(Value::Ident(String::new(), QuoteKind::Double)),
                QuoteKind::None => Ok(Value::Null),
            }
        } else {
            let s = string[start-1..end].to_string();
            match quotes {
                QuoteKind::Double | QuoteKind::Single => Ok(Value::Ident(s, QuoteKind::Double)),
                QuoteKind::None => Ok(Value::Ident(s, QuoteKind::None)),
            }
        }
    });
}
