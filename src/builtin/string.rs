use std::collections::HashMap;

use num_bigint::BigInt;
use num_traits::{Signed, ToPrimitive, Zero};

use super::Builtin;
use crate::common::QuoteKind;
use crate::unit::Unit;
use crate::value::{Number, Value};

pub(crate) fn register(f: &mut HashMap<String, Builtin>) {
    f.insert(
        "to-upper-case".to_owned(),
        Box::new(|mut args, _| {
            max_args!(args, 1);
            match arg!(args, 0, "string") {
                Value::Ident(i, q) => Ok(Value::Ident(i.to_ascii_uppercase(), q)),
                v => Err(format!("$string: {} is not a string.", v).into()),
            }
        }),
    );
    f.insert(
        "to-lower-case".to_owned(),
        Box::new(|mut args, _| {
            max_args!(args, 1);
            match arg!(args, 0, "string") {
                Value::Ident(i, q) => Ok(Value::Ident(i.to_ascii_lowercase(), q)),
                v => Err(format!("$string: {} is not a string.", v).into()),
            }
        }),
    );
    f.insert(
        "str-length".to_owned(),
        Box::new(|mut args, _| {
            max_args!(args, 1);
            match arg!(args, 0, "string") {
                Value::Ident(i, _) => Ok(Value::Dimension(
                    Number::from(i.chars().count()),
                    Unit::None,
                )),
                v => Err(format!("$string: {} is not a string.", v).into()),
            }
        }),
    );
    f.insert(
        "quote".to_owned(),
        Box::new(|mut args, _| {
            max_args!(args, 1);
            match arg!(args, 0, "string") {
                Value::Ident(i, _) => Ok(Value::Ident(i, QuoteKind::Double)),
                v => Err(format!("$string: {} is not a string.", v).into()),
            }
        }),
    );
    f.insert(
        "unquote".to_owned(),
        Box::new(|mut args, _| {
            max_args!(args, 1);
            match arg!(args, 0, "string") {
                i @ Value::Ident(..) => Ok(i.unquote()),
                v => Err(format!("$string: {} is not a string.", v).into()),
            }
        }),
    );
    f.insert(
        "str-slice".to_owned(),
        Box::new(|mut args, _| {
            max_args!(args, 3);
            let (string, quotes) = match arg!(args, 0, "string") {
                Value::Ident(s, q) => (s, q),
                v => return Err(format!("$string: {} is not a string.", v).into()),
            };
            let str_len = string.chars().count();
            let start = match arg!(args, 1, "start-at") {
                Value::Dimension(n, Unit::None) if n.is_decimal() => {
                    return Err(format!("{} is not an int.", n).into())
                }
                Value::Dimension(n, Unit::None) if n.is_positive() => {
                    n.to_integer().to_usize().unwrap_or(str_len + 1)
                }
                Value::Dimension(n, Unit::None) if n.is_zero() => 1_usize,
                Value::Dimension(n, Unit::None) if n < -Number::from(str_len) => 1_usize,
                Value::Dimension(n, Unit::None) => (BigInt::from(str_len + 1) + n.to_integer())
                    .to_usize()
                    .unwrap(),
                v @ Value::Dimension(..) => {
                    return Err(format!("$start: Expected {} to have no units.", v).into())
                }
                v => return Err(format!("$start-at: {} is not a number.", v).into()),
            };
            let mut end = match arg!(args, 2, "end-at" = Value::Null) {
                Value::Dimension(n, Unit::None) if n.is_decimal() => {
                    return Err(format!("{} is not an int.", n).into())
                }
                Value::Dimension(n, Unit::None) if n.is_positive() => {
                    n.to_integer().to_usize().unwrap_or(str_len + 1)
                }
                Value::Dimension(n, Unit::None) if n.is_zero() => 0_usize,
                Value::Dimension(n, Unit::None) if n < -Number::from(str_len) => 0_usize,
                Value::Dimension(n, Unit::None) => (BigInt::from(str_len + 1) + n.to_integer())
                    .to_usize()
                    .unwrap_or(str_len + 1),
                v @ Value::Dimension(..) => {
                    return Err(format!("$end: Expected {} to have no units.", v).into())
                }
                Value::Null => str_len,
                v => return Err(format!("$end-at: {} is not a number.", v).into()),
            };

            if end > str_len {
                end = str_len;
            }

            if start > end || start > str_len {
                Ok(Value::Ident(String::new(), quotes.normalize()))
            } else {
                Ok(Value::Ident(
                    string
                        .chars()
                        .skip(start - 1)
                        .take(end - start + 1)
                        .collect(),
                    quotes.normalize(),
                ))
            }
        }),
    );
    f.insert(
        "str-index".to_owned(),
        Box::new(|mut args, _| {
            max_args!(args, 2);
            let s1 = match arg!(args, 0, "string") {
                Value::Ident(i, _) => i,
                v => return Err(format!("$string: {} is not a string.", v).into()),
            };

            let substr = match arg!(args, 1, "substring") {
                Value::Ident(i, _) => i,
                v => return Err(format!("$substring: {} is not a string.", v).into()),
            };

            Ok(match s1.find(&substr) {
                Some(v) => Value::Dimension(Number::from(v + 1), Unit::None),
                None => Value::Null,
            })
        }),
    );
    f.insert(
        "str-insert".to_owned(),
        Box::new(|mut args, _| {
            max_args!(args, 3);
            let (s1, quotes) = match arg!(args, 0, "string") {
                Value::Ident(i, q) => (i, q.normalize()),
                v => return Err(format!("$string: {} is not a string.", v).into()),
            };

            let substr = match arg!(args, 1, "insert") {
                Value::Ident(i, _) => i,
                v => return Err(format!("$insert: {} is not a string.", v).into()),
            };

            let index = match arg!(args, 2, "index") {
                Value::Dimension(n, Unit::None) if n.is_decimal() => {
                    return Err(format!("$index: {} is not an int.", n).into())
                }
                Value::Dimension(n, Unit::None) => n,
                v @ Value::Dimension(..) => {
                    return Err(format!("$index: Expected {} to have no units.", v).into())
                }
                v => return Err(format!("$index: {} is not a number.", v).into()),
            };

            if s1.is_empty() {
                return Ok(Value::Ident(substr, quotes));
            }

            let len = s1.chars().count();

            // Insert substring at char position, rather than byte position
            let insert = |idx, s1: String, s2| {
                s1.chars()
                    .enumerate()
                    .map(|(i, c)| {
                        if i + 1 == idx {
                            c.to_string() + s2
                        } else if idx == 0 && i == 0 {
                            s2.to_string() + &c.to_string()
                        } else {
                            c.to_string()
                        }
                    })
                    .collect::<String>()
            };

            let string = if index.is_positive() {
                insert(
                    index
                        .to_integer()
                        .to_usize()
                        .unwrap_or(len + 1)
                        .min(len + 1)
                        - 1,
                    s1,
                    &substr,
                )
            } else if index.is_zero() {
                insert(0, s1, &substr)
            } else {
                let idx = index.abs().to_integer().to_usize().unwrap_or(len + 1);
                if idx > len {
                    insert(0, s1, &substr)
                } else {
                    insert(len - idx + 1, s1, &substr)
                }
            };

            Ok(Value::Ident(string, quotes))
        }),
    );
}
