use std::collections::HashMap;

use num_traits::cast::ToPrimitive;

use super::Builtin;
use crate::common::{ListSeparator, QuoteKind};
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
    f.insert(
        "nth".to_owned(),
        Box::new(|args, _| {
            max_args!(args, 2);
            let list = match arg!(args, 0, "list") {
                Value::List(v, _) => v,
                v => vec![v],
            };
            let n = match arg!(args, 1, "n") {
                Value::Dimension(num, _) => num,
                v => return Err(format!("$n: {} is not a number.", v).into()),
            };

            if n == Number::from(0) {
                return Err("$n: List index may not be 0.".into());
            }

            if n.abs() > Number::from(list.len()) {
                return Err(format!(
                    "$n: Invalid index {} for a list with {} elements.",
                    n,
                    list.len()
                )
                .into());
            }

            if n.is_decimal() {
                return Err(format!("$n: {} is not an int.", n).into());
            }

            if n > Number::from(0) {
                Ok(list[n.to_integer().to_usize().unwrap() - 1].clone())
            } else {
                Ok(list[list.len() - n.abs().to_integer().to_usize().unwrap()].clone())
            }
        }),
    );
    f.insert(
        "list-separator".to_owned(),
        Box::new(|args, _| {
            max_args!(args, 1);
            Ok(Value::Ident(
                match arg!(args, 0, "list") {
                    Value::List(_, sep) => sep.name(),
                    _ => ListSeparator::Space.name(),
                }
                .to_owned(),
                QuoteKind::None,
            ))
        }),
    );
    f.insert(
        "set-nth".to_owned(),
        Box::new(|args, _| {
            max_args!(args, 3);
            let (mut list, sep) = match arg!(args, 0, "list") {
                Value::List(v, sep) => (v, sep),
                v => (vec![v], ListSeparator::Space),
            };
            let n = match arg!(args, 1, "n") {
                Value::Dimension(num, _) => num,
                v => return Err(format!("$n: {} is not a number.", v).into()),
            };

            if n == Number::from(0) {
                return Err("$n: List index may not be 0.".into());
            }

            let len = list.len();

            if n.abs() > Number::from(len) {
                return Err(
                    format!("$n: Invalid index {} for a list with {} elements.", n, len).into(),
                );
            }

            if n.is_decimal() {
                return Err(format!("$n: {} is not an int.", n).into());
            }

            let val = arg!(args, 2, "value");

            if n > Number::from(0) {
                list[n.to_integer().to_usize().unwrap() - 1] = val;
            } else {
                list[len - n.abs().to_integer().to_usize().unwrap()] = val;
            }

            Ok(Value::List(list, sep))
        }),
    );
    f.insert(
        "append".to_owned(),
        Box::new(|args, _| {
            max_args!(args, 3);
            let (mut list, sep) = match arg!(args, 0, "list") {
                Value::List(v, sep) => (v, sep),
                v => (vec![v], ListSeparator::Space),
            };
            let val = arg!(args, 1, "val");
            let sep = match arg!(
                args,
                2,
                "separator" = Value::Ident("auto".to_owned(), QuoteKind::None)
            ) {
                Value::Ident(s, ..) => match s.as_str() {
                    "auto" => sep,
                    "comma" => ListSeparator::Comma,
                    "space" => ListSeparator::Space,
                    _ => {
                        return Err("$separator: Must be \"space\", \"comma\", or \"auto\".".into())
                    }
                },
                _ => return Err("$separator: Must be \"space\", \"comma\", or \"auto\".".into()),
            };

            list.push(val);

            Ok(Value::List(list, sep))
        }),
    );
}
