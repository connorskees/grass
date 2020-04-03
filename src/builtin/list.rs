use std::collections::HashMap;

use num_traits::{One, Signed, ToPrimitive, Zero};

use super::Builtin;
use crate::common::{Brackets, ListSeparator, QuoteKind};
use crate::unit::Unit;
use crate::value::{Number, Value};

pub(crate) fn register(f: &mut HashMap<String, Builtin>) {
    f.insert(
        "length".to_owned(),
        Box::new(|mut args, _| {
            max_args!(args, 1);
            let len = match arg!(args, 0, "list") {
                Value::List(v, ..) => Number::from(v.len()),
                Value::Map(m) => Number::from(m.len()),
                _ => Number::one(),
            };
            Ok(Value::Dimension(len, Unit::None))
        }),
    );
    f.insert(
        "nth".to_owned(),
        Box::new(|mut args, _| {
            max_args!(args, 2);
            let list = match arg!(args, 0, "list") {
                Value::List(v, ..) => v,
                Value::Map(m) => m.entries(),
                v => vec![v],
            };
            let n = match arg!(args, 1, "n") {
                Value::Dimension(num, _) => num,
                v => return Err(format!("$n: {} is not a number.", v).into()),
            };

            if n.is_zero() {
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

            if n.is_positive() {
                Ok(list[n.to_integer().to_usize().unwrap() - 1].clone())
            } else {
                Ok(list[list.len() - n.abs().to_integer().to_usize().unwrap()].clone())
            }
        }),
    );
    f.insert(
        "list-separator".to_owned(),
        Box::new(|mut args, _| {
            max_args!(args, 1);
            Ok(Value::Ident(
                match arg!(args, 0, "list") {
                    Value::List(_, sep, ..) => sep.name(),
                    _ => ListSeparator::Space.name(),
                }
                .to_owned(),
                QuoteKind::None,
            ))
        }),
    );
    f.insert(
        "set-nth".to_owned(),
        Box::new(|mut args, _| {
            max_args!(args, 3);
            let (mut list, sep, brackets) = match arg!(args, 0, "list") {
                Value::List(v, sep, b) => (v, sep, b),
                Value::Map(m) => (m.entries(), ListSeparator::Comma, Brackets::None),
                v => (vec![v], ListSeparator::Space, Brackets::None),
            };
            let n = match arg!(args, 1, "n") {
                Value::Dimension(num, _) => num,
                v => return Err(format!("$n: {} is not a number.", v).into()),
            };

            if n.is_zero() {
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

            if n.is_positive() {
                list[n.to_integer().to_usize().unwrap() - 1] = val;
            } else {
                list[len - n.abs().to_integer().to_usize().unwrap()] = val;
            }

            Ok(Value::List(list, sep, brackets))
        }),
    );
    f.insert(
        "append".to_owned(),
        Box::new(|mut args, _| {
            max_args!(args, 3);
            let (mut list, sep, brackets) = match arg!(args, 0, "list") {
                Value::List(v, sep, b) => (v, sep, b),
                v => (vec![v], ListSeparator::Space, Brackets::None),
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
                v => return Err(format!("$separator: {} is not a string.", v).into()),
            };

            list.push(val);

            Ok(Value::List(list, sep, brackets))
        }),
    );
    f.insert(
        "join".to_owned(),
        Box::new(|mut args, _| {
            max_args!(args, 4);
            let (mut list1, sep1, brackets) = match arg!(args, 0, "list1") {
                Value::List(v, sep, brackets) => (v, sep, brackets),
                Value::Map(m) => (m.entries(), ListSeparator::Comma, Brackets::None),
                v => (vec![v], ListSeparator::Space, Brackets::None),
            };
            let (list2, sep2) = match arg!(args, 1, "list2") {
                Value::List(v, sep, ..) => (v, sep),
                Value::Map(m) => (m.entries(), ListSeparator::Comma),
                v => (vec![v], ListSeparator::Space),
            };
            let sep = match arg!(
                args,
                2,
                "separator" = Value::Ident("auto".to_owned(), QuoteKind::None)
            ) {
                Value::Ident(s, ..) => match s.as_str() {
                    "auto" => {
                        if list1.is_empty() || (list1.len() == 1 && sep1 == ListSeparator::Space) {
                            sep2
                        } else {
                            sep1
                        }
                    }
                    "comma" => ListSeparator::Comma,
                    "space" => ListSeparator::Space,
                    _ => {
                        return Err("$separator: Must be \"space\", \"comma\", or \"auto\".".into())
                    }
                },
                v => return Err(format!("$separator: {} is not a string.", v).into()),
            };

            let brackets = match arg!(
                args,
                3,
                "bracketed" = Value::Ident("auto".to_owned(), QuoteKind::None)
            ) {
                Value::Ident(s, ..) => match s.as_str() {
                    "auto" => brackets,
                    _ => Brackets::Bracketed,
                },
                v => {
                    if v.is_true()? {
                        Brackets::Bracketed
                    } else {
                        Brackets::None
                    }
                }
            };

            list1.extend(list2);

            Ok(Value::List(list1, sep, brackets))
        }),
    );
    f.insert(
        "is-bracketed".to_owned(),
        Box::new(|mut args, _| {
            max_args!(args, 1);
            Ok(Value::bool(match arg!(args, 0, "list") {
                Value::List(.., brackets) => match brackets {
                    Brackets::Bracketed => true,
                    Brackets::None => false,
                },
                _ => false,
            }))
        }),
    );
    f.insert(
        "index".to_owned(),
        Box::new(|mut args, _| {
            max_args!(args, 2);
            let list = match arg!(args, 0, "list") {
                Value::List(v, ..) => v,
                Value::Map(m) => m.entries(),
                v => vec![v],
            };
            let value = arg!(args, 1, "value");
            // TODO: find a way around this unwrap.
            // It should be impossible to hit as the arg is
            // evaluated prior to checking equality, but
            // it is still dirty.
            // Potential input to fuzz: index(1px 1in 1cm, 96px + 1rem)
            let index = match list
                .into_iter()
                .position(|v| v.equals(value.clone()).unwrap())
            {
                Some(v) => Number::from(v + 1),
                None => return Ok(Value::Null),
            };
            Ok(Value::Dimension(index, Unit::None))
        }),
    );
    f.insert(
        "zip".to_owned(),
        Box::new(|args, _| {
            let lists = args
                .get_variadic()?
                .into_iter()
                .map(|x| match x {
                    Value::List(v, ..) => v,
                    Value::Map(m) => m.entries(),
                    v => vec![v],
                })
                .collect::<Vec<Vec<Value>>>();

            let len = lists.iter().map(|l| l.len()).min().unwrap_or(0);

            if len == 0 {
                return Ok(Value::List(
                    Vec::new(),
                    ListSeparator::Comma,
                    Brackets::None,
                ));
            }

            let result = (0..len)
                .map(|i| {
                    let items = lists.iter().map(|v| v[i].clone()).collect();
                    Value::List(items, ListSeparator::Space, Brackets::None)
                })
                .collect();

            Ok(Value::List(result, ListSeparator::Comma, Brackets::None))
        }),
    );
}
