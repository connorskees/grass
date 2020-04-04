use std::collections::HashMap;

use super::Builtin;
use crate::unit::Unit;
use crate::value::{Number, Value};

pub(crate) fn register(f: &mut HashMap<String, Builtin>) {
    f.insert(
        "percentage".to_owned(),
        Builtin::new(|mut args, scope, super_selector| {
            max_args!(args, 1);
            let num = match arg!(args, scope, super_selector, 0, "number") {
                Value::Dimension(n, Unit::None) => n * Number::from(100),
                v @ Value::Dimension(..) => {
                    return Err(format!("$number: Expected {} to have no units.", v).into())
                }
                v => return Err(format!("$number: {} is not a number.", v).into()),
            };
            Ok(Value::Dimension(num, Unit::Percent))
        }),
    );
    f.insert(
        "round".to_owned(),
        Builtin::new(|mut args, scope, super_selector| {
            max_args!(args, 1);
            match arg!(args, scope, super_selector, 0, "number") {
                Value::Dimension(n, u) => Ok(Value::Dimension(n.round(), u)),
                v => Err(format!("$number: {} is not a number.", v).into()),
            }
        }),
    );
    f.insert(
        "ceil".to_owned(),
        Builtin::new(|mut args, scope, super_selector| {
            max_args!(args, 1);
            match arg!(args, scope, super_selector, 0, "number") {
                Value::Dimension(n, u) => Ok(Value::Dimension(n.ceil(), u)),
                v => Err(format!("$number: {} is not a number.", v).into()),
            }
        }),
    );
    f.insert(
        "floor".to_owned(),
        Builtin::new(|mut args, scope, super_selector| {
            max_args!(args, 1);
            match arg!(args, scope, super_selector, 0, "number") {
                Value::Dimension(n, u) => Ok(Value::Dimension(n.floor(), u)),
                v => Err(format!("$number: {} is not a number.", v).into()),
            }
        }),
    );
    f.insert(
        "abs".to_owned(),
        Builtin::new(|mut args, scope, super_selector| {
            max_args!(args, 1);
            match arg!(args, scope, super_selector, 0, "number") {
                Value::Dimension(n, u) => Ok(Value::Dimension(n.abs(), u)),
                v => Err(format!("$number: {} is not a number.", v).into()),
            }
        }),
    );
    f.insert(
        "comparable".to_owned(),
        Builtin::new(|mut args, scope, super_selector| {
            max_args!(args, 2);
            let unit1 = match arg!(args, scope, super_selector, 0, "number1") {
                Value::Dimension(_, u) => u,
                v => return Err(format!("$number1: {} is not a number.", v).into()),
            };
            let unit2 = match arg!(args, scope, super_selector, 1, "number2") {
                Value::Dimension(_, u) => u,
                v => return Err(format!("$number2: {} is not a number.", v).into()),
            };

            Ok(Value::bool(unit1.comparable(&unit2)))
        }),
    );
}
