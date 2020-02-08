use std::collections::BTreeMap;
use std::convert::TryInto;

use num_bigint::BigInt;
use num_rational::BigRational;

use super::Builtin;
use crate::color::Color;
use crate::units::Unit;
use crate::value::Value;

pub(crate) fn register(f: &mut BTreeMap<String, Builtin>) {
    decl!(f "rgb", |args, _| {
        let channels = args.get("channels").unwrap_or(&Value::Null);
        if channels.is_null() {
            let red: u16 = arg!(args, 0, "red").clone().try_into().unwrap();
            let green: u16 = arg!(args, 1, "green").clone().try_into().unwrap();
            let blue: u16 = arg!(args, 2, "blue").clone().try_into().unwrap();
            Some(Value::Color(Color::from_values(red, green, blue, 1)))
        } else {
            todo!("channels variable in `rgb`")
        }
    });
    decl!(f "red", |args, _| {
        match arg!(args, 0, "red") {
            Value::Color(c) => Some(Value::Dimension(BigRational::from_integer(BigInt::from(c.red())), Unit::None)),
            _ => todo!("non-color given to builtin function `red()`")
        }
    });
    decl!(f "green", |args, _| {
        match arg!(args, 0, "green") {
            Value::Color(c) => Some(Value::Dimension(BigRational::from_integer(BigInt::from(c.green())), Unit::None)),
            _ => todo!("non-color given to builtin function `green()`")
        }
    });
    decl!(f "blue", |args, _| {
        match arg!(args, 0, "blue") {
            Value::Color(c) => Some(Value::Dimension(BigRational::from_integer(BigInt::from(c.blue())), Unit::None)),
            _ => todo!("non-color given to builtin function `blue()`")
        }
    });
}
