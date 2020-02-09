use std::collections::BTreeMap;
use std::convert::TryInto;

use num_bigint::BigInt;

use super::Builtin;
use crate::color::Color;
use crate::common::QuoteKind;
use crate::units::Unit;
use crate::value::{Number, Value};

pub(crate) fn register(f: &mut BTreeMap<String, Builtin>) {
    decl!(f "rgb", |args, _| {
        let channels = args.get("channels").unwrap_or(&Value::Null);
        if channels.is_null() {
            let red: u16 = arg!(args, 0, "red").clone().try_into().unwrap();
            let green: u16 = arg!(args, 1, "green").clone().try_into().unwrap();
            let blue: u16 = arg!(args, 2, "blue").clone().try_into().unwrap();
            Some(Value::Color(Color::from_values(red, green, blue, Number::from(1))))
        } else {
            todo!("channels variable in `rgb`")
        }
    });
    decl!(f "rgba", |args, _| {
        let channels = args.get("channels").unwrap_or(&Value::Null);
        if channels.is_null() {
            let red: u16 = arg!(args, 0, "red").clone().try_into().unwrap();
            let green: u16 = arg!(args, 1, "green").clone().try_into().unwrap();
            let blue: u16 = arg!(args, 2, "blue").clone().try_into().unwrap();
            let alpha = match arg!(args, 3, "alpha").clone().eval() {
                Value::Dimension(n, Unit::None) => n,
                Value::Dimension(n, Unit::Percent) => n / Number::from(100),
                _ => todo!("expected either unitless or % number for alpha"),
            };
            Some(Value::Color(Color::from_values(red, green, blue, alpha)))
        } else {
            todo!("channels variable in `rgb`")
        }
    });
    decl!(f "red", |args, _| {
        match arg!(args, 0, "red") {
            Value::Color(c) => Some(Value::Dimension(Number::from(BigInt::from(c.red())), Unit::None)),
            _ => todo!("non-color given to builtin function `red()`")
        }
    });
    decl!(f "green", |args, _| {
        match arg!(args, 0, "green") {
            Value::Color(c) => Some(Value::Dimension(Number::from(BigInt::from(c.green())), Unit::None)),
            _ => todo!("non-color given to builtin function `green()`")
        }
    });
    decl!(f "blue", |args, _| {
        match arg!(args, 0, "blue") {
            Value::Color(c) => Some(Value::Dimension(Number::from(BigInt::from(c.blue())), Unit::None)),
            _ => todo!("non-color given to builtin function `blue()`")
        }
    });
    decl!(f "opacity", |args, _| {
        match arg!(args, 0, "color") {
            Value::Color(c) => Some(Value::Dimension(c.alpha() / Number::from(255), Unit::None)),
            Value::Dimension(num, unit) => Some(Value::Ident(format!("opacity({}{})", num , unit), QuoteKind::None)),
            _ => todo!("non-color given to builtin function `opacity()`")
        }
    });
    decl!(f "alpha", |args, _| {
        match arg!(args, 0, "color") {
            Value::Color(c) => Some(Value::Dimension(c.alpha() / Number::from(255), Unit::None)),
            _ => todo!("non-color given to builtin function `alpha()`")
        }
    });
}
