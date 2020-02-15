use std::collections::BTreeMap;

use super::Builtin;
use crate::color::Color;
use crate::units::Unit;
use crate::value::{Number, Value};

pub(crate) fn register(f: &mut BTreeMap<String, Builtin>) {
    decl!(f "rgb", |args, _| {
        let channels = args.get("channels").unwrap_or(&Value::Null);
        if channels.is_null() {
            let red = match arg!(args, 0, "red").eval() {
                Value::Dimension(n, Unit::None) => n,
                Value::Dimension(n, Unit::Percent) => (n / Number::from(100)) * Number::from(255),
                _ => todo!("expected either unitless or % number for alpha"),
            };
            let green = match arg!(args, 1, "green").eval() {
                Value::Dimension(n, Unit::None) => n,
                Value::Dimension(n, Unit::Percent) => (n / Number::from(100)) * Number::from(255),
                _ => todo!("expected either unitless or % number for alpha"),
            };
            let blue = match arg!(args, 2, "blue").eval() {
                Value::Dimension(n, Unit::None) => n,
                Value::Dimension(n, Unit::Percent) => (n / Number::from(100)) * Number::from(255),
                _ => todo!("expected either unitless or % number for alpha"),
            };
            let alpha = match arg!(args, 3, "alpha"=Value::Dimension(Number::from(1), Unit::None)) {
                Value::Dimension(n, Unit::None) => n,
                Value::Dimension(n, Unit::Percent) => n / Number::from(100),
                _ => todo!("non-number alpha given to builtin function `rgb()`")
            };
            Some(Value::Color(Color::from_rgba(red, green, blue, alpha)))
        } else {
            todo!("channels variable in `rgb`")
        }
    });
    decl!(f "rgba", |args, _| {

        if args.len() == 1 {
            let mut channels = match arg!(args, 0, "channels").eval() {
                Value::List(v, _) => v,
                _ => todo!("missing element $green")
            };

            assert_eq!(channels.len(), 3_usize);

            let blue = match channels.pop() {
                Some(Value::Dimension(n, Unit::None)) => n,
                Some(Value::Dimension(n, Unit::Percent)) => n / Number::from(100),
                _ => todo!("$blue: ___ is not a color")
            };

            let green = match channels.pop() {
                Some(Value::Dimension(n, Unit::None)) => n,
                Some(Value::Dimension(n, Unit::Percent)) => n / Number::from(100),
                _ => todo!("$green: ___ is not a color")
            };

            let red = match channels.pop() {
                Some(Value::Dimension(n, Unit::None)) => n,
                Some(Value::Dimension(n, Unit::Percent)) => n / Number::from(100),
                _ => todo!("$red: ___ is not a color")
            };

            let color = Color::from_rgba(red, green, blue, Number::from(1));

            Some(Value::Color(color))

        } else if args.len() == 2 {
            let color = match arg!(args, 0, "color").eval() {
                Value::Color(c) => c,
                _ => todo!("expected color")
            };
            let alpha = match arg!(args, 1, "alpha").eval() {
                Value::Dimension(n, Unit::None) => n,
                Value::Dimension(n, Unit::Percent) => n / Number::from(100),
                _ => todo!("expected either unitless or % number for alpha"),
            };
            Some(Value::Color(color.with_alpha(alpha)))
        } else {
            let red = match arg!(args, 0, "red").eval() {
                Value::Dimension(n, Unit::None) => n,
                Value::Dimension(n, Unit::Percent) => (n / Number::from(100)) * Number::from(255),
                _ => todo!("expected either unitless or % number for alpha"),
            };
            let green = match arg!(args, 1, "green").eval() {
                Value::Dimension(n, Unit::None) => n,
                Value::Dimension(n, Unit::Percent) => (n / Number::from(100)) * Number::from(255),
                _ => todo!("expected either unitless or % number for alpha"),
            };
            let blue = match arg!(args, 2, "blue").eval() {
                Value::Dimension(n, Unit::None) => n,
                Value::Dimension(n, Unit::Percent) => (n / Number::from(100)) * Number::from(255),
                _ => todo!("expected either unitless or % number for alpha"),
            };
            let alpha = match arg!(args, 3, "alpha"=Value::Dimension(Number::from(1), Unit::None)).eval() {
                Value::Dimension(n, Unit::None) => n,
                Value::Dimension(n, Unit::Percent) => n / Number::from(100),
                _ => todo!("expected either unitless or % number for alpha")
            };
            Some(Value::Color(Color::from_rgba(red, green, blue, alpha)))
        }
    });
    decl!(f "red", |args, _| {
        match arg!(args, 0, "color") {
            Value::Color(c) => Some(Value::Dimension(c.red(), Unit::None)),
            _ => todo!("non-color given to builtin function `red()`")
        }
    });
    decl!(f "green", |args, _| {
        match arg!(args, 0, "color") {
            Value::Color(c) => Some(Value::Dimension(c.green(), Unit::None)),
            _ => todo!("non-color given to builtin function `green()`")
        }
    });
    decl!(f "blue", |args, _| {
        match arg!(args, 0, "color") {
            Value::Color(c) => Some(Value::Dimension(c.blue(), Unit::None)),
            _ => todo!("non-color given to builtin function `blue()`")
        }
    });
    decl!(f "mix", |args, _| {
        let color1 = match arg!(args, 0, "color1").eval() {
            Value::Color(c) => c,
            _ => todo!("non-color given to builtin function `mix()`")
        };

        let color2 = match arg!(args, 1, "color2").eval() {
            Value::Color(c) => c,
            _ => todo!("non-color given to builtin function `mix()`")
        };

        let weight = match arg!(args, 2, "weight"=Value::Dimension(Number::ratio(1, 2), Unit::None)) {
            Value::Dimension(n, Unit::None) => n,
            Value::Dimension(n, Unit::Percent) => n / Number::from(100),
            _ => todo!("expected either unitless or % number for $weight")
        };
        Some(Value::Color(color1.mix(color2, weight)))
    });
}
