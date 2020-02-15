use std::collections::BTreeMap;

use super::Builtin;
use crate::color::Color;
use crate::units::Unit;
use crate::value::{Number, Value};

pub(crate) fn register(f: &mut BTreeMap<String, Builtin>) {
    decl!(f "hsl", |args, _| {
        let hue = match arg!(args, 0, "hue").eval() {
            Value::Dimension(n, Unit::None)
            | Value::Dimension(n, Unit::Percent)
            | Value::Dimension(n, Unit::Deg) => n,
            _ => todo!("expected either unitless or % number for alpha"),
        };
        let saturation = match arg!(args, 1, "saturation").eval() {
            Value::Dimension(n, Unit::None)
            | Value::Dimension(n, Unit::Percent) => n / Number::from(100),
            _ => todo!("expected either unitless or % number for alpha"),
        };
        let luminance = match arg!(args, 2, "luminance").eval() {
            Value::Dimension(n, Unit::None)
            | Value::Dimension(n, Unit::Percent) => n / Number::from(100),
            _ => todo!("expected either unitless or % number for alpha"),
        };
        Some(Value::Color(Color::from_hsla(hue, saturation, luminance, Number::from(1))))
    });
    decl!(f "hsla", |args, _| {
        let hue = match arg!(args, 0, "hue").eval() {
            Value::Dimension(n, Unit::None)
            | Value::Dimension(n, Unit::Percent)
            | Value::Dimension(n, Unit::Deg) => n,
            _ => todo!("expected either unitless or % number for alpha"),
        };
        let saturation = match arg!(args, 1, "saturation").eval() {
            Value::Dimension(n, Unit::None)
            | Value::Dimension(n, Unit::Percent) => n / Number::from(100),
            _ => todo!("expected either unitless or % number for alpha"),
        };
        let luminance = match arg!(args, 2, "luminance").eval() {
            Value::Dimension(n, Unit::None)
            | Value::Dimension(n, Unit::Percent) => n / Number::from(100),
            _ => todo!("expected either unitless or % number for alpha"),
        };
        let alpha = match arg!(args, 3, "alpha").eval() {
            Value::Dimension(n, Unit::None) => n,
            Value::Dimension(n, Unit::Percent) => n / Number::from(100),
            _ => todo!("expected either unitless or % number for alpha"),
        };
        Some(Value::Color(Color::from_hsla(hue, saturation, luminance, alpha)))
    });
    decl!(f "hue", |args, _| {
        match arg!(args, 0, "color") {
            Value::Color(c) => Some(Value::Dimension(c.hue(), Unit::Deg)),
            _ => todo!("non-color given to builtin function `hue()`")
        }
    });
    decl!(f "saturation", |args, _| {
        match arg!(args, 0, "color") {
            Value::Color(c) => Some(Value::Dimension(c.saturation(), Unit::Percent)),
            _ => todo!("non-color given to builtin function `saturation()`")
        }
    });
    decl!(f "lightness", |args, _| {
        match arg!(args, 0, "color") {
            Value::Color(c) => Some(Value::Dimension(c.lightness(), Unit::Percent)),
            _ => todo!("non-color given to builtin function `lightness()`")
        }
    });
    decl!(f "adjust-hue", |args, _| {
        let color = match arg!(args, 0, "color").eval() {
            Value::Color(c) => c,
            _ => todo!("non-color given to builtin function `adjust-hue()`")
        };
        let degrees = match arg!(args, 1, "degrees").eval() {
            Value::Dimension(n, Unit::None)
            | Value::Dimension(n, Unit::Percent)
            | Value::Dimension(n, Unit::Deg) => n,
            _ => todo!("expected either unitless or % number for degrees"),
        };
        Some(Value::Color(color.adjust_hue(degrees)))
    });
    decl!(f "lighten", |args, _| {
        let color = match arg!(args, 0, "color").eval() {
            Value::Color(c) => c,
            _ => todo!("non-color given to builtin function `lighten()`")
        };
        let amount = match arg!(args, 1, "amount").eval() {
            Value::Dimension(n, Unit::None) => n,
            Value::Dimension(n, Unit::Percent) => n / Number::from(100),
            _ => todo!("expected either unitless or % number for amount"),
        };
        Some(Value::Color(color.lighten(amount)))
    });
    decl!(f "darken", |args, _| {
        let color = match arg!(args, 0, "color").eval() {
            Value::Color(c) => c,
            _ => todo!("non-color given to builtin function `darken()`")
        };
        let amount = match arg!(args, 1, "amount").eval() {
            Value::Dimension(n, Unit::None) => n,
            Value::Dimension(n, Unit::Percent) => n / Number::from(100),
            _ => todo!("expected either unitless or % number for amount"),
        };
        Some(Value::Color(color.darken(amount)))
    });
    decl!(f "saturate", |args, _| {
        let color = match arg!(args, 0, "color").eval() {
            Value::Color(c) => c,
            _ => todo!("non-color given to builtin function `saturate()`")
        };
        let amount = match arg!(args, 1, "amount").eval() {
            Value::Dimension(n, Unit::None) => n,
            Value::Dimension(n, Unit::Percent) => n / Number::from(100),
            _ => todo!("expected either unitless or % number for amount"),
        };
        Some(Value::Color(color.saturate(amount)))
    });
    decl!(f "desaturate", |args, _| {
        let color = match arg!(args, 0, "color").eval() {
            Value::Color(c) => c,
            _ => todo!("non-color given to builtin function `desaturate()`")
        };
        let amount = match arg!(args, 1, "amount").eval() {
            Value::Dimension(n, Unit::None) => n,
            Value::Dimension(n, Unit::Percent) => n / Number::from(100),
            _ => todo!("expected either unitless or % number for amount"),
        };
        Some(Value::Color(color.desaturate(amount)))
    });
    decl!(f "grayscale", |args, _| {
        let color = match arg!(args, 0, "color").eval() {
            Value::Color(c) => c,
            _ => todo!("non-color given to builtin function `grayscale()`")
        };
        Some(Value::Color(color.desaturate(Number::from(1))))
    });
    decl!(f "complement", |args, _| {
        let color = match arg!(args, 0, "color").eval() {
            Value::Color(c) => c,
            _ => todo!("non-color given to builtin function `complement()`")
        };
        Some(Value::Color(color.complement()))
    });
    decl!(f "invert", |args, _| {
        let weight = match arg!(args, 1, "weight"=Value::Dimension(Number::from(100), Unit::Percent)) {
            Value::Dimension(n, Unit::None)
            | Value::Dimension(n, Unit::Percent) => n / Number::from(100),
            _ => todo!("non-number weight given to builtin function `invert()`")
        };
        match arg!(args, 0, "color") {
            Value::Color(c) => Some(Value::Color(c.invert(weight))),
            _ => todo!("non-color given to builtin function `invert()`")
        }
    });
}
