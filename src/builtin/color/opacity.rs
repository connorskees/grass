use std::collections::BTreeMap;

use super::Builtin;
use crate::common::QuoteKind;
use crate::units::Unit;
use crate::value::{Number, Value};

pub(crate) fn register(f: &mut BTreeMap<String, Builtin>) {
    decl!(f "alpha", |args, _| {
        match arg!(args, 0, "color") {
            Value::Color(c) => Some(Value::Dimension(c.alpha(), Unit::None)),
            _ => todo!("non-color given to builtin function `alpha()`")
        }
    });
    decl!(f "opacity", |args, _| {
        match arg!(args, 0, "color") {
            Value::Color(c) => Some(Value::Dimension(c.alpha(), Unit::None)),
            Value::Dimension(num, unit) => Some(Value::Ident(format!("opacity({}{})", num , unit), QuoteKind::None)),
            _ => todo!("non-color given to builtin function `opacity()`")
        }
    });
    decl!(f "opacify", |args, _| {
        let color = match arg!(args, 0, "color").eval() {
            Value::Color(c) => c,
            _ => todo!("non-color given to builtin function `opacify()`")
        };
        let amount = match arg!(args, 1, "amount").eval() {
            Value::Dimension(n, Unit::None) => n,
            Value::Dimension(n, Unit::Percent) => n / Number::from(100),
            _ => todo!("expected either unitless or % number for amount"),
        };
        Some(Value::Color(color.fade_in(amount)))
    });
    decl!(f "fade-in", |args, _| {
        let color = match arg!(args, 0, "color").eval() {
            Value::Color(c) => c,
            _ => todo!("non-color given to builtin function `fade-in()`")
        };
        let amount = match arg!(args, 1, "amount").eval() {
            Value::Dimension(n, Unit::None) => n,
            Value::Dimension(n, Unit::Percent) => n / Number::from(100),
            _ => todo!("expected either unitless or % number for amount"),
        };
        Some(Value::Color(color.fade_in(amount)))
    });
    decl!(f "transparentize", |args, _| {
        let color = match arg!(args, 0, "color").eval() {
            Value::Color(c) => c,
            _ => todo!("non-color given to builtin function `transparentize()`")
        };
        let amount = match arg!(args, 1, "amount").eval() {
            Value::Dimension(n, Unit::None) => n,
            Value::Dimension(n, Unit::Percent) => n / Number::from(100),
            _ => todo!("expected either unitless or % number for amount"),
        };
        Some(Value::Color(color.fade_out(amount)))
    });
    decl!(f "fade-out", |args, _| {
        let color = match arg!(args, 0, "color").eval() {
            Value::Color(c) => c,
            _ => todo!("non-color given to builtin function `fade-out()`")
        };
        let amount = match arg!(args, 1, "amount").eval() {
            Value::Dimension(n, Unit::None) => n,
            Value::Dimension(n, Unit::Percent) => n / Number::from(100),
            _ => todo!("expected either unitless or % number for amount"),
        };
        Some(Value::Color(color.fade_out(amount)))
    });
}
