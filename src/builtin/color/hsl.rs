use std::collections::BTreeMap;

use super::Builtin;
use crate::color::Color;
use crate::common::QuoteKind;
use crate::units::Unit;
use crate::value::{Number, Value};

pub(crate) fn register(f: &mut BTreeMap<String, Builtin>) {
    decl!(f "hsl", |args, _| {
        let hue = match arg!(args, 0, "hue") {
            Value::Dimension(n, _) => n,
            v => return Err(format!("$hue: {} is not a number.", v).into()),
        };
        let saturation = match arg!(args, 1, "saturation") {
            Value::Dimension(n, _) => n / Number::from(100),
            v => return Err(format!("$saturation: {} is not a number.", v).into()),
        };
        let luminance = match arg!(args, 2, "luminance") {
            Value::Dimension(n, _) => n / Number::from(100),
            v => return Err(format!("$luminance: {} is not a number.", v).into()),
        };
        let alpha = match arg!(args, 3, "alpha"=Value::Dimension(Number::from(1), Unit::None)) {
            Value::Dimension(n, Unit::None) => n,
            Value::Dimension(n, Unit::Percent) => n / Number::from(100),
            v @ Value::Dimension(..) => return Err(format!("$alpha: Expected {} to have no units or \"%\".", v).into()),
            v => return Err(format!("$alpha: {} is not a number.", v).into()),
        };
        Ok(Value::Color(Color::from_hsla(hue, saturation, luminance, alpha)))
    });
    decl!(f "hsla", |args, _| {
        let hue = match arg!(args, 0, "hue") {
            Value::Dimension(n, _) => n,
            v => return Err(format!("$hue: {} is not a number.", v).into()),
        };
        let saturation = match arg!(args, 1, "saturation") {
            Value::Dimension(n, _) => n / Number::from(100),
            v => return Err(format!("$saturation: {} is not a number.", v).into()),
        };
        let luminance = match arg!(args, 2, "luminance") {
            Value::Dimension(n, _) => n / Number::from(100),
            v => return Err(format!("$luminance: {} is not a number.", v).into()),
        };
        let alpha = match arg!(args, 3, "alpha") {
            Value::Dimension(n, Unit::None) => n,
            Value::Dimension(n, Unit::Percent) => n / Number::from(100),
            v @ Value::Dimension(..) => return Err(format!("$alpha: Expected {} to have no units or \"%\".", v).into()),
            v => return Err(format!("$alpha: {} is not a number.", v).into()),
        };
        Ok(Value::Color(Color::from_hsla(hue, saturation, luminance, alpha)))
    });
    decl!(f "hue", |args, _| {
        max_args!(args, 1);
        match arg!(args, 0, "color") {
            Value::Color(c) => Ok(Value::Dimension(c.hue(), Unit::Deg)),
            v => Err(format!("$color: {} is not a color.", v).into()),
        }
    });
    decl!(f "saturation", |args, _| {
        max_args!(args, 1);
        match arg!(args, 0, "color") {
            Value::Color(c) => Ok(Value::Dimension(c.saturation(), Unit::Percent)),
            v => Err(format!("$color: {} is not a color.", v).into()),
        }
    });
    decl!(f "lightness", |args, _| {
        max_args!(args, 1);
        match arg!(args, 0, "color") {
            Value::Color(c) => Ok(Value::Dimension(c.lightness(), Unit::Percent)),
            v => Err(format!("$color: {} is not a color.", v).into()),
        }
    });
    decl!(f "adjust-hue", |args, _| {
        max_args!(args, 2);
        let color = match arg!(args, 0, "color") {
            Value::Color(c) => c,
            v => return Err(format!("$color: {} is not a color.", v).into()),
        };
        let degrees = match arg!(args, 1, "degrees") {
            Value::Dimension(n, _) => n,
            v => return Err(format!("$degrees: {} is not a number.", v).into()),
        };
        Ok(Value::Color(color.adjust_hue(degrees)))
    });
    decl!(f "lighten", |args, _| {
        max_args!(args, 2);
        let color = match arg!(args, 0, "color") {
            Value::Color(c) => c,
            v => return Err(format!("$color: {} is not a color.", v).into()),
        };
        let amount = match arg!(args, 1, "amount") {
            Value::Dimension(n, u) => bound!("amount", n, u, 0, 100) / Number::from(100),
            v => return Err(format!("$amount: {} is not a number.", v).into())
        };
        Ok(Value::Color(color.lighten(amount)))
    });
    decl!(f "darken", |args, _| {
        max_args!(args, 2);
        let color = match arg!(args, 0, "color") {
            Value::Color(c) => c,
            v => return Err(format!("$color: {} is not a color.", v).into()),
        };
        let amount = match arg!(args, 1, "amount") {
            Value::Dimension(n, u) => bound!("amount", n, u, 0, 100) / Number::from(100),
            v => return Err(format!("$amount: {} is not a number.", v).into())
        };
        Ok(Value::Color(color.darken(amount)))
    });
    decl!(f "saturate", |args, _| {
        max_args!(args, 2);
        let amount = match arg!(args, 1, "amount") {
            Value::Dimension(n, u) => bound!("amount", n, u, 0, 100) / Number::from(100),
            v => return Err(format!("$amount: {} is not a number.", v).into())
        };
        let color = match arg!(args, 0, "color") {
            Value::Color(c) => c,
            Value::Dimension(n, u) => return Ok(Value::Ident(format!("saturate({}{})", n, u), QuoteKind::None)),
            v => return Err(format!("$color: {} is not a color.", v).into()),
        };
        Ok(Value::Color(color.saturate(amount)))
    });
    decl!(f "desaturate", |args, _| {
        max_args!(args, 2);
        let color = match arg!(args, 0, "color") {
            Value::Color(c) => c,
            v => return Err(format!("$color: {} is not a color.", v).into()),
        };
        let amount = match arg!(args, 1, "amount") {
            Value::Dimension(n, u) => bound!("amount", n, u, 0, 100) / Number::from(100),
            v => return Err(format!("$amount: {} is not a number.", v).into())
        };
        Ok(Value::Color(color.desaturate(amount)))
    });
    decl!(f "grayscale", |args, _| {
        max_args!(args, 1);
        let color = match arg!(args, 0, "color") {
            Value::Color(c) => c,
            Value::Dimension(n, u) => return Ok(Value::Ident(format!("grayscale({}{})", n, u), QuoteKind::None)),
            v => return Err(format!("$color: {} is not a color.", v).into()),
        };
        Ok(Value::Color(color.desaturate(Number::from(1))))
    });
    decl!(f "complement", |args, _| {
        max_args!(args, 1);
        let color = match arg!(args, 0, "color") {
            Value::Color(c) => c,
            v => return Err(format!("$color: {} is not a color.", v).into()),
        };
        Ok(Value::Color(color.complement()))
    });
    decl!(f "invert", |args, _| {
        max_args!(args, 2);
        let weight = match arg!(args, 1, "weight"=Value::Dimension(Number::from(100), Unit::Percent)) {
            Value::Dimension(n, u) => bound!("weight", n, u, 0, 100) / Number::from(100),
            v => return Err(format!("$weight: {} is not a number.", v).into()),
        };
        match arg!(args, 0, "color") {
            Value::Color(c) => Ok(Value::Color(c.invert(weight))),
            Value::Dimension(n, Unit::Percent) => Ok(Value::Ident(format!("invert({}%)", n), QuoteKind::None)),
            Value::Dimension(..) => Err("Only one argument may be passed to the plain-CSS invert() function.".into()),
            v => Err(format!("$color: {} is not a color.", v).into()),
        }
    });
}
