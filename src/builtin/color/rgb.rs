use std::collections::BTreeMap;

use super::Builtin;
use crate::color::Color;
use crate::units::Unit;
use crate::value::{Number, Value};

pub(crate) fn register(f: &mut BTreeMap<String, Builtin>) {
    decl!(f "rgb", |args, _| {
        if args.len() == 1 {
            let mut channels = match arg!(args, 0, "channels").eval() {
                Value::List(v, _) => v,
                _ => return Err("Missing element $green.".into())
            };

            assert_eq!(channels.len(), 3_usize);

            let blue = match channels.pop() {
                Some(Value::Dimension(n, Unit::None)) => n,
                Some(Value::Dimension(n, Unit::Percent)) => n / Number::from(100),
                Some(v) => return Err(format!("$blue: {} is not a color", v).into()),
                None => return Err("Missing element $blue.".into()),
            };

            let green = match channels.pop() {
                Some(Value::Dimension(n, Unit::None)) => n,
                Some(Value::Dimension(n, Unit::Percent)) => n / Number::from(100),
                Some(v) => return Err(format!("$green: {} is not a color", v).into()),
                None => return Err("Missing element $green.".into()),
            };

            let red = match channels.pop() {
                Some(Value::Dimension(n, Unit::None)) => n,
                Some(Value::Dimension(n, Unit::Percent)) => n / Number::from(100),
                Some(v) => return Err(format!("$red: {} is not a color", v).into()),
                None => return Err("Missing element $red.".into()),
            };

            let color = Color::from_rgba(red, green, blue, Number::from(1));

            Ok(Value::Color(color))

        } else if args.len() == 2 {
            let color = match arg!(args, 0, "color").eval() {
                Value::Color(c) => c,
                v => return Err(format!("$color: {} is not a color.", v).into()),
            };
            let alpha = match arg!(args, 1, "alpha").eval() {
                Value::Dimension(n, Unit::None) => n,
                Value::Dimension(n, Unit::Percent) => n / Number::from(100),
                v @ Value::Dimension(..) => return Err(format!("$alpha: Expected {} to have no units or \"%\".", v).into()),
                v => return Err(format!("$alpha: {} is not a number.", v).into()),
            };
            Ok(Value::Color(color.with_alpha(alpha)))
        } else {
            let red = match arg!(args, 0, "red").eval() {
                Value::Dimension(n, Unit::None) => n,
                Value::Dimension(n, Unit::Percent) => (n / Number::from(100)) * Number::from(255),
                v @ Value::Dimension(..) => return Err(format!("$red: Expected {} to have no units or \"%\".", v).into()),
                v => return Err(format!("$red: {} is not a number.", v).into()),
            };
            let green = match arg!(args, 1, "green").eval() {
                Value::Dimension(n, Unit::None) => n,
                Value::Dimension(n, Unit::Percent) => (n / Number::from(100)) * Number::from(255),
                v @ Value::Dimension(..) => return Err(format!("$green: Expected {} to have no units or \"%\".", v).into()),
                v => return Err(format!("$green: {} is not a number.", v).into()),
            };
            let blue = match arg!(args, 2, "blue").eval() {
                Value::Dimension(n, Unit::None) => n,
                Value::Dimension(n, Unit::Percent) => (n / Number::from(100)) * Number::from(255),
                v @ Value::Dimension(..) => return Err(format!("$blue: Expected {} to have no units or \"%\".", v).into()),
                v => return Err(format!("$blue: {} is not a number.", v).into()),
            };
            let alpha = match arg!(args, 3, "alpha"=Value::Dimension(Number::from(1), Unit::None)).eval() {
                Value::Dimension(n, Unit::None) => n,
                Value::Dimension(n, Unit::Percent) => n / Number::from(100),
                v @ Value::Dimension(..) => return Err(format!("$alpha: Expected {} to have no units or \"%\".", v).into()),
                v => return Err(format!("$alpha: {} is not a number.", v).into()),
            };
            Ok(Value::Color(Color::from_rgba(red, green, blue, alpha)))
        }
    });
    decl!(f "rgba", |args, _| {
        if args.len() == 1 {
            let mut channels = match arg!(args, 0, "channels").eval() {
                Value::List(v, _) => v,
                _ => return Err("Missing element $green.".into())
            };

            assert_eq!(channels.len(), 3_usize);

            let blue = match channels.pop() {
                Some(Value::Dimension(n, Unit::None)) => n,
                Some(Value::Dimension(n, Unit::Percent)) => n / Number::from(100),
                Some(v) => return Err(format!("$blue: {} is not a color", v).into()),
                None => return Err("Missing element $blue.".into()),
            };

            let green = match channels.pop() {
                Some(Value::Dimension(n, Unit::None)) => n,
                Some(Value::Dimension(n, Unit::Percent)) => n / Number::from(100),
                Some(v) => return Err(format!("$green: {} is not a color", v).into()),
                None => return Err("Missing element $green.".into()),
            };

            let red = match channels.pop() {
                Some(Value::Dimension(n, Unit::None)) => n,
                Some(Value::Dimension(n, Unit::Percent)) => n / Number::from(100),
                Some(v) => return Err(format!("$red: {} is not a color", v).into()),
                None => return Err("Missing element $red.".into()),
            };

            let color = Color::from_rgba(red, green, blue, Number::from(1));

            Ok(Value::Color(color))

        } else if args.len() == 2 {
            let color = match arg!(args, 0, "color").eval() {
                Value::Color(c) => c,
                v => return Err(format!("$color: {} is not a color.", v).into()),
            };
            let alpha = match arg!(args, 1, "alpha").eval() {
                Value::Dimension(n, Unit::None) => n,
                Value::Dimension(n, Unit::Percent) => n / Number::from(100),
                v @ Value::Dimension(..) => return Err(format!("$alpha: Expected {} to have no units or \"%\".", v).into()),
                v => return Err(format!("$alpha: {} is not a number.", v).into()),
            };
            Ok(Value::Color(color.with_alpha(alpha)))
        } else {
            let red = match arg!(args, 0, "red").eval() {
                Value::Dimension(n, Unit::None) => n,
                Value::Dimension(n, Unit::Percent) => (n / Number::from(100)) * Number::from(255),
                v @ Value::Dimension(..) => return Err(format!("$red: Expected {} to have no units or \"%\".", v).into()),
                v => return Err(format!("$red: {} is not a number.", v).into()),
            };
            let green = match arg!(args, 1, "green").eval() {
                Value::Dimension(n, Unit::None) => n,
                Value::Dimension(n, Unit::Percent) => (n / Number::from(100)) * Number::from(255),
                v @ Value::Dimension(..) => return Err(format!("$green: Expected {} to have no units or \"%\".", v).into()),
                v => return Err(format!("$green: {} is not a number.", v).into()),
            };
            let blue = match arg!(args, 2, "blue").eval() {
                Value::Dimension(n, Unit::None) => n,
                Value::Dimension(n, Unit::Percent) => (n / Number::from(100)) * Number::from(255),
                v @ Value::Dimension(..) => return Err(format!("$blue: Expected {} to have no units or \"%\".", v).into()),
                v => return Err(format!("$blue: {} is not a number.", v).into()),
            };
            let alpha = match arg!(args, 3, "alpha"=Value::Dimension(Number::from(1), Unit::None)).eval() {
                Value::Dimension(n, Unit::None) => n,
                Value::Dimension(n, Unit::Percent) => n / Number::from(100),
                v @ Value::Dimension(..) => return Err(format!("$alpha: Expected {} to have no units or \"%\".", v).into()),
                v => return Err(format!("$alpha: {} is not a number.", v).into()),
            };
            Ok(Value::Color(Color::from_rgba(red, green, blue, alpha)))
        }
    });
    decl!(f "red", |args, _| {
        max_args!(args, 1);
        match arg!(args, 0, "color") {
            Value::Color(c) => Ok(Value::Dimension(c.red(), Unit::None)),
            v => return Err(format!("$color: {} is not a color.", v).into()),
        }
    });
    decl!(f "green", |args, _| {
        max_args!(args, 1);
        match arg!(args, 0, "color") {
            Value::Color(c) => Ok(Value::Dimension(c.green(), Unit::None)),
            v => return Err(format!("$color: {} is not a color.", v).into()),
        }
    });
    decl!(f "blue", |args, _| {
        max_args!(args, 1);
        match arg!(args, 0, "color") {
            Value::Color(c) => Ok(Value::Dimension(c.blue(), Unit::None)),
            v => return Err(format!("$color: {} is not a color.", v).into()),
        }
    });
    decl!(f "mix", |args, _| {
        max_args!(args, 3);
        let color1 = match arg!(args, 0, "color1").eval() {
            Value::Color(c) => c,
            v => return Err(format!("$color1: {} is not a color.", v).into()),
        };

        let color2 = match arg!(args, 1, "color2").eval() {
            Value::Color(c) => c,
            v => return Err(format!("$color2: {} is not a color.", v).into()),
        };

        let weight = match arg!(args, 2, "weight"=Value::Dimension(Number::from(50), Unit::None)) {
            Value::Dimension(n, u) => bound!("weight", n, u, 0, 100) / Number::from(100),
            v => return Err(format!("$weight: {} is not a number.", v).into()),
        };
        Ok(Value::Color(color1.mix(color2, weight)))
    });
}
