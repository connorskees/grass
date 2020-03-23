use std::collections::HashMap;

use num_traits::One;

use super::Builtin;
use crate::color::Color;
use crate::common::QuoteKind;
use crate::unit::Unit;
use crate::value::{Number, Value};

pub(crate) fn register(f: &mut HashMap<String, Builtin>) {
    f.insert(
        "hsl".to_owned(),
        Box::new(|args, _| {
            if args.len() == 1 {
                let mut channels = match arg!(args, 0, "channels") {
                    Value::List(v, _) => v,
                    _ => return Err("Missing element $green.".into()),
                };

                if channels.len() > 3 {
                    return Err(format!(
                        "Only 3 elements allowed, but {} were passed.",
                        channels.len()
                    )
                    .into());
                }

                let luminance = match channels.pop() {
                    Some(Value::Dimension(n, _)) => n / Number::from(100),
                    Some(v) => return Err(format!("$luminance: {} is not a color", v).into()),
                    None => return Err("Missing element $luminance.".into()),
                };

                let saturation = match channels.pop() {
                    Some(Value::Dimension(n, _)) => n / Number::from(100),
                    Some(v) => return Err(format!("$saturation: {} is not a color", v).into()),
                    None => return Err("Missing element $saturation.".into()),
                };

                let hue = match channels.pop() {
                    Some(Value::Dimension(n, _)) => n,
                    Some(v) => return Err(format!("$hue: {} is not a color", v).into()),
                    None => return Err("Missing element $hue.".into()),
                };

                Ok(Value::Color(Color::from_hsla(
                    hue,
                    saturation,
                    luminance,
                    Number::one(),
                )))
            } else {
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
                let alpha = match arg!(
                    args,
                    3,
                    "alpha" = Value::Dimension(Number::one(), Unit::None)
                ) {
                    Value::Dimension(n, Unit::None) => n,
                    Value::Dimension(n, Unit::Percent) => n / Number::from(100),
                    v @ Value::Dimension(..) => {
                        return Err(
                            format!("$alpha: Expected {} to have no units or \"%\".", v).into()
                        )
                    }
                    v => return Err(format!("$alpha: {} is not a number.", v).into()),
                };
                Ok(Value::Color(Color::from_hsla(
                    hue, saturation, luminance, alpha,
                )))
            }
        }),
    );
    f.insert(
        "hsla".to_owned(),
        Box::new(|args, _| {
            if args.len() == 1 {
                let mut channels = match arg!(args, 0, "channels") {
                    Value::List(v, _) => v,
                    _ => return Err("Missing element $green.".into()),
                };

                if channels.len() > 3 {
                    return Err(format!(
                        "Only 3 elements allowed, but {} were passed.",
                        channels.len()
                    )
                    .into());
                }

                let luminance = match channels.pop() {
                    Some(Value::Dimension(n, _)) => n / Number::from(100),
                    Some(v) => return Err(format!("$luminance: {} is not a color", v).into()),
                    None => return Err("Missing element $luminance.".into()),
                };

                let saturation = match channels.pop() {
                    Some(Value::Dimension(n, _)) => n / Number::from(100),
                    Some(v) => return Err(format!("$saturation: {} is not a color", v).into()),
                    None => return Err("Missing element $saturation.".into()),
                };

                let hue = match channels.pop() {
                    Some(Value::Dimension(n, _)) => n,
                    Some(v) => return Err(format!("$hue: {} is not a color", v).into()),
                    None => return Err("Missing element $hue.".into()),
                };

                Ok(Value::Color(Color::from_hsla(
                    hue,
                    saturation,
                    luminance,
                    Number::one(),
                )))
            } else {
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
                let alpha = match arg!(
                    args,
                    3,
                    "alpha" = Value::Dimension(Number::one(), Unit::None)
                ) {
                    Value::Dimension(n, Unit::None) => n,
                    Value::Dimension(n, Unit::Percent) => n / Number::from(100),
                    v @ Value::Dimension(..) => {
                        return Err(
                            format!("$alpha: Expected {} to have no units or \"%\".", v).into()
                        )
                    }
                    v => return Err(format!("$alpha: {} is not a number.", v).into()),
                };
                Ok(Value::Color(Color::from_hsla(
                    hue, saturation, luminance, alpha,
                )))
            }
        }),
    );
    f.insert(
        "hue".to_owned(),
        Box::new(|args, _| {
            max_args!(args, 1);
            match arg!(args, 0, "color") {
                Value::Color(c) => Ok(Value::Dimension(c.hue(), Unit::Deg)),
                v => Err(format!("$color: {} is not a color.", v).into()),
            }
        }),
    );
    f.insert(
        "saturation".to_owned(),
        Box::new(|args, _| {
            max_args!(args, 1);
            match arg!(args, 0, "color") {
                Value::Color(c) => Ok(Value::Dimension(c.saturation(), Unit::Percent)),
                v => Err(format!("$color: {} is not a color.", v).into()),
            }
        }),
    );
    f.insert(
        "lightness".to_owned(),
        Box::new(|args, _| {
            max_args!(args, 1);
            match arg!(args, 0, "color") {
                Value::Color(c) => Ok(Value::Dimension(c.lightness(), Unit::Percent)),
                v => Err(format!("$color: {} is not a color.", v).into()),
            }
        }),
    );
    f.insert(
        "adjust-hue".to_owned(),
        Box::new(|args, _| {
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
        }),
    );
    f.insert(
        "lighten".to_owned(),
        Box::new(|args, _| {
            max_args!(args, 2);
            let color = match arg!(args, 0, "color") {
                Value::Color(c) => c,
                v => return Err(format!("$color: {} is not a color.", v).into()),
            };
            let amount = match arg!(args, 1, "amount") {
                Value::Dimension(n, u) => bound!("amount", n, u, 0, 100) / Number::from(100),
                v => return Err(format!("$amount: {} is not a number.", v).into()),
            };
            Ok(Value::Color(color.lighten(amount)))
        }),
    );
    f.insert(
        "darken".to_owned(),
        Box::new(|args, _| {
            max_args!(args, 2);
            let color = match arg!(args, 0, "color") {
                Value::Color(c) => c,
                v => return Err(format!("$color: {} is not a color.", v).into()),
            };
            let amount = match arg!(args, 1, "amount") {
                Value::Dimension(n, u) => bound!("amount", n, u, 0, 100) / Number::from(100),
                v => return Err(format!("$amount: {} is not a number.", v).into()),
            };
            Ok(Value::Color(color.darken(amount)))
        }),
    );
    f.insert(
        "saturate".to_owned(),
        Box::new(|args, _| {
            max_args!(args, 2);
            if args.len() == 1 {
                return Ok(Value::Ident(format!("saturate({})", arg!(args, 0, "amount")), QuoteKind::None));
            }

            let amount = match arg!(args, 1, "amount") {
                Value::Dimension(n, u) => bound!("amount", n, u, 0, 100) / Number::from(100),
                v => return Err(format!("$amount: {} is not a number.", v).into()),
            };
            let color = match arg!(args, 0, "color") {
                Value::Color(c) => c,
                Value::Dimension(n, u) => {
                    return Ok(Value::Ident(
                        format!("saturate({}{})", n, u),
                        QuoteKind::None,
                    ))
                }
                v => return Err(format!("$color: {} is not a color.", v).into()),
            };
            Ok(Value::Color(color.saturate(amount)))
        }),
    );
    f.insert(
        "desaturate".to_owned(),
        Box::new(|args, _| {
            max_args!(args, 2);
            let color = match arg!(args, 0, "color") {
                Value::Color(c) => c,
                v => return Err(format!("$color: {} is not a color.", v).into()),
            };
            let amount = match arg!(args, 1, "amount") {
                Value::Dimension(n, u) => bound!("amount", n, u, 0, 100) / Number::from(100),
                v => return Err(format!("$amount: {} is not a number.", v).into()),
            };
            Ok(Value::Color(color.desaturate(amount)))
        }),
    );
    f.insert(
        "grayscale".to_owned(),
        Box::new(|args, _| {
            max_args!(args, 1);
            let color = match arg!(args, 0, "color") {
                Value::Color(c) => c,
                Value::Dimension(n, u) => {
                    return Ok(Value::Ident(
                        format!("grayscale({}{})", n, u),
                        QuoteKind::None,
                    ))
                }
                v => return Err(format!("$color: {} is not a color.", v).into()),
            };
            Ok(Value::Color(color.desaturate(Number::one())))
        }),
    );
    f.insert(
        "complement".to_owned(),
        Box::new(|args, _| {
            max_args!(args, 1);
            let color = match arg!(args, 0, "color") {
                Value::Color(c) => c,
                v => return Err(format!("$color: {} is not a color.", v).into()),
            };
            Ok(Value::Color(color.complement()))
        }),
    );
    f.insert(
        "invert".to_owned(),
        Box::new(|args, _| {
            max_args!(args, 2);
            let weight = match arg!(
                args,
                1,
                "weight" = Value::Dimension(Number::from(100), Unit::Percent)
            ) {
                Value::Dimension(n, u) => bound!("weight", n, u, 0, 100) / Number::from(100),
                v => return Err(format!("$weight: {} is not a number.", v).into()),
            };
            match arg!(args, 0, "color") {
                Value::Color(c) => Ok(Value::Color(c.invert(weight))),
                Value::Dimension(n, Unit::Percent) => {
                    Ok(Value::Ident(format!("invert({}%)", n), QuoteKind::None))
                }
                Value::Dimension(..) => Err(
                    "Only one argument may be passed to the plain-CSS invert() function.".into(),
                ),
                v => Err(format!("$color: {} is not a color.", v).into()),
            }
        }),
    );
}
