use std::cmp::Ordering;

use num_traits::{One, Signed, Zero};

use crate::{
    args::CallArgs,
    builtin::{
        math::{abs, ceil, comparable, floor, max, min, percentage, round},
        meta::{unit, unitless},
        modules::Module,
    },
    common::Op,
    error::SassResult,
    parse::Parser,
    unit::Unit,
    value::{Number, Value},
};

#[cfg(feature = "random")]
use crate::builtin::math::random;

fn clamp(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(3)?;
    let span = args.span();

    let min = match args.get_err(0, "min")? {
        v @ Value::Dimension(..) => v,
        v => {
            return Err((
                format!("$min: {} is not a number.", v.inspect(args.span())?),
                span,
            )
                .into())
        }
    };

    let number = match args.get_err(1, "number")? {
        v @ Value::Dimension(..) => v,
        v => {
            return Err((
                format!("$number: {} is not a number.", v.inspect(span)?),
                span,
            )
                .into())
        }
    };

    let max = match args.get_err(2, "max")? {
        v @ Value::Dimension(..) => v,
        v => return Err((format!("$max: {} is not a number.", v.inspect(span)?), span).into()),
    };

    // ensure that `min` and `max` are compatible
    min.cmp(&max, span, Op::LessThan)?;

    let min_unit = match min {
        Value::Dimension(_, ref u, _) => u,
        _ => unreachable!(),
    };
    let number_unit = match number {
        Value::Dimension(_, ref u, _) => u,
        _ => unreachable!(),
    };
    let max_unit = match max {
        Value::Dimension(_, ref u, _) => u,
        _ => unreachable!(),
    };

    if min_unit == &Unit::None && number_unit != &Unit::None {
        return Err((
            format!(
                "$min is unitless but $number has unit {}. Arguments must all have units or all be unitless.",
                number_unit
            ), span).into());
    } else if min_unit != &Unit::None && number_unit == &Unit::None {
        return Err((
                format!(
                    "$min has unit {} but $number is unitless. Arguments must all have units or all be unitless.",
                    min_unit
                ), span).into());
    } else if min_unit != &Unit::None && max_unit == &Unit::None {
        return Err((
            format!(
                "$min has unit {} but $max is unitless. Arguments must all have units or all be unitless.",
                min_unit
            ), span).into());
    }

    match min.cmp(&number, span, Op::LessThan)? {
        Ordering::Greater => return Ok(min),
        Ordering::Equal => return Ok(number),
        Ordering::Less => {}
    }

    match max.cmp(&number, span, Op::GreaterThan)? {
        Ordering::Less => return Ok(max),
        Ordering::Equal => return Ok(number),
        Ordering::Greater => {}
    }

    Ok(number)
}

fn hypot(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    todo!()
}

fn log(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(2)?;

    let number = match args.get_err(0, "number")? {
        Value::Dimension(Some(n), Unit::None, ..) => n,
        v @ Value::Dimension(Some(..), ..) => {
            return Err((
                format!(
                    "$number: Expected {} to be unitless.",
                    v.inspect(args.span())?
                ),
                args.span(),
            )
                .into())
        }
        v @ Value::Dimension(None, ..) => return Ok(v),
        v => {
            return Err((
                format!("$number: {} is not a number.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    let base = match args.default_arg(1, "base", Value::Null)? {
        Value::Null => None,
        Value::Dimension(Some(n), Unit::None, ..) => Some(n),
        v @ Value::Dimension(Some(..), ..) => {
            return Err((
                format!(
                    "$number: Expected {} to be unitless.",
                    v.inspect(args.span())?
                ),
                args.span(),
            )
                .into())
        }
        v @ Value::Dimension(None, ..) => return Ok(v),
        v => {
            return Err((
                format!("$base: {} is not a number.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    Ok(Value::Dimension(
        if let Some(base) = base {
            if base.is_zero() {
                Some(Number::zero())
            } else {
                (|| Some(number.ln()? / base.ln()?))()
            }
        } else if number.is_negative() {
            None
        } else if number.is_zero() {
            todo!()
        } else {
            number.ln()
        },
        Unit::None,
        true,
    ))
}

fn pow(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(2)?;
    todo!()
}

fn sqrt(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    let number = args.get_err(0, "number")?;

    Ok(match number {
        Value::Dimension(Some(n), Unit::None, ..) => Value::Dimension(n.sqrt(), Unit::None, true),
        v @ Value::Dimension(Some(..), ..) => {
            return Err((
                format!(
                    "$number: Expected {} to have no units.",
                    v.inspect(args.span())?
                ),
                args.span(),
            )
                .into())
        }
        Value::Dimension(None, ..) => Value::Dimension(None, Unit::None, true),
        v => {
            return Err((
                format!("$number: {} is not a number.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    })
}

macro_rules! trig_fn {
    ($name:ident, $name_deg:ident) => {
        fn $name(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
            args.max_args(1)?;
            let number = args.get_err(0, "number")?;

            Ok(match number {
                Value::Dimension(Some(n), Unit::None, ..)
                | Value::Dimension(Some(n), Unit::Rad, ..) => {
                    Value::Dimension(n.$name(), Unit::None, true)
                }
                Value::Dimension(Some(n), Unit::Deg, ..) => {
                    Value::Dimension(n.$name_deg(), Unit::None, true)
                }
                v @ Value::Dimension(Some(..), ..) => {
                    return Err((
                        format!(
                            "$number: Expected {} to be an angle.",
                            v.inspect(args.span())?
                        ),
                        args.span(),
                    )
                        .into())
                }
                Value::Dimension(None, ..) => Value::Dimension(None, Unit::None, true),
                v => {
                    return Err((
                        format!("$number: {} is not a number.", v.inspect(args.span())?),
                        args.span(),
                    )
                        .into())
                }
            })
        }
    };
}

trig_fn!(cos, cos_deg);
trig_fn!(sin, sin_deg);
trig_fn!(tan, tan_deg);

fn acos(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    let number = args.get_err(0, "number")?;

    Ok(match number {
        Value::Dimension(Some(n), Unit::None, ..) => Value::Dimension(
            if n > Number::from(1) || n < Number::from(-1) {
                None
            } else if n.is_one() {
                Some(Number::zero())
            } else {
                n.acos()
            },
            Unit::Deg,
            true,
        ),
        v @ Value::Dimension(Some(..), ..) => {
            return Err((
                format!(
                    "$number: Expected {} to be unitless.",
                    v.inspect(args.span())?
                ),
                args.span(),
            )
                .into())
        }
        Value::Dimension(None, ..) => Value::Dimension(None, Unit::Deg, true),
        v => {
            return Err((
                format!("$number: {} is not a number.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    })
}

fn asin(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    let number = args.get_err(0, "number")?;

    Ok(match number {
        Value::Dimension(Some(n), Unit::None, ..) => {
            if n > Number::from(1) || n < Number::from(-1) {
                return Ok(Value::Dimension(None, Unit::Deg, true));
            } else if n.is_zero() {
                return Ok(Value::Dimension(Some(Number::zero()), Unit::Deg, true));
            }

            Value::Dimension(n.asin(), Unit::Deg, true)
        }
        v @ Value::Dimension(Some(..), ..) => {
            return Err((
                format!(
                    "$number: Expected {} to be unitless.",
                    v.inspect(args.span())?
                ),
                args.span(),
            )
                .into())
        }
        Value::Dimension(None, ..) => Value::Dimension(None, Unit::Deg, true),
        v => {
            return Err((
                format!("$number: {} is not a number.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    })
}

fn atan(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    let number = args.get_err(0, "number")?;

    Ok(match number {
        Value::Dimension(Some(n), Unit::None, ..) => {
            if n.is_zero() {
                return Ok(Value::Dimension(Some(Number::zero()), Unit::Deg, true));
            }

            Value::Dimension(n.atan(), Unit::Deg, true)
        }
        v @ Value::Dimension(Some(..), ..) => {
            return Err((
                format!(
                    "$number: Expected {} to be unitless.",
                    v.inspect(args.span())?
                ),
                args.span(),
            )
                .into())
        }
        Value::Dimension(None, ..) => Value::Dimension(None, Unit::Deg, true),
        v => {
            return Err((
                format!("$number: {} is not a number.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    })
}

fn atan2(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(2)?;
    todo!()
}

pub(crate) fn declare(f: &mut Module) {
    f.insert_builtin("ceil", ceil);
    f.insert_builtin("floor", floor);
    f.insert_builtin("max", max);
    f.insert_builtin("min", min);
    f.insert_builtin("round", round);
    f.insert_builtin("abs", abs);
    f.insert_builtin("compatible", comparable);
    f.insert_builtin("is-unitless", unitless);
    f.insert_builtin("unit", unit);
    f.insert_builtin("percentage", percentage);
    f.insert_builtin("clamp", clamp);
    f.insert_builtin("sqrt", sqrt);
    f.insert_builtin("cos", cos);
    f.insert_builtin("sin", sin);
    f.insert_builtin("tan", tan);
    f.insert_builtin("acos", acos);
    f.insert_builtin("asin", asin);
    f.insert_builtin("atan", atan);
    f.insert_builtin("log", log);
    #[cfg(feature = "random")]
    f.insert_builtin("random", random);

    f.insert_builtin_var(
        "pi",
        Value::Dimension(Some(Number::from(std::f64::consts::PI)), Unit::None, true),
    );
    f.insert_builtin_var(
        "e",
        Value::Dimension(Some(Number::from(std::f64::consts::E)), Unit::None, true),
    );
}
