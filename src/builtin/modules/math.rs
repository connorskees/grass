use std::cmp::Ordering;

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
    todo!()
}

fn pow(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(2)?;
    todo!()
}

fn sqrt(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    todo!()
}

fn cos(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    todo!()
}

fn sin(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    todo!()
}

fn tan(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    todo!()
}

fn acos(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    todo!()
}

fn asin(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    todo!()
}

fn atan(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    todo!()
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
    #[cfg(feature = "random")]
    f.insert_builtin("random", random);

    f.insert_builtin_var(
        "pi",
        Value::Dimension(Number::from(std::f64::consts::PI), Unit::None, true),
    );
    f.insert_builtin_var(
        "e",
        Value::Dimension(Number::from(std::f64::consts::E), Unit::None, true),
    );
}
