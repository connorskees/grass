use super::{Builtin, GlobalFunctionMap};

#[cfg(feature = "random")]
use num_traits::{One, Signed, ToPrimitive, Zero};
#[cfg(feature = "random")]
use rand::Rng;

use crate::{
    args::CallArgs,
    error::SassResult,
    parse::{HigherIntermediateValue, Parser, ValueVisitor},
    unit::Unit,
    value::{Number, Value},
};

pub(crate) fn percentage(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    let num = match args.get_err(0, "number")? {
        Value::Dimension(Some(n), Unit::None, _) => n * Number::from(100),
        Value::Dimension(None, ..) => todo!(),
        v @ Value::Dimension(..) => {
            return Err((
                format!(
                    "$number: Expected {} to have no units.",
                    v.inspect(args.span())?
                ),
                args.span(),
            )
                .into())
        }
        v => {
            return Err((
                format!("$number: {} is not a number.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::Dimension(Some(num), Unit::Percent, true))
}

pub(crate) fn round(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    match args.get_err(0, "number")? {
        Value::Dimension(Some(n), u, _) => Ok(Value::Dimension(Some(n.round()), u, true)),
        Value::Dimension(None, ..) => todo!(),
        v => Err((
            format!("$number: {} is not a number.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn ceil(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    match args.get_err(0, "number")? {
        Value::Dimension(Some(n), u, _) => Ok(Value::Dimension(Some(n.ceil()), u, true)),
        Value::Dimension(None, ..) => todo!(),
        v => Err((
            format!("$number: {} is not a number.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn floor(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    match args.get_err(0, "number")? {
        Value::Dimension(Some(n), u, _) => Ok(Value::Dimension(Some(n.floor()), u, true)),
        Value::Dimension(None, ..) => todo!(),
        v => Err((
            format!("$number: {} is not a number.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn abs(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    match args.get_err(0, "number")? {
        Value::Dimension(Some(n), u, _) => Ok(Value::Dimension(Some(n.abs()), u, true)),
        Value::Dimension(None, ..) => todo!(),
        v => Err((
            format!("$number: {} is not a number.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn comparable(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(2)?;
    let unit1 = match args.get_err(0, "number1")? {
        Value::Dimension(_, u, _) => u,
        v => {
            return Err((
                format!("$number1: {} is not a number.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    let unit2 = match args.get_err(1, "number2")? {
        Value::Dimension(_, u, _) => u,
        v => {
            return Err((
                format!("$number2: {} is not a number.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    Ok(Value::bool(unit1.comparable(&unit2)))
}

// TODO: write tests for this
#[cfg(feature = "random")]
pub(crate) fn random(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    let limit = match args.default_arg(0, "limit", Value::Null)? {
        Value::Dimension(Some(n), ..) => n,
        Value::Dimension(None, ..) => todo!(),
        Value::Null => {
            let mut rng = rand::thread_rng();
            return Ok(Value::Dimension(
                Some(Number::from(rng.gen_range(0.0, 1.0))),
                Unit::None,
                true,
            ));
        }
        v => {
            return Err((
                format!("$limit: {} is not a number.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    if limit.is_one() {
        return Ok(Value::Dimension(Some(Number::one()), Unit::None, true));
    }

    if limit.is_decimal() {
        return Err((format!("$limit: {} is not an int.", limit), args.span()).into());
    }

    if limit.is_zero() || limit.is_negative() {
        return Err((
            format!("$limit: Must be greater than 0, was {}.", limit),
            args.span(),
        )
            .into());
    }

    let limit = match limit.to_integer().to_u32() {
        Some(n) => n,
        None => {
            return Err((
                format!("max must be in range 0 < max \u{2264} 2^32, was {}", limit),
                args.span(),
            )
                .into())
        }
    };

    let mut rng = rand::thread_rng();
    Ok(Value::Dimension(
        Some(Number::from(rng.gen_range(0, limit) + 1)),
        Unit::None,
        true,
    ))
}

pub(crate) fn min(args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.min_args(1)?;
    let span = args.span();
    let mut nums = args
        .get_variadic()?
        .into_iter()
        .map(|val| match val.node {
            Value::Dimension(Some(number), unit, _) => Ok((number, unit)),
            Value::Dimension(None, ..) => todo!(),
            v => Err((format!("{} is not a number.", v.inspect(span)?), span).into()),
        })
        .collect::<SassResult<Vec<(Number, Unit)>>>()?
        .into_iter();

    // we know that there *must* be at least one item
    let mut min = nums.next().unwrap();

    for num in nums {
        if ValueVisitor::new(parser, span)
            .less_than(
                HigherIntermediateValue::Literal(Value::Dimension(
                    Some(num.0.clone()),
                    num.1.clone(),
                    true,
                )),
                HigherIntermediateValue::Literal(Value::Dimension(
                    Some(min.0.clone()),
                    min.1.clone(),
                    true,
                )),
            )?
            .is_true()
        {
            min = num;
        }
    }
    Ok(Value::Dimension(Some(min.0), min.1, true))
}

pub(crate) fn max(args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.min_args(1)?;
    let span = args.span();
    let mut nums = args
        .get_variadic()?
        .into_iter()
        .map(|val| match val.node {
            Value::Dimension(Some(number), unit, _) => Ok((number, unit)),
            Value::Dimension(None, ..) => todo!(),
            v => Err((format!("{} is not a number.", v.inspect(span)?), span).into()),
        })
        .collect::<SassResult<Vec<(Number, Unit)>>>()?
        .into_iter();

    // we know that there *must* be at least one item
    let mut max = nums.next().unwrap();

    for num in nums {
        if ValueVisitor::new(parser, span)
            .greater_than(
                HigherIntermediateValue::Literal(Value::Dimension(
                    Some(num.0.clone()),
                    num.1.clone(),
                    true,
                )),
                HigherIntermediateValue::Literal(Value::Dimension(
                    Some(max.0.clone()),
                    max.1.clone(),
                    true,
                )),
            )?
            .is_true()
        {
            max = num;
        }
    }
    Ok(Value::Dimension(Some(max.0), max.1, true))
}

pub(crate) fn declare(f: &mut GlobalFunctionMap) {
    f.insert("percentage", Builtin::new(percentage));
    f.insert("round", Builtin::new(round));
    f.insert("ceil", Builtin::new(ceil));
    f.insert("floor", Builtin::new(floor));
    f.insert("abs", Builtin::new(abs));
    f.insert("min", Builtin::new(min));
    f.insert("max", Builtin::new(max));
    f.insert("comparable", Builtin::new(comparable));
    #[cfg(feature = "random")]
    f.insert("random", Builtin::new(random));
}
