use super::{Builtin, GlobalFunctionMap};

#[cfg(feature = "random")]
use num_traits::{One, Signed, ToPrimitive, Zero};
#[cfg(feature = "random")]
use rand::Rng;

use crate::{
    args::CallArgs,
    error::SassResult,
    parse::Parser,
    unit::Unit,
    value::{Number, Value},
};

fn percentage(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    let num = match parser.arg(&mut args, 0, "number")? {
        Value::Dimension(n, Unit::None) => n * Number::from(100),
        v @ Value::Dimension(..) => {
            return Err((
                format!(
                    "$number: Expected {} to have no units.",
                    v.to_css_string(args.span())?
                ),
                args.span(),
            )
                .into())
        }
        v => {
            return Err((
                format!(
                    "$number: {} is not a number.",
                    v.to_css_string(args.span())?
                ),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::Dimension(num, Unit::Percent))
}

fn round(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    match parser.arg(&mut args, 0, "number")? {
        Value::Dimension(n, u) => Ok(Value::Dimension(n.round(), u)),
        v => Err((
            format!(
                "$number: {} is not a number.",
                v.to_css_string(args.span())?
            ),
            args.span(),
        )
            .into()),
    }
}

fn ceil(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    match parser.arg(&mut args, 0, "number")? {
        Value::Dimension(n, u) => Ok(Value::Dimension(n.ceil(), u)),
        v => Err((
            format!(
                "$number: {} is not a number.",
                v.to_css_string(args.span())?
            ),
            args.span(),
        )
            .into()),
    }
}

fn floor(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    match parser.arg(&mut args, 0, "number")? {
        Value::Dimension(n, u) => Ok(Value::Dimension(n.floor(), u)),
        v => Err((
            format!(
                "$number: {} is not a number.",
                v.to_css_string(args.span())?
            ),
            args.span(),
        )
            .into()),
    }
}

fn abs(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    match parser.arg(&mut args, 0, "number")? {
        Value::Dimension(n, u) => Ok(Value::Dimension(n.abs(), u)),
        v => Err((
            format!(
                "$number: {} is not a number.",
                v.to_css_string(args.span())?
            ),
            args.span(),
        )
            .into()),
    }
}

fn comparable(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(2)?;
    let unit1 = match parser.arg(&mut args, 0, "number1")? {
        Value::Dimension(_, u) => u,
        v => {
            return Err((
                format!(
                    "$number1: {} is not a number.",
                    v.to_css_string(args.span())?
                ),
                args.span(),
            )
                .into())
        }
    };
    let unit2 = match parser.arg(&mut args, 1, "number2")? {
        Value::Dimension(_, u) => u,
        v => {
            return Err((
                format!(
                    "$number2: {} is not a number.",
                    v.to_css_string(args.span())?
                ),
                args.span(),
            )
                .into())
        }
    };

    Ok(Value::bool(unit1.comparable(&unit2)))
}

// TODO: write tests for this
#[cfg(feature = "random")]
fn random(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    let limit = match parser.default_arg(&mut args, 0, "limit", Value::Null)? {
        Value::Dimension(n, _) => n,
        Value::Null => {
            let mut rng = rand::thread_rng();
            return Ok(Value::Dimension(
                Number::from(rng.gen_range(0.0, 1.0)),
                Unit::None,
            ));
        }
        v => {
            return Err((
                format!("$limit: {} is not a number.", v.to_css_string(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    if limit.is_one() {
        return Ok(Value::Dimension(Number::one(), Unit::None));
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
        Number::from(rng.gen_range(0, limit) + 1),
        Unit::None,
    ))
}

pub(crate) fn declare(f: &mut GlobalFunctionMap) {
    f.insert("percentage", Builtin::new(percentage));
    f.insert("round", Builtin::new(round));
    f.insert("ceil", Builtin::new(ceil));
    f.insert("floor", Builtin::new(floor));
    f.insert("abs", Builtin::new(abs));
    f.insert("comparable", Builtin::new(comparable));
    #[cfg(feature = "random")]
    f.insert("random", Builtin::new(random));
}
