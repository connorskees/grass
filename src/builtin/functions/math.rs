use crate::{builtin::builtin_imports::*, evaluate::div};

pub(crate) fn percentage(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    let num = match args.get_err(0, "number")? {
        Value::Dimension {
            num: n,
            unit: Unit::None,
            as_slash: _,
        } => n * Number::from(100),
        v @ Value::Dimension { .. } => {
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
    Ok(Value::Dimension {
        num,
        unit: Unit::Percent,
        as_slash: None,
    })
}

pub(crate) fn round(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    match args.get_err(0, "number")? {
        // todo: better error message, consider finities
        Value::Dimension { num: n, .. } if n.is_nan() => {
            Err(("Infinity or NaN toInt", args.span()).into())
        }
        Value::Dimension {
            num: n,
            unit: u,
            as_slash: _,
        } => Ok(Value::Dimension {
            num: (n.round()),
            unit: u,
            as_slash: None,
        }),
        v => Err((
            format!("$number: {} is not a number.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn ceil(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    match args.get_err(0, "number")? {
        // todo: better error message, consider finities
        Value::Dimension { num: n, .. } if n.is_nan() => {
            Err(("Infinity or NaN toInt", args.span()).into())
        }
        Value::Dimension {
            num: n,
            unit: u,
            as_slash: _,
        } => Ok(Value::Dimension {
            num: (n.ceil()),
            unit: u,
            as_slash: None,
        }),
        v => Err((
            format!("$number: {} is not a number.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn floor(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    match args.get_err(0, "number")? {
        // todo: better error message, consider finities
        Value::Dimension { num: n, .. } if n.is_nan() => {
            Err(("Infinity or NaN toInt", args.span()).into())
        }
        Value::Dimension {
            num: n,
            unit: u,
            as_slash: _,
        } => Ok(Value::Dimension {
            num: (n.floor()),
            unit: u,
            as_slash: None,
        }),
        v => Err((
            format!("$number: {} is not a number.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn abs(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    match args.get_err(0, "number")? {
        Value::Dimension {
            num: n,
            unit: u,
            as_slash: _,
        } => Ok(Value::Dimension {
            num: (n.abs()),
            unit: u,
            as_slash: None,
        }),
        v => Err((
            format!("$number: {} is not a number.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn comparable(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(2)?;
    let unit1 = match args.get_err(0, "number1")? {
        Value::Dimension {
            num: _,
            unit: u,
            as_slash: _,
        } => u,
        v => {
            return Err((
                format!("$number1: {} is not a number.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    let unit2 = match args.get_err(1, "number2")? {
        Value::Dimension {
            num: _,
            unit: u,
            as_slash: _,
        } => u,
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
pub(crate) fn random(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    let limit = match args.default_arg(0, "limit", Value::Null) {
        Value::Dimension {
            num: n, unit: u, ..
        } if n.is_nan() => {
            // todo: likely same for finities
            // todo: can remove match altogether thanks to assert_int
            return Err((format!("$limit: NaN{} is not an int.", u), args.span()).into());
        }
        Value::Dimension { num: n, .. } => n,
        Value::Null => {
            let mut rng = rand::thread_rng();
            return Ok(Value::Dimension {
                num: (Number::from(rng.gen_range(0.0..1.0))),
                unit: Unit::None,
                as_slash: None,
            });
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
        return Ok(Value::Dimension {
            num: (Number::one()),
            unit: Unit::None,
            as_slash: None,
        });
    }

    if limit.is_decimal() {
        return Err((
            format!("$limit: {} is not an int.", limit.inspect()),
            args.span(),
        )
            .into());
    }

    if limit.is_zero() || limit.is_negative() {
        return Err((
            format!("$limit: Must be greater than 0, was {}.", limit.inspect()),
            args.span(),
        )
            .into());
    }

    let limit = match limit.to_integer().to_u32() {
        Some(n) => n,
        None => {
            return Err((
                format!(
                    "max must be in range 0 < max \u{2264} 2^32, was {}",
                    limit.inspect()
                ),
                args.span(),
            )
                .into())
        }
    };

    let mut rng = rand::thread_rng();
    Ok(Value::Dimension {
        num: (Number::from(rng.gen_range(0..limit) + 1)),
        unit: Unit::None,
        as_slash: None,
    })
}

pub(crate) fn min(args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.min_args(1)?;
    let span = args.span();
    let mut nums = args
        .get_variadic()?
        .into_iter()
        .map(|val| match val.node {
            Value::Dimension {
                num: number,
                unit,
                as_slash: _,
            } => Ok((number, unit)),
            v => Err((format!("{} is not a number.", v.inspect(span)?), span).into()),
        })
        .collect::<SassResult<Vec<(Number, Unit)>>>()?
        .into_iter();

    let mut min = match nums.next() {
        Some((n, u)) => (n, u),
        None => unreachable!(),
    };

    for (num, unit) in nums {
        let lhs = Value::Dimension {
            num,
            unit: unit.clone(),
            as_slash: None,
        };
        let rhs = Value::Dimension {
            num: (min.0),
            unit: min.1.clone(),
            as_slash: None,
        };

        if crate::evaluate::cmp(
            &lhs,
            &rhs,
            parser.parser.options,
            parser.parser.span_before,
            BinaryOp::LessThan,
        )?
        .is_true()
        {
            min = (num, unit);
        }
    }
    Ok(Value::Dimension {
        num: (min.0),
        unit: min.1,
        as_slash: None,
    })
}

pub(crate) fn max(args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.min_args(1)?;
    let span = args.span();
    let mut nums = args
        .get_variadic()?
        .into_iter()
        .map(|val| match val.node {
            Value::Dimension {
                num: number,
                unit,
                as_slash: _,
            } => Ok((number, unit)),
            v => Err((format!("{} is not a number.", v.inspect(span)?), span).into()),
        })
        .collect::<SassResult<Vec<(Number, Unit)>>>()?
        .into_iter();

    let mut max = match nums.next() {
        Some((n, u)) => (n, u),
        None => unreachable!(),
    };

    for (num, unit) in nums {
        let lhs = Value::Dimension {
            num,
            unit: unit.clone(),
            as_slash: None,
        };
        let rhs = Value::Dimension {
            num: (max.0),
            unit: max.1.clone(),
            as_slash: None,
        };

        if crate::evaluate::cmp(
            &lhs,
            &rhs,
            parser.parser.options,
            parser.parser.span_before,
            BinaryOp::GreaterThan,
        )?
        .is_true()
        {
            max = (num, unit);
        }
    }
    Ok(Value::Dimension {
        num: (max.0),
        unit: max.1,
        as_slash: None,
    })
}

pub(crate) fn divide(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(2)?;

    let number1 = args.get_err(0, "number1")?;
    let number2 = args.get_err(1, "number2")?;

    div(number1, number2, parser.parser.options, args.span())
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
