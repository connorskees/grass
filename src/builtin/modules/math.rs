use crate::builtin::builtin_imports::*;

use crate::builtin::{
    math::{abs, ceil, comparable, divide, floor, max, min, percentage, round},
    meta::{unit, unitless},
    modules::Module,
};

#[cfg(feature = "random")]
use crate::builtin::math::random;

fn clamp(mut args: ArgumentResult, _: &mut Visitor) -> SassResult<Value> {
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
    min.cmp(&max, span, BinaryOp::LessThan)?;

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

    match min.cmp(&number, span, BinaryOp::LessThan)? {
        Ordering::Greater => return Ok(min),
        Ordering::Equal => return Ok(number),
        Ordering::Less => {}
    }

    match max.cmp(&number, span, BinaryOp::GreaterThan)? {
        Ordering::Less => return Ok(max),
        Ordering::Equal => return Ok(number),
        Ordering::Greater => {}
    }

    Ok(number)
}

fn hypot(args: ArgumentResult, _: &mut Visitor) -> SassResult<Value> {
    args.min_args(1)?;

    let span = args.span();

    let mut numbers = args.get_variadic()?.into_iter().map(|v| -> SassResult<_> {
        match v.node {
            Value::Dimension(n, u, ..) => Ok((n, u)),
            v => Err((format!("{} is not a number.", v.inspect(span)?), span).into()),
        }
    });

    let first: (Number, Unit) = match numbers.next().unwrap()? {
        ((n), u) => (n * n, u),
    };

    let rest = numbers
        .enumerate()
        .map(|(idx, val)| -> SassResult<Number> {
            let (number, unit) = val?;
            if first.1 == Unit::None {
                if unit == Unit::None {
                    Ok(number * number)
                } else {
                    Err((
                        format!(
                            "Argument 1 is unitless but argument {} has unit {}. \
                            Arguments must all have units or all be unitless.",
                            idx + 2,
                            unit
                        ),
                        span,
                    )
                        .into())
                }
            } else if unit == Unit::None {
                Err((
                    format!(
                        "Argument 1 has unit {} but argument {} is unitless. \
                        Arguments must all have units or all be unitless.",
                        first.1,
                        idx + 2,
                    ),
                    span,
                )
                    .into())
            } else if first.1.comparable(&unit) {
                let n = number.convert(&unit, &first.1);
                Ok(n * n)
            } else {
                Err((
                    format!("Incompatible units {} and {}.", first.1, unit),
                    span,
                )
                    .into())
            }
        })
        .collect::<SassResult<Vec<Number>>>()?;

    let sum = first.0 + rest.into_iter().fold(Number::zero(), |a, b| a + b);

    Ok(Value::Dimension(sum.sqrt(), first.1, None))
}

fn log(mut args: ArgumentResult, _: &mut Visitor) -> SassResult<Value> {
    args.max_args(2)?;

    let number = match args.get_err(0, "number")? {
        Value::Dimension(n, ..) if n.is_nan() => todo!(),
        Value::Dimension((n), Unit::None, ..) => n,
        v @ Value::Dimension(..) => {
            return Err((
                format!(
                    "$number: Expected {} to be unitless.",
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

    let base = match args.default_arg(1, "base", Value::Null) {
        Value::Null => None,
        Value::Dimension(n, ..) if n.is_nan() => todo!(),
        Value::Dimension((n), Unit::None, ..) => Some(n),
        v @ Value::Dimension(..) => {
            return Err((
                format!(
                    "$number: Expected {} to be unitless.",
                    v.inspect(args.span())?
                ),
                args.span(),
            )
                .into())
        }
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
                (Number::zero())
            } else {
                number.log(base)
            }
        } else if number.is_negative() {
            // todo: NaN
            todo!()
            // None
        } else if number.is_zero() {
            todo!()
        } else {
            number.ln()
        },
        Unit::None,
        None,
    ))
}

fn pow(mut args: ArgumentResult, _: &mut Visitor) -> SassResult<Value> {
    args.max_args(2)?;

    let base = match args.get_err(0, "base")? {
        Value::Dimension(n, ..) if n.is_nan() => todo!(),
        Value::Dimension((n), Unit::None, ..) => n,
        v @ Value::Dimension(..) => {
            return Err((
                format!(
                    "$base: Expected {} to have no units.",
                    v.inspect(args.span())?
                ),
                args.span(),
            )
                .into())
        }
        v => {
            return Err((
                format!("$base: {} is not a number.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    let exponent = match args.get_err(1, "exponent")? {
        Value::Dimension(n, ..) if n.is_nan() => todo!(),
        Value::Dimension((n), Unit::None, ..) => n,
        v @ Value::Dimension(..) => {
            return Err((
                format!(
                    "$exponent: Expected {} to have no units.",
                    v.inspect(args.span())?
                ),
                args.span(),
            )
                .into())
        }
        v => {
            return Err((
                format!("$exponent: {} is not a number.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    Ok(Value::Dimension(base.pow(exponent), Unit::None, None))
}

fn sqrt(mut args: ArgumentResult, _: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    let number = args.get_err(0, "number")?;

    Ok(match number {
        Value::Dimension(n, ..) if n.is_nan() => todo!(),
        Value::Dimension((n), Unit::None, ..) => Value::Dimension(n.sqrt(), Unit::None, None),
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
    })
}

macro_rules! trig_fn {
    ($name:ident, $name_deg:ident) => {
        fn $name(mut args: ArgumentResult, _: &mut Visitor) -> SassResult<Value> {
            args.max_args(1)?;
            let number = args.get_err(0, "number")?;

            Ok(match number {
                Value::Dimension(n, ..) if n.is_nan() => todo!(),
                Value::Dimension((n), Unit::None, ..) | Value::Dimension((n), Unit::Rad, ..) => {
                    Value::Dimension(n.$name(), Unit::None, None)
                }
                Value::Dimension((n), Unit::Deg, ..) => {
                    Value::Dimension(n.$name_deg(), Unit::None, None)
                }
                v @ Value::Dimension(..) => {
                    return Err((
                        format!(
                            "$number: Expected {} to be an angle.",
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
            })
        }
    };
}

trig_fn!(cos, cos_deg);
trig_fn!(sin, sin_deg);
trig_fn!(tan, tan_deg);

fn acos(mut args: ArgumentResult, _: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    let number = args.get_err(0, "number")?;

    Ok(match number {
        Value::Dimension(n, ..) if n.is_nan() => todo!(),
        Value::Dimension((n), Unit::None, ..) => Value::Dimension(
            if n > Number::from(1) || n < Number::from(-1) {
                // todo: NaN
                // None
                todo!()
            } else if n.is_one() {
                (Number::zero())
            } else {
                n.acos()
            },
            Unit::Deg,
            None,
        ),
        v @ Value::Dimension(..) => {
            return Err((
                format!(
                    "$number: Expected {} to be unitless.",
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
    })
}

fn asin(mut args: ArgumentResult, _: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    let number = args.get_err(0, "number")?;

    Ok(match number {
        Value::Dimension((n), Unit::None, ..) => {
            if n > Number::from(1) || n < Number::from(-1) {
                // todo: NaN
                // return Ok(Value::Dimension(None, Unit::Deg, None));
            } else if n.is_zero() {
                return Ok(Value::Dimension((Number::zero()), Unit::Deg, None));
            }

            Value::Dimension(n.asin(), Unit::Deg, None)
        }
        v @ Value::Dimension(..) => {
            return Err((
                format!(
                    "$number: Expected {} to be unitless.",
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
    })
}

fn atan(mut args: ArgumentResult, _: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    let number = args.get_err(0, "number")?;

    Ok(match number {
        Value::Dimension(n, Unit::None, ..) => {
            if n.is_zero() {
                return Ok(Value::Dimension((Number::zero()), Unit::Deg, None));
            }

            Value::Dimension(n.atan(), Unit::Deg, None)
        }
        v @ Value::Dimension(..) => {
            return Err((
                format!(
                    "$number: Expected {} to be unitless.",
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
    })
}

fn atan2(mut args: ArgumentResult, _: &mut Visitor) -> SassResult<Value> {
    args.max_args(2)?;
    let (y_num, y_unit) = match args.get_err(0, "y")? {
        Value::Dimension(n, u, ..) => (n, u),
        v => {
            return Err((
                format!("$y: {} is not a number.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    let (x_num, x_unit) = match args.get_err(1, "x")? {
        Value::Dimension(n, u, ..) => (n, u),
        v => {
            return Err((
                format!("$x: {} is not a number.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    let (x_num, y_num) = if x_unit == Unit::None && y_unit == Unit::None {
        (x_num, y_num)
    } else if y_unit == Unit::None {
        return Err((
            format!(
                "$y is unitless but $x has unit {}. \
            Arguments must all have units or all be unitless.",
                x_unit
            ),
            args.span(),
        )
            .into());
    } else if x_unit == Unit::None {
        return Err((
            format!(
                "$y has unit {} but $x is unitless. \
                Arguments must all have units or all be unitless.",
                y_unit
            ),
            args.span(),
        )
            .into());
    } else if x_unit.comparable(&y_unit) {
        (x_num, y_num.convert(&y_unit, &x_unit))
    } else {
        return Err((
            format!("Incompatible units {} and {}.", y_unit, x_unit),
            args.span(),
        )
            .into());
    };

    Ok(
        match (
            NumberState::from_number(&x_num),
            NumberState::from_number(&y_num),
        ) {
            (NumberState::Zero, NumberState::FiniteNegative) => {
                Value::Dimension((Number::from(-90)), Unit::Deg, None)
            }
            (NumberState::Zero, NumberState::Zero) | (NumberState::Finite, NumberState::Zero) => {
                Value::Dimension((Number::zero()), Unit::Deg, None)
            }
            (NumberState::Zero, NumberState::Finite) => {
                Value::Dimension((Number::from(90)), Unit::Deg, None)
            }
            (NumberState::Finite, NumberState::Finite)
            | (NumberState::FiniteNegative, NumberState::Finite)
            | (NumberState::Finite, NumberState::FiniteNegative)
            | (NumberState::FiniteNegative, NumberState::FiniteNegative) => Value::Dimension(
                (y_num.atan2(x_num) * Number::from(180)) / Number::pi(),
                Unit::Deg,
                None,
            ),
            (NumberState::FiniteNegative, NumberState::Zero) => {
                Value::Dimension((Number::from(180)), Unit::Deg, None)
            }
        },
    )
}

enum NumberState {
    Zero,
    Finite,
    FiniteNegative,
}

impl NumberState {
    fn from_number(num: &Number) -> Self {
        match (num.is_zero(), num.is_negative()) {
            (true, _) => NumberState::Zero,
            (false, true) => NumberState::Finite,
            (false, false) => NumberState::FiniteNegative,
        }
    }
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
    f.insert_builtin("pow", pow);
    f.insert_builtin("hypot", hypot);
    f.insert_builtin("div", divide);
    f.insert_builtin("atan2", atan2);
    #[cfg(feature = "random")]
    f.insert_builtin("random", random);

    f.insert_builtin_var(
        "e",
        Value::Dimension(Number::from(std::f64::consts::E), Unit::None, None),
    );
    f.insert_builtin_var(
        "pi",
        Value::Dimension(Number::from(std::f64::consts::PI), Unit::None, None),
    );
}
