use core::fmt;
use std::iter::Iterator;

use codemap::Span;

use crate::{
    common::BinaryOp,
    error::SassResult,
    serializer::inspect_number,
    unit::Unit,
    value::{Number, SassNumber, Value},
    Options,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum CalculationArg {
    Number(SassNumber),
    Calculation(SassCalculation),
    String(String),
    Operation {
        lhs: Box<Self>,
        op: BinaryOp,
        rhs: Box<Self>,
    },
    Interpolation(String),
}

impl CalculationArg {
    pub fn inspect(&self, span: Span) -> SassResult<String> {
        Ok(match self {
            CalculationArg::Number(SassNumber {
                num,
                unit,
                as_slash,
            }) => Value::Dimension {
                num: Number(*num),
                unit: unit.clone(),
                as_slash: as_slash.clone(),
            }
            .inspect(span)?
            .into_owned(),
            CalculationArg::Calculation(calc) => {
                Value::Calculation(calc.clone()).inspect(span)?.into_owned()
            }
            CalculationArg::String(s) | CalculationArg::Interpolation(s) => s.clone(),
            CalculationArg::Operation { lhs, op, rhs } => {
                format!("{} {op} {}", lhs.inspect(span)?, rhs.inspect(span)?)
            }
        })
    }
}

impl CalculationArg {
    fn parenthesize_calculation_rhs(outer: BinaryOp, right: BinaryOp) -> bool {
        if outer == BinaryOp::Div {
            true
        } else if outer == BinaryOp::Plus {
            false
        } else {
            right == BinaryOp::Plus || right == BinaryOp::Minus
        }
    }

    fn write_calculation_value(
        buf: &mut String,
        val: &CalculationArg,
        is_compressed: bool,
        span: Span,
    ) -> SassResult<()> {
        match val {
            CalculationArg::Number(n) => {
                // todo: superfluous clone
                let n = n.clone();
                buf.push_str(
                    &Value::Dimension {
                        num: Number(n.num),
                        unit: n.unit,
                        as_slash: n.as_slash,
                    }
                    .to_css_string(span, is_compressed)?,
                );
            }
            CalculationArg::Calculation(calc) => {
                buf.push_str(&Value::Calculation(calc.clone()).to_css_string(span, is_compressed)?);
            }
            CalculationArg::Operation { lhs, op, rhs } => {
                let paren_left = match &**lhs {
                    CalculationArg::Interpolation(..) => true,
                    CalculationArg::Operation { op: lhs_op, .. }
                        if lhs_op.precedence() < op.precedence() =>
                    {
                        true
                    }
                    _ => false,
                };

                if paren_left {
                    buf.push('(');
                }

                Self::write_calculation_value(buf, lhs, is_compressed, span)?;

                if paren_left {
                    buf.push(')');
                }

                let op_whitespace = !is_compressed || op.precedence() == 2;

                if op_whitespace {
                    buf.push(' ');
                }

                buf.push_str(&op.to_string());

                if op_whitespace {
                    buf.push(' ');
                }

                let paren_right = match &**lhs {
                    CalculationArg::Interpolation(..) => true,
                    CalculationArg::Operation { op: rhs_op, .. }
                        if Self::parenthesize_calculation_rhs(*op, *rhs_op) =>
                    {
                        true
                    }
                    _ => false,
                };

                if paren_right {
                    buf.push('(');
                }

                Self::write_calculation_value(buf, rhs, is_compressed, span)?;

                if paren_right {
                    buf.push(')');
                }
            }
            CalculationArg::String(i) | CalculationArg::Interpolation(i) => buf.push_str(i),
        }

        Ok(())
    }

    pub fn to_css_string(&self, span: Span, is_compressed: bool) -> SassResult<String> {
        let mut buf = String::new();
        Self::write_calculation_value(&mut buf, self, is_compressed, span)?;
        Ok(buf)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum CalculationName {
    Calc,
    Min,
    Max,
    Clamp,
}

impl fmt::Display for CalculationName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CalculationName::Calc => f.write_str("calc"),
            CalculationName::Min => f.write_str("min"),
            CalculationName::Max => f.write_str("max"),
            CalculationName::Clamp => f.write_str("clamp"),
        }
    }
}

impl CalculationName {
    pub fn in_min_or_max(self) -> bool {
        self == CalculationName::Min || self == CalculationName::Max
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct SassCalculation {
    pub name: CalculationName,
    pub args: Vec<CalculationArg>,
}

impl SassCalculation {
    pub fn unsimplified(name: CalculationName, args: Vec<CalculationArg>) -> Self {
        Self { name, args }
    }

    pub fn calc(arg: CalculationArg) -> SassResult<Value> {
        let arg = Self::simplify(arg);
        match arg {
            CalculationArg::Number(n) => Ok(Value::Dimension {
                num: Number(n.num),
                unit: n.unit,
                as_slash: n.as_slash,
            }),
            CalculationArg::Calculation(c) => Ok(Value::Calculation(c)),
            _ => Ok(Value::Calculation(SassCalculation {
                name: CalculationName::Calc,
                args: vec![arg],
            })),
        }
    }

    pub fn min(args: Vec<CalculationArg>) -> SassResult<Value> {
        let args = Self::simplify_arguments(args);
        if args.is_empty() {
            todo!("min() must have at least one argument.")
        }

        let mut minimum: Option<SassNumber> = None;

        for arg in &args {
            match arg {
                CalculationArg::Number(n)
                    if minimum.is_some() && !minimum.as_ref().unwrap().is_comparable_to(n) =>
                {
                    minimum = None;
                    break;
                }
                // todo: units
                CalculationArg::Number(n)
                    if minimum.is_none() || minimum.as_ref().unwrap().num > n.num =>
                {
                    minimum = Some(n.clone());
                }
                CalculationArg::Number(..) => continue,
                _ => {
                    minimum = None;
                    break;
                }
            }
        }

        Ok(match minimum {
            Some(min) => Value::Dimension {
                num: Number(min.num),
                unit: min.unit,
                as_slash: min.as_slash,
            },
            None => {
                // _verifyCompatibleNumbers(args);
                Value::Calculation(SassCalculation {
                    name: CalculationName::Min,
                    args,
                })
            }
        })
    }

    pub fn max(args: Vec<CalculationArg>) -> SassResult<Value> {
        let args = Self::simplify_arguments(args);
        if args.is_empty() {
            todo!("max() must have at least one argument.")
        }

        let mut maximum: Option<SassNumber> = None;

        for arg in &args {
            match arg {
                CalculationArg::Number(n)
                    if maximum.is_some() && !maximum.as_ref().unwrap().is_comparable_to(n) =>
                {
                    maximum = None;
                    break;
                }
                // todo: units
                CalculationArg::Number(n)
                    if maximum.is_none() || maximum.as_ref().unwrap().num < n.num =>
                {
                    maximum = Some(n.clone());
                }
                CalculationArg::Number(..) => continue,
                _ => {
                    maximum = None;
                    break;
                }
            }
        }

        Ok(match maximum {
            Some(max) => Value::Dimension {
                num: Number(max.num),
                unit: max.unit,
                as_slash: max.as_slash,
            },
            None => {
                // todo: _verifyCompatibleNumbers(args);
                Value::Calculation(SassCalculation {
                    name: CalculationName::Max,
                    args,
                })
            }
        })
    }

    pub fn clamp(
        min: CalculationArg,
        value: Option<CalculationArg>,
        max: Option<CalculationArg>,
        options: &Options,
        span: Span,
    ) -> SassResult<Value> {
        if value.is_none() && max.is_some() {
            return Err(("If value is null, max must also be null.", span).into());
        }

        let min = Self::simplify(min);
        let value = value.map(Self::simplify);
        let max = max.map(Self::simplify);

        match (min.clone(), value.clone(), max.clone()) {
            (
                CalculationArg::Number(min),
                Some(CalculationArg::Number(value)),
                Some(CalculationArg::Number(max)),
            ) => {
                if min.is_comparable_to(&value) && min.is_comparable_to(&max) {
                    // todo: account for units?
                    if value.num <= min.num {
                        return Ok(Value::Dimension {
                            num: Number(min.num),
                            unit: min.unit,
                            as_slash: min.as_slash,
                        });
                    }

                    // todo: account for units?
                    if value.num >= max.num {
                        return Ok(Value::Dimension {
                            num: Number(max.num),
                            unit: max.unit,
                            as_slash: max.as_slash,
                        });
                    }

                    return Ok(Value::Dimension {
                        num: Number(value.num),
                        unit: value.unit,
                        as_slash: value.as_slash,
                    });
                }
            }
            _ => {}
        }

        let mut args = vec![min];

        if let Some(value) = value {
            args.push(value);
        }

        if let Some(max) = max {
            args.push(max);
        }

        Self::verify_length(&args, 3, span)?;
        Self::verify_compatible_numbers(&args, options, span)?;

        Ok(Value::Calculation(SassCalculation {
            name: CalculationName::Clamp,
            args,
        }))
    }

    fn verify_length(args: &[CalculationArg], len: usize, span: Span) -> SassResult<()> {
        if args.len() == len {
            return Ok(());
        }

        if args.iter().any(|arg| {
            matches!(
                arg,
                CalculationArg::String(..) | CalculationArg::Interpolation(..)
            )
        }) {
            return Ok(());
        }

        let was_or_were = if args.len() == 1 { "was" } else { "were" };

        Err((
            format!(
                "{len} arguments required, but only {} {was_or_were} passed.",
                args.len()
            ),
            span,
        )
            .into())
    }

    fn verify_compatible_numbers(
        args: &[CalculationArg],
        options: &Options,
        span: Span,
    ) -> SassResult<()> {
        for arg in args {
            match arg {
                CalculationArg::Number(num) => match &num.unit {
                    Unit::Complex { numer, denom } => {
                        if numer.len() > 1 || !denom.is_empty() {
                            let num = num.clone();
                            let value = Value::Dimension {
                                num: Number(num.num),
                                unit: num.unit,
                                as_slash: num.as_slash,
                            };
                            return Err((
                                format!(
                                    "Number {} isn't compatible with CSS calculations.",
                                    value.to_css_string(span, false)?
                                ),
                                span,
                            )
                                .into());
                        }
                    }
                    _ => continue,
                },
                _ => continue,
            }
        }

        for i in 0..args.len() {
            let number1 = match &args[i] {
                CalculationArg::Number(num) => num,
                _ => continue,
            };

            for j in (i + 1)..args.len() {
                let number2 = match &args[j] {
                    CalculationArg::Number(num) => num,
                    _ => continue,
                };

                if number1.has_possibly_compatible_units(number2) {
                    continue;
                }

                return Err((
                    format!(
                        "{} and {} are incompatible.",
                        inspect_number(number1, options, span)?,
                        inspect_number(number2, options, span)?
                    ),
                    span,
                )
                    .into());
            }
        }

        Ok(())
    }

    pub fn operate_internal(
        mut op: BinaryOp,
        left: CalculationArg,
        right: CalculationArg,
        in_min_or_max: bool,
        simplify: bool,
        options: &Options,
        span: Span,
    ) -> SassResult<CalculationArg> {
        if !simplify {
            return Ok(CalculationArg::Operation {
                lhs: Box::new(left),
                op,
                rhs: Box::new(right),
            });
        }

        let left = Self::simplify(left);
        let mut right = Self::simplify(right);

        if op == BinaryOp::Plus || op == BinaryOp::Minus {
            let is_comparable = if in_min_or_max {
                // todo:
                // left.isComparableTo(right)
                true
            } else {
                // left.hasCompatibleUnits(right)
                true
            };
            match (&left, &right) {
                (CalculationArg::Number(num1), CalculationArg::Number(num2))
                    if num1.is_comparable_to(num2) =>
                {
                    if op == BinaryOp::Plus {
                        return Ok(CalculationArg::Number(num1.clone() + num2.clone()));
                    } else {
                        return Ok(CalculationArg::Number(num1.clone() - num2.clone()));
                    }
                }
                _ => {}
            }

            Self::verify_compatible_numbers(&[left.clone(), right.clone()], options, span)?;

            if let CalculationArg::Number(mut n) = right {
                if Number(n.num).is_negative() {
                    n.num *= -1.0;
                    op = if op == BinaryOp::Plus {
                        BinaryOp::Minus
                    } else {
                        BinaryOp::Plus
                    }
                } else {
                }
                right = CalculationArg::Number(n);
            }

            return Ok(CalculationArg::Operation {
                lhs: Box::new(left),
                op,
                rhs: Box::new(right),
            });
        }

        match (left, right) {
            (CalculationArg::Number(num1), CalculationArg::Number(num2)) => {
                if op == BinaryOp::Mul {
                    Ok(CalculationArg::Number(num1 * num2))
                } else {
                    Ok(CalculationArg::Number(num1 / num2))
                }
            }
            (left, right) => Ok(CalculationArg::Operation {
                lhs: Box::new(left),
                op,
                rhs: Box::new(right),
            }),
        }

        //   _verifyCompatibleNumbers([left, right]);

        // Ok(CalculationArg::Operation {
        //     lhs: Box::new(left),
        //     op,
        //     rhs: Box::new(right),
        // })
    }

    fn simplify(arg: CalculationArg) -> CalculationArg {
        match arg {
            CalculationArg::Number(..)
            | CalculationArg::Operation { .. }
            | CalculationArg::Interpolation(..)
            | CalculationArg::String(..) => arg,
            CalculationArg::Calculation(mut calc) => {
                if calc.name == CalculationName::Calc {
                    calc.args.remove(0)
                } else {
                    CalculationArg::Calculation(calc)
                }
            }
        }
    }

    fn simplify_arguments(args: Vec<CalculationArg>) -> Vec<CalculationArg> {
        args.into_iter().map(Self::simplify).collect()
    }
}
