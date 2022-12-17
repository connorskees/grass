use core::fmt;
use std::iter::Iterator;

use codemap::Span;

use crate::{
    common::BinaryOp,
    error::SassResult,
    value::{Number, SassNumber, Value},
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

                Self::write_calculation_value(buf, &**lhs, is_compressed, span)?;

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

                Self::write_calculation_value(buf, &**rhs, is_compressed, span)?;

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
    pub fn in_min_or_max(&self) -> bool {
        *self == CalculationName::Min || *self == CalculationName::Max
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
        let arg = Self::simplify(arg)?;
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
        let args = Self::simplify_arguments(args)?;
        if args.is_empty() {
            todo!("min() must have at least one argument.")
        }

        let mut minimum: Option<SassNumber> = None;

        for arg in args.iter() {
            match arg {
                CalculationArg::Number(n)
                    if minimum.is_some() && !minimum.as_ref().unwrap().is_comparable_to(&n) =>
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
                _ => break,
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
        let args = Self::simplify_arguments(args)?;
        if args.is_empty() {
            todo!("max() must have at least one argument.")
        }

        let mut maximum: Option<SassNumber> = None;

        for arg in args.iter() {
            match arg {
                CalculationArg::Number(n)
                    if maximum.is_some() && !maximum.as_ref().unwrap().is_comparable_to(&n) =>
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
                _ => break,
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
    ) -> SassResult<Value> {
        todo!()
    }

    pub fn operate_internal(
        mut op: BinaryOp,
        left: CalculationArg,
        right: CalculationArg,
        in_min_or_max: bool,
        simplify: bool,
    ) -> SassResult<CalculationArg> {
        if !simplify {
            return Ok(CalculationArg::Operation {
                lhs: Box::new(left),
                op,
                rhs: Box::new(right),
            });
        }

        let left = Self::simplify(left)?;
        let mut right = Self::simplify(right)?;

        if op == BinaryOp::Plus || op == BinaryOp::Minus {
            let is_comparable = if in_min_or_max {
                // todo:
                // left.isComparableTo(right)
                true
            } else {
                // left.hasCompatibleUnits(right)
                true
            };
            if matches!(left, CalculationArg::Number(..))
                && matches!(right, CalculationArg::Number(..))
                && is_comparable
            {
                return Ok(CalculationArg::Operation {
                    lhs: Box::new(left),
                    op,
                    rhs: Box::new(right),
                });
            }

            if let CalculationArg::Number(mut n) = right {
                if n.num.is_negative() {
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
        }

        //   _verifyCompatibleNumbers([left, right]);

        Ok(CalculationArg::Operation {
            lhs: Box::new(left),
            op,
            rhs: Box::new(right),
        })
    }

    fn simplify(arg: CalculationArg) -> SassResult<CalculationArg> {
        Ok(match arg {
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
        })
    }

    fn simplify_arguments(args: Vec<CalculationArg>) -> SassResult<Vec<CalculationArg>> {
        args.into_iter().map(Self::simplify).collect()
    }
}
