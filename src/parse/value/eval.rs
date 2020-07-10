#![allow(unused_variables)]

use std::cmp::Ordering;

use codemap::{Span, Spanned};

use crate::{
    args::CallArgs,
    common::{Op, QuoteKind},
    error::SassResult,
    unit::{Unit, UNIT_CONVERSION_TABLE},
    value::{SassFunction, Value},
};

use super::super::Parser;

#[derive(Clone, Debug)]
pub(crate) enum HigherIntermediateValue {
    Literal(Value),
    /// A function that hasn't yet been evaluated
    Function(SassFunction, CallArgs),
    BinaryOp(Box<Self>, Op, Box<Self>),
    UnaryOp(Op, Box<Self>),
    Paren(Box<Self>),
}

impl HigherIntermediateValue {
    pub const fn span(self, span: Span) -> Spanned<Self> {
        Spanned { node: self, span }
    }
}

impl<'a> Parser<'a> {
    fn call_function(&mut self, function: SassFunction, args: CallArgs) -> SassResult<Value> {
        function.call(args, self)
    }
}

pub(crate) struct ValueVisitor<'a, 'b: 'a> {
    parser: &'a mut Parser<'b>,
    span: Span,
}

impl<'a, 'b: 'a> ValueVisitor<'a, 'b> {
    pub fn new(parser: &'a mut Parser<'b>, span: Span) -> Self {
        Self { parser, span }
    }

    pub fn eval(&mut self, value: HigherIntermediateValue, in_parens: bool) -> SassResult<Value> {
        match value {
            HigherIntermediateValue::Literal(Value::Dimension(n, u, _)) if in_parens => {
                Ok(Value::Dimension(n, u, true))
            }
            HigherIntermediateValue::Literal(v) => Ok(v),
            HigherIntermediateValue::BinaryOp(v1, op, v2) => self.bin_op(*v1, op, *v2, in_parens),
            HigherIntermediateValue::UnaryOp(op, val) => self.unary_op(op, *val, in_parens),
            HigherIntermediateValue::Paren(val) => self.eval(*val, true),
            HigherIntermediateValue::Function(function, args) => {
                self.parser.call_function(function, args)
            }
        }
    }

    fn bin_op(
        &mut self,
        val1: HigherIntermediateValue,
        op: Op,
        val2: HigherIntermediateValue,
        in_parens: bool,
    ) -> SassResult<Value> {
        let mut val1 = self.paren_or_unary(val1, in_parens)?;
        let val2 = self.paren_or_unary(val2, in_parens)?;

        if let HigherIntermediateValue::BinaryOp(val1_1, op2, val1_2) = val1 {
            let in_parens = op != Op::Div || op2 != Op::Div;
            if op2.precedence() >= op.precedence() {
                val1 = HigherIntermediateValue::Literal(
                    self.bin_op(*val1_1, op2, *val1_2, in_parens)?,
                );
            } else {
                let val2 =
                    HigherIntermediateValue::Literal(self.bin_op(*val1_2, op, val2, in_parens)?);
                return self.bin_op(*val1_1, op2, val2, in_parens);
            }
        }

        Ok(match op {
            Op::Plus => self.add(val1, val2)?,
            Op::Minus => self.sub(val1, val2)?,
            Op::Mul => self.mul(val1, val2)?,
            Op::Div => self.div(val1, val2, in_parens)?,
            Op::Rem => self.rem(val1, val2)?,
            Op::And => Self::and(val1, val2)?,
            Op::Or => Self::or(val1, val2)?,
            Op::Equal => Self::equal(val1, val2),
            Op::NotEqual => Self::not_equal(val1, val2),
            Op::GreaterThan => self.greater_than(val1, val2)?,
            Op::GreaterThanEqual => self.greater_than_or_equal(val1, val2)?,
            Op::LessThan => self.less_than(val1, val2)?,
            Op::LessThanEqual => self.less_than_or_equal(val1, val2)?,
            Op::Not => unreachable!(),
        })
    }

    fn unary_op(
        &mut self,
        op: Op,
        val: HigherIntermediateValue,
        in_parens: bool,
    ) -> SassResult<Value> {
        let val = self.eval(val, in_parens)?;
        match op {
            Op::Minus => self.unary_minus(val),
            Op::Not => Self::unary_not(&val),
            Op::Plus => self.unary_plus(val),
            _ => unreachable!(),
        }
    }

    fn unary_minus(&self, val: Value) -> SassResult<Value> {
        Ok(match val {
            Value::Dimension(n, u, should_divide) => Value::Dimension(-n, u, should_divide),
            v => Value::String(format!("-{}", v.to_css_string(self.span)?), QuoteKind::None),
        })
    }

    fn unary_plus(&self, val: Value) -> SassResult<Value> {
        Ok(match val {
            v @ Value::Dimension(..) => v,
            v => Value::String(format!("+{}", v.to_css_string(self.span)?), QuoteKind::None),
        })
    }

    fn unary_not(val: &Value) -> SassResult<Value> {
        Ok(Value::bool(!val.is_true()))
    }

    fn paren(&mut self, val: HigherIntermediateValue) -> SassResult<HigherIntermediateValue> {
        Ok(if let HigherIntermediateValue::Paren(v) = val {
            HigherIntermediateValue::Literal(self.eval(*v, true)?)
        } else {
            val
        })
    }

    fn paren_or_unary(
        &mut self,
        val: HigherIntermediateValue,
        in_parens: bool,
    ) -> SassResult<HigherIntermediateValue> {
        let val = self.paren(val)?;
        Ok(match val {
            HigherIntermediateValue::UnaryOp(op, val) => {
                HigherIntermediateValue::Literal(self.unary_op(op, *val, in_parens)?)
            }
            HigherIntermediateValue::Function(function, args) => {
                HigherIntermediateValue::Literal(self.parser.call_function(function, args)?)
            }
            val => val,
        })
    }

    fn add(
        &self,
        left: HigherIntermediateValue,
        right: HigherIntermediateValue,
    ) -> SassResult<Value> {
        let left = match left {
            HigherIntermediateValue::Literal(v) => v,
            v => panic!("{:?}", v),
        };
        let right = match right {
            HigherIntermediateValue::Literal(v) => v,
            v => panic!("{:?}", v),
        };
        Ok(match left {
            Value::Map(..) | Value::FunctionRef(..) => {
                return Err((
                    format!("{} isn't a valid CSS value.", left.inspect(self.span)?),
                    self.span,
                )
                    .into())
            }
            Value::Important | Value::True | Value::False => match right {
                Value::String(s, QuoteKind::Quoted) => Value::String(
                    format!("{}{}", left.to_css_string(self.span)?, s),
                    QuoteKind::Quoted,
                ),
                Value::Null => {
                    Value::String(left.to_css_string(self.span)?.into_owned(), QuoteKind::None)
                }
                _ => Value::String(
                    format!(
                        "{}{}",
                        left.to_css_string(self.span)?,
                        right.to_css_string(self.span)?
                    ),
                    QuoteKind::None,
                ),
            },
            Value::Null => match right {
                Value::Null => Value::Null,
                _ => Value::String(
                    right.to_css_string(self.span)?.into_owned(),
                    QuoteKind::None,
                ),
            },
            Value::Dimension(num, unit, _) => match right {
                Value::Dimension(num2, unit2, _) => {
                    if !unit.comparable(&unit2) {
                        return Err((
                            format!("Incompatible units {} and {}.", unit2, unit),
                            self.span,
                        )
                            .into());
                    }
                    if unit == unit2 {
                        Value::Dimension(num + num2, unit, true)
                    } else if unit == Unit::None {
                        Value::Dimension(num + num2, unit2, true)
                    } else if unit2 == Unit::None {
                        Value::Dimension(num + num2, unit, true)
                    } else {
                        Value::Dimension(
                            num + num2
                                * UNIT_CONVERSION_TABLE[unit.to_string().as_str()]
                                    [unit2.to_string().as_str()]
                                .clone(),
                            unit,
                            true,
                        )
                    }
                }
                Value::String(s, q) => Value::String(format!("{}{}{}", num, unit, s), q),
                Value::Null => Value::String(format!("{}{}", num, unit), QuoteKind::None),
                Value::True | Value::False | Value::List(..) => Value::String(
                    format!("{}{}{}", num, unit, right.to_css_string(self.span)?),
                    QuoteKind::None,
                ),
                Value::Map(..) | Value::FunctionRef(..) => {
                    return Err((
                        format!("{} isn't a valid CSS value.", right.inspect(self.span)?),
                        self.span,
                    )
                        .into())
                }
                _ => {
                    return Err((
                        format!(
                            "Undefined operation \"{}{} + {}\".",
                            num,
                            unit,
                            right.inspect(self.span)?
                        ),
                        self.span,
                    )
                        .into())
                }
            },
            Value::Color(c) => match right {
                Value::String(s, q) => Value::String(format!("{}{}", c, s), q),
                Value::Null => Value::String(c.to_string(), QuoteKind::None),
                Value::List(..) => Value::String(
                    format!("{}{}", c, right.to_css_string(self.span)?),
                    QuoteKind::None,
                ),
                _ => {
                    return Err((
                        format!(
                            "Undefined operation \"{} + {}\".",
                            c,
                            right.inspect(self.span)?
                        ),
                        self.span,
                    )
                        .into())
                }
            },
            Value::String(text, quotes) => match right {
                Value::String(text2, ..) => Value::String(text + &text2, quotes),
                _ => Value::String(text + &right.to_css_string(self.span)?, quotes),
            },
            Value::List(..) | Value::ArgList(..) => match right {
                Value::String(s, q) => {
                    Value::String(format!("{}{}", left.to_css_string(self.span)?, s), q)
                }
                _ => Value::String(
                    format!(
                        "{}{}",
                        left.to_css_string(self.span)?,
                        right.to_css_string(self.span)?
                    ),
                    QuoteKind::None,
                ),
            },
        })
    }

    fn sub(
        &self,
        left: HigherIntermediateValue,
        right: HigherIntermediateValue,
    ) -> SassResult<Value> {
        let left = match left {
            HigherIntermediateValue::Literal(v) => v,
            v => panic!("{:?}", v),
        };
        let right = match right {
            HigherIntermediateValue::Literal(v) => v,
            v => panic!("{:?}", v),
        };
        Ok(match left {
            Value::Null => Value::String(
                format!("-{}", right.to_css_string(self.span)?),
                QuoteKind::None,
            ),
            Value::Dimension(num, unit, _) => match right {
                Value::Dimension(num2, unit2, _) => {
                    if !unit.comparable(&unit2) {
                        return Err((
                            format!("Incompatible units {} and {}.", unit2, unit),
                            self.span,
                        )
                            .into());
                    }
                    if unit == unit2 {
                        Value::Dimension(num - num2, unit, true)
                    } else if unit == Unit::None {
                        Value::Dimension(num - num2, unit2, true)
                    } else if unit2 == Unit::None {
                        Value::Dimension(num - num2, unit, true)
                    } else {
                        Value::Dimension(
                            num - num2
                                * UNIT_CONVERSION_TABLE[unit.to_string().as_str()]
                                    [unit2.to_string().as_str()]
                                .clone(),
                            unit,
                            true,
                        )
                    }
                }
                Value::List(..)
                | Value::String(..)
                | Value::Important
                | Value::True
                | Value::False
                | Value::ArgList(..) => Value::String(
                    format!("{}{}-{}", num, unit, right.to_css_string(self.span)?),
                    QuoteKind::None,
                ),
                Value::Map(..) | Value::FunctionRef(..) => {
                    return Err((
                        format!("{} isn't a valid CSS value.", right.inspect(self.span)?),
                        self.span,
                    )
                        .into())
                }
                Value::Color(..) => {
                    return Err((
                        format!(
                            "Undefined operation \"{}{} - {}\".",
                            num,
                            unit,
                            right.inspect(self.span)?
                        ),
                        self.span,
                    )
                        .into())
                }
                Value::Null => Value::String(format!("{}{}-", num, unit), QuoteKind::None),
            },
            Value::Color(c) => match right {
                Value::String(s, q) => {
                    Value::String(format!("{}-{}{}{}", c, q, s, q), QuoteKind::None)
                }
                Value::Null => Value::String(format!("{}-", c), QuoteKind::None),
                Value::Dimension(..) | Value::Color(..) => {
                    return Err((
                        format!(
                            "Undefined operation \"{} - {}\".",
                            c,
                            right.inspect(self.span)?
                        ),
                        self.span,
                    )
                        .into())
                }
                _ => Value::String(
                    format!("{}-{}", c, right.to_css_string(self.span)?),
                    QuoteKind::None,
                ),
            },
            Value::String(..) => Value::String(
                format!(
                    "{}-{}",
                    left.to_css_string(self.span)?,
                    right.to_css_string(self.span)?
                ),
                QuoteKind::None,
            ),
            _ => match right {
                Value::String(s, q) => Value::String(
                    format!("{}-{}{}{}", left.to_css_string(self.span)?, q, s, q),
                    QuoteKind::None,
                ),
                Value::Null => Value::String(
                    format!("{}-", left.to_css_string(self.span)?),
                    QuoteKind::None,
                ),
                _ => Value::String(
                    format!(
                        "{}-{}",
                        left.to_css_string(self.span)?,
                        right.to_css_string(self.span)?
                    ),
                    QuoteKind::None,
                ),
            },
        })
    }

    fn mul(
        &self,
        left: HigherIntermediateValue,
        right: HigherIntermediateValue,
    ) -> SassResult<Value> {
        let left = match left {
            HigherIntermediateValue::Literal(v) => v,
            v => panic!("{:?}", v),
        };
        let right = match right {
            HigherIntermediateValue::Literal(v) => v,
            v => panic!("{:?}", v),
        };
        Ok(match left {
            Value::Dimension(num, unit, _) => match right {
                Value::Dimension(num2, unit2, _) => {
                    if unit == Unit::None {
                        Value::Dimension(num * num2, unit2, true)
                    } else if unit2 == Unit::None {
                        Value::Dimension(num * num2, unit, true)
                    } else {
                        Value::Dimension(num * num2, unit * unit2, true)
                    }
                }
                _ => {
                    return Err((
                        format!(
                            "Undefined operation \"{}{} * {}\".",
                            num,
                            unit,
                            right.inspect(self.span)?
                        ),
                        self.span,
                    )
                        .into())
                }
            },
            _ => {
                return Err((
                    format!(
                        "Undefined operation \"{} * {}\".",
                        left.inspect(self.span)?,
                        right.inspect(self.span)?
                    ),
                    self.span,
                )
                    .into())
            }
        })
    }

    fn div(
        &self,
        left: HigherIntermediateValue,
        right: HigherIntermediateValue,
        in_parens: bool,
    ) -> SassResult<Value> {
        let left = match left {
            HigherIntermediateValue::Literal(v) => v,
            v => panic!("{:?}", v),
        };
        let right = match right {
            HigherIntermediateValue::Literal(v) => v,
            v => panic!("{:?}", v),
        };
        Ok(match left {
            Value::Null => Value::String(
                format!("/{}", right.to_css_string(self.span)?),
                QuoteKind::None,
            ),
            Value::Dimension(num, unit, should_divide1) => match right {
                Value::Dimension(num2, unit2, should_divide2) => {
                    if should_divide1 || should_divide2 || in_parens {
                        // `unit(1em / 1em)` => `""`
                        if unit == unit2 {
                            Value::Dimension(num / num2, Unit::None, true)

                        // `unit(1 / 1em)` => `"em^-1"`
                        } else if unit == Unit::None {
                            Value::Dimension(num / num2, Unit::None / unit2, true)

                        // `unit(1em / 1)` => `"em"`
                        } else if unit2 == Unit::None {
                            Value::Dimension(num / num2, unit, true)

                        // `unit(1in / 1px)` => `""`
                        } else if unit.comparable(&unit2) {
                            Value::Dimension(
                                num / (num2
                                    * UNIT_CONVERSION_TABLE[unit.to_string().as_str()]
                                        [unit2.to_string().as_str()]
                                    .clone()),
                                Unit::None,
                                true,
                            )
                        // `unit(1em / 1px)` => `"em/px"`
                        // todo: this should probably be its own variant
                        // within the `Value` enum
                        } else {
                            // todo: remember to account for `Mul` and `Div`
                            todo!("non-comparable inverse units")
                        }
                    } else {
                        Value::String(
                            format!("{}{}/{}{}", num, unit, num2, unit2),
                            QuoteKind::None,
                        )
                    }
                }
                Value::String(s, q) => {
                    Value::String(format!("{}{}/{}{}{}", num, unit, q, s, q), QuoteKind::None)
                }
                Value::List(..)
                | Value::True
                | Value::False
                | Value::Important
                | Value::Color(..)
                | Value::ArgList(..) => Value::String(
                    format!("{}{}/{}", num, unit, right.to_css_string(self.span)?),
                    QuoteKind::None,
                ),
                Value::Null => Value::String(format!("{}{}/", num, unit), QuoteKind::None),
                Value::Map(..) | Value::FunctionRef(..) => {
                    return Err((
                        format!("{} isn't a valid CSS value.", right.inspect(self.span)?),
                        self.span,
                    )
                        .into())
                }
            },
            Value::Color(c) => match right {
                Value::String(s, q) => {
                    Value::String(format!("{}/{}{}{}", c, q, s, q), QuoteKind::None)
                }
                Value::Null => Value::String(format!("{}/", c), QuoteKind::None),
                Value::Dimension(..) | Value::Color(..) => {
                    return Err((
                        format!(
                            "Undefined operation \"{} / {}\".",
                            c,
                            right.inspect(self.span)?
                        ),
                        self.span,
                    )
                        .into())
                }
                _ => Value::String(
                    format!("{}/{}", c, right.to_css_string(self.span)?),
                    QuoteKind::None,
                ),
            },
            Value::String(s1, q1) => match right {
                Value::String(s2, q2) => Value::String(
                    format!("{}{}{}/{}{}{}", q1, s1, q1, q2, s2, q2),
                    QuoteKind::None,
                ),
                Value::Important
                | Value::True
                | Value::False
                | Value::Dimension(..)
                | Value::Color(..)
                | Value::List(..)
                | Value::ArgList(..) => Value::String(
                    format!("{}{}{}/{}", q1, s1, q1, right.to_css_string(self.span)?),
                    QuoteKind::None,
                ),
                Value::Null => Value::String(format!("{}{}{}/", q1, s1, q1), QuoteKind::None),
                Value::Map(..) | Value::FunctionRef(..) => {
                    return Err((
                        format!("{} isn't a valid CSS value.", right.inspect(self.span)?),
                        self.span,
                    )
                        .into())
                }
            },
            _ => match right {
                Value::String(s, q) => Value::String(
                    format!("{}/{}{}{}", left.to_css_string(self.span)?, q, s, q),
                    QuoteKind::None,
                ),
                Value::Null => Value::String(
                    format!("{}/", left.to_css_string(self.span)?),
                    QuoteKind::None,
                ),
                _ => Value::String(
                    format!(
                        "{}/{}",
                        left.to_css_string(self.span)?,
                        right.to_css_string(self.span)?
                    ),
                    QuoteKind::None,
                ),
            },
        })
    }

    fn rem(
        &self,
        left: HigherIntermediateValue,
        right: HigherIntermediateValue,
    ) -> SassResult<Value> {
        let left = match left {
            HigherIntermediateValue::Literal(v) => v,
            v => panic!("{:?}", v),
        };
        let right = match right {
            HigherIntermediateValue::Literal(v) => v,
            v => panic!("{:?}", v),
        };
        Ok(match left {
            Value::Dimension(n, u, _) => match right {
                Value::Dimension(n2, u2, _) => {
                    if !u.comparable(&u2) {
                        return Err(
                            (format!("Incompatible units {} and {}.", u2, u), self.span).into()
                        );
                    }
                    if u == u2 {
                        Value::Dimension(n % n2, u, true)
                    } else if u == Unit::None {
                        Value::Dimension(n % n2, u2, true)
                    } else if u2 == Unit::None {
                        Value::Dimension(n % n2, u, true)
                    } else {
                        Value::Dimension(n, u, true)
                    }
                }
                _ => {
                    return Err((
                        format!(
                            "Undefined operation \"{} % {}\".",
                            Value::Dimension(n, u, true).inspect(self.span)?,
                            right.inspect(self.span)?
                        ),
                        self.span,
                    )
                        .into())
                }
            },
            _ => {
                return Err((
                    format!(
                        "Undefined operation \"{} % {}\".",
                        left.inspect(self.span)?,
                        right.inspect(self.span)?
                    ),
                    self.span,
                )
                    .into())
            }
        })
    }

    fn and(left: HigherIntermediateValue, right: HigherIntermediateValue) -> SassResult<Value> {
        let left = match left {
            HigherIntermediateValue::Literal(v) => v,
            v => panic!("{:?}", v),
        };
        let right = match right {
            HigherIntermediateValue::Literal(v) => v,
            v => panic!("{:?}", v),
        };
        Ok(if left.is_true() { right } else { left })
    }

    fn or(left: HigherIntermediateValue, right: HigherIntermediateValue) -> SassResult<Value> {
        let left = match left {
            HigherIntermediateValue::Literal(v) => v,
            v => panic!("{:?}", v),
        };
        let right = match right {
            HigherIntermediateValue::Literal(v) => v,
            v => panic!("{:?}", v),
        };
        Ok(if left.is_true() { left } else { right })
    }

    pub fn equal(left: HigherIntermediateValue, right: HigherIntermediateValue) -> Value {
        let left = match left {
            HigherIntermediateValue::Literal(v) => v,
            v => panic!("{:?}", v),
        };
        let right = match right {
            HigherIntermediateValue::Literal(v) => v,
            v => panic!("{:?}", v),
        };
        Value::bool(left.equals(&right))
    }

    fn not_equal(left: HigherIntermediateValue, right: HigherIntermediateValue) -> Value {
        let left = match left {
            HigherIntermediateValue::Literal(v) => v,
            v => panic!("{:?}", v),
        };
        let right = match right {
            HigherIntermediateValue::Literal(v) => v,
            v => panic!("{:?}", v),
        };
        Value::bool(left.not_equals(&right))
    }

    fn cmp(
        &self,
        left: HigherIntermediateValue,
        op: Op,
        right: HigherIntermediateValue,
    ) -> SassResult<Value> {
        let left = match left {
            HigherIntermediateValue::Literal(v) => v,
            v => panic!("{:?}", v),
        };
        let right = match right {
            HigherIntermediateValue::Literal(v) => v,
            v => panic!("{:?}", v),
        };
        let ordering = match left {
            Value::Dimension(num, unit, _) => match &right {
                Value::Dimension(num2, unit2, _) => {
                    if !unit.comparable(unit2) {
                        return Err((
                            format!("Incompatible units {} and {}.", unit2, unit),
                            self.span,
                        )
                            .into());
                    }
                    if &unit == unit2 || unit == Unit::None || unit2 == &Unit::None {
                        num.cmp(num2)
                    } else {
                        num.cmp(
                            &(num2.clone()
                                * UNIT_CONVERSION_TABLE[unit.to_string().as_str()]
                                    [unit2.to_string().as_str()]
                                .clone()),
                        )
                    }
                }
                v => {
                    return Err((
                        format!(
                            "Undefined operation \"{} {} {}\".",
                            v.inspect(self.span)?,
                            op,
                            right.inspect(self.span)?
                        ),
                        self.span,
                    )
                        .into())
                }
            },
            _ => {
                return Err((
                    format!(
                        "Undefined operation \"{} {} {}\".",
                        left.inspect(self.span)?,
                        op,
                        right.inspect(self.span)?
                    ),
                    self.span,
                )
                    .into())
            }
        };
        Ok(match op {
            Op::GreaterThan => match ordering {
                Ordering::Greater => Value::True,
                Ordering::Less | Ordering::Equal => Value::False,
            },
            Op::GreaterThanEqual => match ordering {
                Ordering::Greater | Ordering::Equal => Value::True,
                Ordering::Less => Value::False,
            },
            Op::LessThan => match ordering {
                Ordering::Less => Value::True,
                Ordering::Greater | Ordering::Equal => Value::False,
            },
            Op::LessThanEqual => match ordering {
                Ordering::Less | Ordering::Equal => Value::True,
                Ordering::Greater => Value::False,
            },
            _ => unreachable!(),
        })
    }

    pub fn greater_than(
        &self,
        left: HigherIntermediateValue,
        right: HigherIntermediateValue,
    ) -> SassResult<Value> {
        self.cmp(left, Op::GreaterThan, right)
    }

    fn greater_than_or_equal(
        &self,
        left: HigherIntermediateValue,
        right: HigherIntermediateValue,
    ) -> SassResult<Value> {
        self.cmp(left, Op::GreaterThanEqual, right)
    }

    pub fn less_than(
        &self,
        left: HigherIntermediateValue,
        right: HigherIntermediateValue,
    ) -> SassResult<Value> {
        self.cmp(left, Op::LessThan, right)
    }

    fn less_than_or_equal(
        &self,
        left: HigherIntermediateValue,
        right: HigherIntermediateValue,
    ) -> SassResult<Value> {
        self.cmp(left, Op::LessThanEqual, right)
    }
}
