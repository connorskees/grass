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

#[derive(Clone, Debug, Eq, PartialEq)]
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

    pub fn eval(&mut self, value: HigherIntermediateValue) -> SassResult<Value> {
        match value {
            HigherIntermediateValue::Literal(v) => Ok(v),
            HigherIntermediateValue::BinaryOp(v1, op, v2) => self.bin_op(*v1, op, *v2),
            HigherIntermediateValue::UnaryOp(op, val) => self.unary_op(op, *val),
            HigherIntermediateValue::Paren(val) => self.eval(*val),
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
    ) -> SassResult<Value> {
        let mut val1 = self.paren_or_unary(val1)?;
        let val2 = self.paren_or_unary(val2)?;

        if let HigherIntermediateValue::BinaryOp(val1_1, op2, val1_2) = val1 {
            if op2.precedence() > op.precedence() {
                val1 = HigherIntermediateValue::Literal(self.bin_op(*val1_1, op2, *val1_2)?);
            } else {
                let val2 = HigherIntermediateValue::Literal(self.bin_op(*val1_2, op, val2)?);
                return self.bin_op(*val1_1, op2, val2);
            }
        }

        Ok(match op {
            Op::Plus => self.add(val1, val2)?,
            Op::Minus => self.sub(val1, val2)?,
            Op::Mul => self.mul(val1, val2)?,
            Op::Div => self.div(val1, val2)?,
            Op::Rem => self.rem(val1, val2)?,
            Op::And => Self::and(val1, val2)?,
            Op::Or => Self::or(val1, val2)?,
            Op::Equal => self.equal(val1, val2)?,
            Op::NotEqual => self.not_equal(val1, val2)?,
            Op::GreaterThan => self.greater_than(val1, val2)?,
            Op::GreaterThanEqual => self.greater_than_or_equal(val1, val2)?,
            Op::LessThan => self.less_than(val1, val2)?,
            Op::LessThanEqual => self.less_than_or_equal(val1, val2)?,
            Op::Not => unreachable!(),
        })
    }

    fn unary_op(&mut self, op: Op, val: HigherIntermediateValue) -> SassResult<Value> {
        let val = self.eval(val)?;
        match op {
            Op::Minus => self.unary_minus(val),
            Op::Not => Self::unary_not(&val),
            Op::Plus => self.unary_plus(val),
            _ => unreachable!(),
        }
    }

    fn unary_minus(&self, val: Value) -> SassResult<Value> {
        Ok(match val {
            Value::Dimension(n, u) => Value::Dimension(-n, u),
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
            HigherIntermediateValue::Literal(self.eval(*v)?)
        } else {
            val
        })
    }

    fn paren_or_unary(
        &mut self,
        val: HigherIntermediateValue,
    ) -> SassResult<HigherIntermediateValue> {
        let val = self.paren(val)?;
        Ok(match val {
            HigherIntermediateValue::UnaryOp(op, val) => {
                HigherIntermediateValue::Literal(self.unary_op(op, *val)?)
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
            Value::ArgList(..) => todo!(),
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
            Value::Dimension(num, unit) => match right {
                Value::Dimension(num2, unit2) => {
                    if !unit.comparable(&unit2) {
                        return Err((
                            format!("Incompatible units {} and {}.", unit2, unit),
                            self.span,
                        )
                            .into());
                    }
                    if unit == unit2 {
                        Value::Dimension(num + num2, unit)
                    } else if unit == Unit::None {
                        Value::Dimension(num + num2, unit2)
                    } else if unit2 == Unit::None {
                        Value::Dimension(num + num2, unit)
                    } else {
                        Value::Dimension(
                            num + num2
                                * UNIT_CONVERSION_TABLE[unit.to_string().as_str()]
                                    [unit2.to_string().as_str()]
                                .clone(),
                            unit,
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
            Value::List(..) => match right {
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
            Value::Dimension(num, unit) => match right {
                Value::Dimension(num2, unit2) => {
                    if !unit.comparable(&unit2) {
                        return Err((
                            format!("Incompatible units {} and {}.", unit2, unit),
                            self.span,
                        )
                            .into());
                    }
                    if unit == unit2 {
                        Value::Dimension(num - num2, unit)
                    } else if unit == Unit::None {
                        Value::Dimension(num - num2, unit2)
                    } else if unit2 == Unit::None {
                        Value::Dimension(num - num2, unit)
                    } else {
                        Value::Dimension(
                            num - num2
                                * UNIT_CONVERSION_TABLE[unit.to_string().as_str()]
                                    [unit2.to_string().as_str()]
                                .clone(),
                            unit,
                        )
                    }
                }
                Value::List(..) | Value::String(..) => Value::String(
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
                _ => todo!(),
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
            Value::List(..) => match right {
                Value::String(s, q) => Value::String(
                    format!("{}-{}{}{}", left.to_css_string(self.span)?, q, s, q),
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
            Value::Null => todo!(),
            Value::Dimension(num, unit) => match right {
                Value::Dimension(num2, unit2) => {
                    if unit == Unit::None {
                        Value::Dimension(num * num2, unit2)
                    } else if unit2 == Unit::None {
                        Value::Dimension(num * num2, unit)
                    } else if let Unit::Mul(u) = unit {
                        let mut unit1 = u.into_vec();
                        unit1.push(unit2);
                        Value::Dimension(num * num2, Unit::Mul(unit1.into_boxed_slice()))
                    } else if let Unit::Mul(u2) = unit2 {
                        let mut u = vec![unit];
                        u.append(&mut u2.into_vec());
                        Value::Dimension(num * num2, Unit::Mul(u.into_boxed_slice()))
                    } else {
                        Value::Dimension(
                            num * num2,
                            Unit::Mul(vec![unit, unit2].into_boxed_slice()),
                        )
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
            Value::Null => todo!(),
            Value::Dimension(num, unit) => match right {
                Value::Dimension(num2, unit2) => {
                    if !unit.comparable(&unit2) {
                        return Err((
                            format!("Incompatible units {} and {}.", unit2, unit),
                            self.span,
                        )
                            .into());
                    }
                    if unit == unit2 {
                        Value::Dimension(num / num2, Unit::None)
                    } else if unit == Unit::None {
                        todo!("inverse units")
                    } else if unit2 == Unit::None {
                        Value::Dimension(num / num2, unit)
                    } else {
                        Value::Dimension(
                            num / (num2
                                * UNIT_CONVERSION_TABLE[unit.to_string().as_str()]
                                    [unit2.to_string().as_str()]
                                .clone()),
                            Unit::None,
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
                | Value::Color(..) => Value::String(
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
                Value::ArgList(..) => todo!(),
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
                | Value::Color(..) => Value::String(
                    format!("{}{}{}/{}", q1, s1, q1, right.to_css_string(self.span)?),
                    QuoteKind::None,
                ),
                Value::Null => Value::String(format!("{}{}{}/", q1, s1, q1), QuoteKind::None),
                _ => todo!(),
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
            Value::Dimension(n, u) => match right {
                Value::Dimension(n2, u2) => {
                    if !u.comparable(&u2) {
                        return Err(
                            (format!("Incompatible units {} and {}.", u2, u), self.span).into()
                        );
                    }
                    if u == u2 {
                        Value::Dimension(n % n2, u)
                    } else if u == Unit::None {
                        Value::Dimension(n % n2, u2)
                    } else if u2 == Unit::None {
                        Value::Dimension(n % n2, u)
                    } else {
                        Value::Dimension(n, u)
                    }
                }
                _ => {
                    return Err((
                        format!(
                            "Undefined operation \"{} % {}\".",
                            Value::Dimension(n, u).inspect(self.span)?,
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

    pub fn equal(
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
        Ok(Value::bool(match left {
            Value::String(s1, ..) => match right {
                Value::String(s2, ..) => s1 == s2,
                _ => false,
            },
            Value::Dimension(n, unit) => match right {
                Value::Dimension(n2, unit2) => {
                    if !unit.comparable(&unit2) {
                        false
                    } else if unit == unit2 {
                        n == n2
                    } else if unit == Unit::None || unit2 == Unit::None {
                        false
                    } else {
                        n == (n2
                            * UNIT_CONVERSION_TABLE[unit.to_string().as_str()]
                                [unit2.to_string().as_str()]
                            .clone())
                    }
                }
                _ => false,
            },
            Value::List(list1, sep1, brackets1) => match right {
                Value::List(list2, sep2, brackets2) => {
                    if sep1 != sep2 || brackets1 != brackets2 || list1.len() != list2.len() {
                        false
                    } else {
                        let mut equals = true;
                        for (a, b) in list1.into_iter().zip(list2) {
                            if !self
                                .equal(
                                    HigherIntermediateValue::Literal(a),
                                    HigherIntermediateValue::Literal(b),
                                )?
                                .is_true()
                            {
                                equals = false;
                                break;
                            }
                        }
                        equals
                    }
                }
                _ => false,
            },
            s => s == right,
        }))
    }

    fn not_equal(
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
        Ok(Value::bool(match left {
            Value::String(s1, ..) => match right {
                Value::String(s2, ..) => s1 != s2,
                _ => true,
            },
            Value::Dimension(n, unit) => match right {
                Value::Dimension(n2, unit2) => {
                    if !unit.comparable(&unit2) {
                        true
                    } else if unit == unit2 {
                        n != n2
                    } else if unit == Unit::None || unit2 == Unit::None {
                        true
                    } else {
                        n != (n2
                            * UNIT_CONVERSION_TABLE[unit.to_string().as_str()]
                                [unit2.to_string().as_str()]
                            .clone())
                    }
                }
                _ => true,
            },
            Value::List(list1, sep1, brackets1) => match right {
                Value::List(list2, sep2, brackets2) => {
                    if sep1 != sep2 || brackets1 != brackets2 || list1.len() != list2.len() {
                        true
                    } else {
                        let mut equals = false;
                        for (a, b) in list1.into_iter().zip(list2) {
                            if self
                                .not_equal(
                                    HigherIntermediateValue::Literal(a),
                                    HigherIntermediateValue::Literal(b),
                                )?
                                .is_true()
                            {
                                equals = true;
                                break;
                            }
                        }
                        equals
                    }
                }
                _ => true,
            },
            s => s != right,
        }))
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
            Value::Dimension(num, unit) => match &right {
                Value::Dimension(num2, unit2) => {
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
