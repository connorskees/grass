#![allow(unused_variables)]

use std::cmp::Ordering;

use codemap::Span;
use num_traits::Zero;

use crate::{
    common::{BinaryOp, QuoteKind},
    error::SassResult,
    unit::Unit,
    value::Value,
    Options,
};

// use super::super::Parser;

// #[derive(Clone, Debug)]
// pub(crate) enum HigherIntermediateValue {
//     Literal(Value),
//     /// A function that hasn't yet been evaluated
//     Function(SassFunction, CallArgs, Option<Spanned<Identifier>>),
//     BinaryOp(Box<Self>, Op, Box<Self>),
//     UnaryOp(Op, Box<Self>),
// }

// impl HigherIntermediateValue {
//     pub const fn span(self, span: Span) -> Spanned<Self> {
//         Spanned { node: self, span }
//     }
// }

// impl<'a, 'b> Parser<'a, 'b> {
//     fn call_function(
//         &mut self,
//         function: SassFunction,
//         args: CallArgs,
//         module: Option<Spanned<Identifier>>,
//     ) -> SassResult<Value> {
//         todo!()
//         // function.call(args, module, self)
//     }
// }

pub(crate) fn add(left: Value, right: Value, options: &Options, span: Span) -> SassResult<Value> {
    Ok(match left {
        Value::Calculation(..) => todo!(),
        Value::Map(..) | Value::FunctionRef(..) => {
            return Err((
                format!("{} isn't a valid CSS value.", left.inspect(span)?),
                span,
            )
                .into())
        }
        Value::True | Value::False => match right {
            Value::String(s, QuoteKind::Quoted) => Value::String(
                format!(
                    "{}{}",
                    left.to_css_string(span, options.is_compressed())?,
                    s
                ),
                QuoteKind::Quoted,
            ),
            _ => Value::String(
                format!(
                    "{}{}",
                    left.to_css_string(span, options.is_compressed())?,
                    right.to_css_string(span, options.is_compressed())?
                ),
                QuoteKind::None,
            ),
        },
        Value::Null => match right {
            Value::Null => Value::Null,
            _ => Value::String(
                right
                    .to_css_string(span, options.is_compressed())?
                    .into_owned(),
                QuoteKind::None,
            ),
        },
        Value::Dimension(n, ..) if n.is_nan() => todo!(),
        Value::Dimension(num, unit, _) => match right {
            Value::Calculation(..) => todo!(),
            Value::Dimension(n, ..) if n.is_nan() => todo!(),
            Value::Dimension(num2, unit2, _) => {
                if !unit.comparable(&unit2) {
                    return Err(
                        (format!("Incompatible units {} and {}.", unit2, unit), span).into(),
                    );
                }
                if unit == unit2 {
                    Value::Dimension(num + num2, unit, None)
                } else if unit == Unit::None {
                    Value::Dimension(num + num2, unit2, None)
                } else if unit2 == Unit::None {
                    Value::Dimension(num + num2, unit, None)
                } else {
                    Value::Dimension(num + num2.convert(&unit2, &unit), unit, None)
                }
            }
            Value::String(s, q) => Value::String(
                format!("{}{}{}", num.to_string(options.is_compressed()), unit, s),
                q,
            ),
            Value::Null => Value::String(
                format!("{}{}", num.to_string(options.is_compressed()), unit),
                QuoteKind::None,
            ),
            Value::True | Value::False | Value::List(..) | Value::ArgList(..) => Value::String(
                format!(
                    "{}{}{}",
                    num.to_string(options.is_compressed()),
                    unit,
                    right.to_css_string(span, options.is_compressed())?
                ),
                QuoteKind::None,
            ),
            Value::Map(..) | Value::FunctionRef(..) => {
                return Err((
                    format!("{} isn't a valid CSS value.", right.inspect(span)?),
                    span,
                )
                    .into())
            }
            Value::Color(..) => {
                return Err((
                    format!(
                        "Undefined operation \"{}{} + {}\".",
                        num.inspect(),
                        unit,
                        right.inspect(span)?
                    ),
                    span,
                )
                    .into())
            }
        },
        Value::Color(c) => match right {
            Value::String(s, q) => Value::String(format!("{}{}", c, s), q),
            Value::Null => Value::String(c.to_string(), QuoteKind::None),
            Value::List(..) => Value::String(
                format!(
                    "{}{}",
                    c,
                    right.to_css_string(span, options.is_compressed())?
                ),
                QuoteKind::None,
            ),
            _ => {
                return Err((
                    format!("Undefined operation \"{} + {}\".", c, right.inspect(span)?),
                    span,
                )
                    .into())
            }
        },
        Value::String(text, quotes) => match right {
            Value::String(text2, ..) => Value::String(text + &text2, quotes),
            _ => Value::String(
                text + &right.to_css_string(span, options.is_compressed())?,
                quotes,
            ),
        },
        Value::List(..) | Value::ArgList(..) => match right {
            Value::String(s, q) => Value::String(
                format!(
                    "{}{}",
                    left.to_css_string(span, options.is_compressed())?,
                    s
                ),
                q,
            ),
            _ => Value::String(
                format!(
                    "{}{}",
                    left.to_css_string(span, options.is_compressed())?,
                    right.to_css_string(span, options.is_compressed())?
                ),
                QuoteKind::None,
            ),
        },
    })
}

pub(crate) fn sub(left: Value, right: Value, options: &Options, span: Span) -> SassResult<Value> {
    Ok(match left {
        Value::Calculation(..) => todo!(),
        Value::Null => Value::String(
            format!("-{}", right.to_css_string(span, options.is_compressed())?),
            QuoteKind::None,
        ),
        Value::Dimension(n, ..) if n.is_nan() => todo!(),
        Value::Dimension(num, unit, _) => match right {
            Value::Calculation(..) => todo!(),
            Value::Dimension(n, ..) if n.is_nan() => todo!(),
            Value::Dimension(num2, unit2, _) => {
                if !unit.comparable(&unit2) {
                    return Err(
                        (format!("Incompatible units {} and {}.", unit2, unit), span).into(),
                    );
                }
                if unit == unit2 {
                    Value::Dimension(num - num2, unit, None)
                } else if unit == Unit::None {
                    Value::Dimension(num - num2, unit2, None)
                } else if unit2 == Unit::None {
                    Value::Dimension(num - num2, unit, None)
                } else {
                    Value::Dimension(num - num2.convert(&unit2, &unit), unit, None)
                }
            }
            Value::List(..)
            | Value::String(..)
            | Value::True
            | Value::False
            | Value::ArgList(..) => Value::String(
                format!(
                    "{}{}-{}",
                    num.to_string(options.is_compressed()),
                    unit,
                    right.to_css_string(span, options.is_compressed())?
                ),
                QuoteKind::None,
            ),
            Value::Map(..) | Value::FunctionRef(..) => {
                return Err((
                    format!("{} isn't a valid CSS value.", right.inspect(span)?),
                    span,
                )
                    .into())
            }
            Value::Color(..) => {
                return Err((
                    format!(
                        "Undefined operation \"{}{} - {}\".",
                        num.inspect(),
                        unit,
                        right.inspect(span)?
                    ),
                    span,
                )
                    .into())
            }
            Value::Null => Value::String(
                format!("{}{}-", num.to_string(options.is_compressed()), unit),
                QuoteKind::None,
            ),
        },
        Value::Color(c) => match right {
            Value::String(s, q) => Value::String(format!("{}-{}{}{}", c, q, s, q), QuoteKind::None),
            Value::Null => Value::String(format!("{}-", c), QuoteKind::None),
            Value::Dimension(..) | Value::Color(..) => {
                return Err((
                    format!("Undefined operation \"{} - {}\".", c, right.inspect(span)?),
                    span,
                )
                    .into())
            }
            _ => Value::String(
                format!(
                    "{}-{}",
                    c,
                    right.to_css_string(span, options.is_compressed())?
                ),
                QuoteKind::None,
            ),
        },
        Value::String(..) => Value::String(
            format!(
                "{}-{}",
                left.to_css_string(span, options.is_compressed())?,
                right.to_css_string(span, options.is_compressed())?
            ),
            QuoteKind::None,
        ),
        // todo: can be greatly simplified
        _ => match right {
            Value::String(s, q) => Value::String(
                format!(
                    "{}-{}{}{}",
                    left.to_css_string(span, options.is_compressed())?,
                    q,
                    s,
                    q
                ),
                QuoteKind::None,
            ),
            Value::Null => Value::String(
                format!("{}-", left.to_css_string(span, options.is_compressed())?),
                QuoteKind::None,
            ),
            _ => Value::String(
                format!(
                    "{}-{}",
                    left.to_css_string(span, options.is_compressed())?,
                    right.to_css_string(span, options.is_compressed())?
                ),
                QuoteKind::None,
            ),
        },
    })
}

pub(crate) fn mul(left: Value, right: Value, options: &Options, span: Span) -> SassResult<Value> {
    Ok(match left {
        Value::Dimension(n, ..) if n.is_nan() => todo!(),
        Value::Dimension(num, unit, _) => match right {
            Value::Dimension(n, ..) if n.is_nan() => todo!(),
            Value::Dimension(num2, unit2, _) => {
                if unit == Unit::None {
                    Value::Dimension(num * num2, unit2, None)
                } else if unit2 == Unit::None {
                    Value::Dimension(num * num2, unit, None)
                } else {
                    Value::Dimension(num * num2, unit * unit2, None)
                }
            }
            _ => {
                return Err((
                    format!(
                        "Undefined operation \"{}{} * {}\".",
                        num.inspect(),
                        unit,
                        right.inspect(span)?
                    ),
                    span,
                )
                    .into())
            }
        },
        _ => {
            return Err((
                format!(
                    "Undefined operation \"{} * {}\".",
                    left.inspect(span)?,
                    right.inspect(span)?
                ),
                span,
            )
                .into())
        }
    })
}

pub(crate) fn cmp(
    left: Value,
    right: Value,
    options: &Options,
    span: Span,
    op: BinaryOp,
) -> SassResult<Value> {
    let ordering = left.cmp(&right, span, op)?;

    Ok(match op {
        BinaryOp::GreaterThan => match ordering {
            Ordering::Greater => Value::True,
            Ordering::Less | Ordering::Equal => Value::False,
        },
        BinaryOp::GreaterThanEqual => match ordering {
            Ordering::Greater | Ordering::Equal => Value::True,
            Ordering::Less => Value::False,
        },
        BinaryOp::LessThan => match ordering {
            Ordering::Less => Value::True,
            Ordering::Greater | Ordering::Equal => Value::False,
        },
        BinaryOp::LessThanEqual => match ordering {
            Ordering::Less | Ordering::Equal => Value::True,
            Ordering::Greater => Value::False,
        },
        _ => unreachable!(),
    })
}

pub(crate) fn single_eq(
    left: Value,
    right: Value,
    options: &Options,
    span: Span,
) -> SassResult<Value> {
    Ok(match left {
        _ => Value::String(
            format!(
                "{}={}",
                left.to_css_string(span, options.is_compressed())?,
                right.to_css_string(span, options.is_compressed())?
            ),
            QuoteKind::None,
        ),
    })
}

pub(crate) fn div(left: Value, right: Value, options: &Options, span: Span) -> SassResult<Value> {
    Ok(match left {
        Value::Calculation(..) => todo!(),
        Value::Null => Value::String(
            format!("/{}", right.to_css_string(span, options.is_compressed())?),
            QuoteKind::None,
        ),
        Value::Dimension(num, unit, as_slash1) => match right {
            Value::Calculation(..) => todo!(),
            Value::Dimension(num2, unit2, as_slash2) => {
                // if should_divide1 || should_divide2 {
                // if num.is_zero() && num2.is_zero() {
                //     // todo: nan
                //     // todo!()
                //     return Ok(Value::Dimension(NaN, Unit::None, true));
                // }

                // if num2.is_zero() {
                //     // todo: Infinity and -Infinity
                //     return Err(("Infinity not yet implemented.", span).into());
                // }

                // `unit(1em / 1em)` => `""`
                if unit == unit2 {
                    Value::Dimension(num / num2, Unit::None, None)

                // `unit(1 / 1em)` => `"em^-1"`
                } else if unit == Unit::None {
                    Value::Dimension(num / num2, Unit::None / unit2, None)

                // `unit(1em / 1)` => `"em"`
                } else if unit2 == Unit::None {
                    Value::Dimension(num / num2, unit, None)

                // `unit(1in / 1px)` => `""`
                } else if unit.comparable(&unit2) {
                    // let sass_num_1 = SassNumber(num.0, unit.clone(), as_slash1);
                    // let sass_num_2 = SassNumber(num2.0, unit2.clone(), as_slash2);
                    Value::Dimension(num / num2.convert(&unit2, &unit), Unit::None, None)
                    // Value::Dimension(num / num2.convert(&unit2, &unit), Unit::None, Some(Box::new((sass_num_1, sass_num_2))))
                    // `unit(1em / 1px)` => `"em/px"`
                    // todo: this should probably be its own variant
                    // within the `Value` enum
                } else {
                    // todo: remember to account for `Mul` and `Div`
                    // todo!("non-comparable inverse units")
                    return Err(
                        ("Division of non-comparable units not yet supported.", span).into(),
                    );
                }
                // } else {
                //     Value::String(
                //         format!(
                //             "{}{}/{}{}",
                //             num.to_string(options.is_compressed()),
                //             unit,
                //             num2.to_string(options.is_compressed()),
                //             unit2
                //         ),
                //         QuoteKind::None,
                //     )
                // }
            }
            Value::String(s, q) => Value::String(
                format!(
                    "{}{}/{}{}{}",
                    num.to_string(options.is_compressed()),
                    unit,
                    q,
                    s,
                    q
                ),
                QuoteKind::None,
            ),
            Value::List(..)
            | Value::True
            | Value::False
            | Value::Color(..)
            | Value::ArgList(..) => Value::String(
                format!(
                    "{}{}/{}",
                    num.to_string(options.is_compressed()),
                    unit,
                    right.to_css_string(span, options.is_compressed())?
                ),
                QuoteKind::None,
            ),
            Value::Null => Value::String(
                format!("{}{}/", num.to_string(options.is_compressed()), unit),
                QuoteKind::None,
            ),
            Value::Map(..) | Value::FunctionRef(..) => {
                return Err((
                    format!("{} isn't a valid CSS value.", right.inspect(span)?),
                    span,
                )
                    .into())
            }
        },
        Value::Color(c) => match right {
            Value::String(s, q) => Value::String(format!("{}/{}{}{}", c, q, s, q), QuoteKind::None),
            Value::Null => Value::String(format!("{}/", c), QuoteKind::None),
            Value::Dimension(..) | Value::Color(..) => {
                return Err((
                    format!("Undefined operation \"{} / {}\".", c, right.inspect(span)?),
                    span,
                )
                    .into())
            }
            _ => Value::String(
                format!(
                    "{}/{}",
                    c,
                    right.to_css_string(span, options.is_compressed())?
                ),
                QuoteKind::None,
            ),
        },
        Value::String(s1, q1) => match right {
            Value::Calculation(..) => todo!(),
            Value::String(s2, q2) => Value::String(
                format!("{}{}{}/{}{}{}", q1, s1, q1, q2, s2, q2),
                QuoteKind::None,
            ),
            Value::True
            | Value::False
            | Value::Dimension(..)
            | Value::Color(..)
            | Value::List(..)
            | Value::ArgList(..) => Value::String(
                format!(
                    "{}{}{}/{}",
                    q1,
                    s1,
                    q1,
                    right.to_css_string(span, options.is_compressed())?
                ),
                QuoteKind::None,
            ),
            Value::Null => Value::String(format!("{}{}{}/", q1, s1, q1), QuoteKind::None),
            Value::Map(..) | Value::FunctionRef(..) => {
                return Err((
                    format!("{} isn't a valid CSS value.", right.inspect(span)?),
                    span,
                )
                    .into())
            }
        },
        _ => match right {
            Value::String(s, q) => Value::String(
                format!(
                    "{}/{}{}{}",
                    left.to_css_string(span, options.is_compressed())?,
                    q,
                    s,
                    q
                ),
                QuoteKind::None,
            ),
            Value::Null => Value::String(
                format!("{}/", left.to_css_string(span, options.is_compressed())?),
                QuoteKind::None,
            ),
            _ => Value::String(
                format!(
                    "{}/{}",
                    left.to_css_string(span, options.is_compressed())?,
                    right.to_css_string(span, options.is_compressed())?
                ),
                QuoteKind::None,
            ),
        },
    })
}

pub(crate) fn rem(left: Value, right: Value, options: &Options, span: Span) -> SassResult<Value> {
    Ok(match left {
        Value::Dimension(n, ..) if n.is_nan() => todo!(),
        Value::Dimension(n, u, _) => match right {
            Value::Dimension(n, ..) if n.is_nan() => todo!(),
            Value::Dimension(n2, u2, _) => {
                if !u.comparable(&u2) {
                    return Err((format!("Incompatible units {} and {}.", u, u2), span).into());
                }

                // if n2.is_zero() {
                //     // todo: NaN
                //     todo!()
                //     // return Ok(Value::Dimension(
                //     //     None,
                //     //     if u == Unit::None { u2 } else { u },
                //     //     true,
                //     // ));
                // }

                if u == u2 {
                    Value::Dimension(n % n2, u, None)
                } else if u == Unit::None {
                    Value::Dimension(n % n2, u2, None)
                } else if u2 == Unit::None {
                    Value::Dimension(n % n2, u, None)
                } else {
                    Value::Dimension(n, u, None)
                }
            }
            _ => {
                return Err((
                    format!(
                        "Undefined operation \"{} % {}\".",
                        Value::Dimension(n, u, None).inspect(span)?,
                        right.inspect(span)?
                    ),
                    span,
                )
                    .into())
            }
        },
        _ => {
            return Err((
                format!(
                    "Undefined operation \"{} % {}\".",
                    left.inspect(span)?,
                    right.inspect(span)?
                ),
                span,
            )
                .into())
        }
    })
}

// pub(crate) struct ValueVisitor<'a, 'b: 'a, 'c> {
//     parser: &'a mut Parser<'b, 'c>,
//     span: Span,
// }

// impl<'a, 'b: 'a, 'c> ValueVisitor<'a, 'b, 'c> {
// pub fn new(parser: &'a mut Parser<'b, 'c>, span: Span) -> Self {
//     Self { parser, span }
// }

// pub fn eval(&mut self, value: HigherIntermediateValue, in_parens: bool) -> SassResult<Value> {
//     match value {
//         HigherIntermediateValue::Literal(Value::Dimension(n, u, _)) if in_parens => {
//             Ok(Value::Dimension(n, u, true))
//         }
//         HigherIntermediateValue::Literal(v) => Ok(v),
//         HigherIntermediateValue::BinaryOp(v1, op, v2) => self.bin_op(*v1, op, *v2, in_parens),
//         HigherIntermediateValue::UnaryOp(op, val) => self.unary_op(op, *val, in_parens),
//         HigherIntermediateValue::Function(function, args, module) => {
//             self.parser.call_function(function, args, module)
//         }
//     }
// }

// fn bin_op_one_level(
//     &mut self,
//     val1: HigherIntermediateValue,
//     op: Op,
//     val2: HigherIntermediateValue,
//     in_parens: bool,
// ) -> SassResult<HigherIntermediateValue> {
//     let val1 = self.unary(val1, in_parens)?;
//     let val2 = self.unary(val2, in_parens)?;

//     let val1 = match val1 {
//         HigherIntermediateValue::Literal(val1) => val1,
//         HigherIntermediateValue::BinaryOp(val1_1, val1_op, val1_2) => {
//             if val1_op.precedence() >= op.precedence() {
//                 return Ok(HigherIntermediateValue::BinaryOp(
//                     Box::new(self.bin_op_one_level(*val1_1, val1_op, *val1_2, in_parens)?),
//                     op,
//                     Box::new(val2),
//                 ));
//             }

//             return Ok(HigherIntermediateValue::BinaryOp(
//                 val1_1,
//                 val1_op,
//                 Box::new(self.bin_op_one_level(*val1_2, op, val2, in_parens)?),
//             ));
//         }
//         _ => unreachable!(),
//     };

//     let val2 = match val2 {
//         HigherIntermediateValue::Literal(val2) => val2,
//         HigherIntermediateValue::BinaryOp(val2_1, val2_op, val2_2) => {
//             todo!()
//         }
//         _ => unreachable!(),
//     };

//     let val1 = HigherIntermediateValue::Literal(val1);
//     let val2 = HigherIntermediateValue::Literal(val2);

//     Ok(HigherIntermediateValue::Literal(match op {
//         Op::Plus => self.add(val1, val2)?,
//         Op::Minus => self.sub(val1, val2)?,
//         Op::Mul => self.mul(val1, val2)?,
//         Op::Div => self.div(val1, val2, in_parens)?,
//         Op::Rem => self.rem(val1, val2)?,
//         Op::And => Self::and(val1, val2),
//         Op::Or => Self::or(val1, val2),
//         Op::Equal => Self::equal(val1, val2),
//         Op::NotEqual => Self::not_equal(val1, val2),
//         Op::GreaterThan => self.greater_than(val1, val2)?,
//         Op::GreaterThanEqual => self.greater_than_or_equal(val1, val2)?,
//         Op::LessThan => self.less_than(val1, val2)?,
//         Op::LessThanEqual => self.less_than_or_equal(val1, val2)?,
//         Op::Not => unreachable!(),
//     }))
// }

// fn bin_op(
//     &mut self,
//     val1: HigherIntermediateValue,
//     op: Op,
//     val2: HigherIntermediateValue,
//     in_parens: bool,
// ) -> SassResult<Value> {
//     let mut val1 = self.unary(val1, in_parens)?;
//     let mut val2 = self.unary(val2, in_parens)?;

//     if let HigherIntermediateValue::BinaryOp(val1_1, val1_op, val1_2) = val1 {
//         let in_parens = op != Op::Div || val1_op != Op::Div;

//         return if val1_op.precedence() >= op.precedence() {
//             val1 = self.bin_op_one_level(*val1_1, val1_op, *val1_2, in_parens)?;
//             self.bin_op(val1, op, val2, in_parens)
//         } else {
//             val2 = self.bin_op_one_level(*val1_2, op, val2, in_parens)?;
//             self.bin_op(*val1_1, val1_op, val2, in_parens)
//         };
//     }

//     Ok(match op {
//         Op::Plus => self.add(val1, val2)?,
//         Op::Minus => self.sub(val1, val2)?,
//         Op::Mul => self.mul(val1, val2)?,
//         Op::Div => self.div(val1, val2, in_parens)?,
//         Op::Rem => self.rem(val1, val2)?,
//         Op::And => Self::and(val1, val2),
//         Op::Or => Self::or(val1, val2),
//         Op::Equal => Self::equal(val1, val2),
//         Op::NotEqual => Self::not_equal(val1, val2),
//         Op::GreaterThan => self.greater_than(val1, val2)?,
//         Op::GreaterThanEqual => self.greater_than_or_equal(val1, val2)?,
//         Op::LessThan => self.less_than(val1, val2)?,
//         Op::LessThanEqual => self.less_than_or_equal(val1, val2)?,
//         Op::Not => unreachable!(),
//     })
// }

// fn unary_op(
//     &mut self,
//     op: Op,
//     val: HigherIntermediateValue,
//     in_parens: bool,
// ) -> SassResult<Value> {
//     let val = self.eval(val, in_parens)?;
//     match op {
//         Op::Minus => self.unary_minus(val),
//         Op::Not => Ok(Self::unary_not(&val)),
//         Op::Plus => self.unary_plus(val),
//         _ => unreachable!(),
//     }
// }

// fn unary_minus(&self, val: Value) -> SassResult<Value> {
//     Ok(match val {
//         Value::Dimension(n, u, should_divide) => Value::Dimension(-n, u, should_divide),
//         v => Value::String(
//             format!(
//                 "-{}",
//                 v.to_css_string(self.span, self.parser.options.is_compressed())?
//             ),
//             QuoteKind::None,
//         ),
//     })
// }

// fn unary_plus(&self, val: Value) -> SassResult<Value> {
//     Ok(match val {
//         v @ Value::Dimension(..) => v,
//         v => Value::String(
//             format!(
//                 "+{}",
//                 v.to_css_string(self.span, self.parser.options.is_compressed())?
//             ),
//             QuoteKind::None,
//         ),
//     })
// }

// fn unary_not(val: &Value) -> Value {
//     Value::bool(!val.is_true())
// }

// fn unary(
//     &mut self,
//     val: HigherIntermediateValue,
//     in_parens: bool,
// ) -> SassResult<HigherIntermediateValue> {
//     Ok(match val {
//         HigherIntermediateValue::UnaryOp(op, val) => {
//             HigherIntermediateValue::Literal(self.unary_op(op, *val, in_parens)?)
//         }
//         HigherIntermediateValue::Function(function, args, module) => {
//             HigherIntermediateValue::Literal(self.parser.call_function(function, args, module)?)
//         }
//         _ => val,
//     })
// }

// fn add(
//     &self,
//     left: HigherIntermediateValue,
//     right: HigherIntermediateValue,
// ) -> SassResult<Value> {
//     let left = match left {
//         HigherIntermediateValue::Literal(v) => v,
//         v => panic!("{:?}", v),
//     };
//     let right = match right {
//         HigherIntermediateValue::Literal(v) => v,
//         v => panic!("{:?}", v),
//     };
//     // Ok(match left {
//     //     Value::Map(..) | Value::FunctionRef(..) => {
//     //         return Err((
//     //             format!("{} isn't a valid CSS value.", left.inspect(self.span)?),
//     //             self.span,
//     //         )
//     //             .into())
//     //     }
//     //     Value::True | Value::False => match right {
//     //         Value::String(s, QuoteKind::Quoted) => Value::String(
//     //             format!(
//     //                 "{}{}",
//     //                 left.to_css_string(self.span, self.parser.options.is_compressed())?,
//     //                 s
//     //             ),
//     //             QuoteKind::Quoted,
//     //         ),
//     //         _ => Value::String(
//     //             format!(
//     //                 "{}{}",
//     //                 left.to_css_string(self.span, self.parser.options.is_compressed())?,
//     //                 right.to_css_string(self.span, self.parser.options.is_compressed())?
//     //             ),
//     //             QuoteKind::None,
//     //         ),
//     //     },
//     //     Value::Important => match right {
//     //         Value::String(s, ..) => Value::String(
//     //             format!(
//     //                 "{}{}",
//     //                 left.to_css_string(self.span, self.parser.options.is_compressed())?,
//     //                 s
//     //             ),
//     //             QuoteKind::None,
//     //         ),
//     //         _ => Value::String(
//     //             format!(
//     //                 "{}{}",
//     //                 left.to_css_string(self.span, self.parser.options.is_compressed())?,
//     //                 right.to_css_string(self.span, self.parser.options.is_compressed())?
//     //             ),
//     //             QuoteKind::None,
//     //         ),
//     //     },
//     //     Value::Null => match right {
//     //         Value::Null => Value::Null,
//     //         _ => Value::String(
//     //             right
//     //                 .to_css_string(self.span, self.parser.options.is_compressed())?
//     //                 .into_owned(),
//     //             QuoteKind::None,
//     //         ),
//     //     },
//     //     v @ Value::Dimension(None, ..) => v,
//     //     Value::Dimension(Some(num), unit, _) => match right {
//     //         v @ Value::Dimension(None, ..) => v,
//     //         Value::Dimension(Some(num2), unit2, _) => {
//     //             if !unit.comparable(&unit2) {
//     //                 return Err((
//     //                     format!("Incompatible units {} and {}.", unit2, unit),
//     //                     self.span,
//     //                 )
//     //                     .into());
//     //             }
//     //             if unit == unit2 {
//     //                 Value::Dimension(Some(num + num2), unit, true)
//     //             } else if unit == Unit::None {
//     //                 Value::Dimension(Some(num + num2), unit2, true)
//     //             } else if unit2 == Unit::None {
//     //                 Value::Dimension(Some(num + num2), unit, true)
//     //             } else {
//     //                 Value::Dimension(Some(num + num2.convert(&unit2, &unit)), unit, true)
//     //             }
//     //         }
//     //         Value::String(s, q) => Value::String(
//     //             format!(
//     //                 "{}{}{}",
//     //                 num.to_string(self.parser.options.is_compressed()),
//     //                 unit,
//     //                 s
//     //             ),
//     //             q,
//     //         ),
//     //         Value::Null => Value::String(
//     //             format!(
//     //                 "{}{}",
//     //                 num.to_string(self.parser.options.is_compressed()),
//     //                 unit
//     //             ),
//     //             QuoteKind::None,
//     //         ),
//     //         Value::True
//     //         | Value::False
//     //         | Value::List(..)
//     //         | Value::Important
//     //         | Value::ArgList(..) => Value::String(
//     //             format!(
//     //                 "{}{}{}",
//     //                 num.to_string(self.parser.options.is_compressed()),
//     //                 unit,
//     //                 right.to_css_string(self.span, self.parser.options.is_compressed())?
//     //             ),
//     //             QuoteKind::None,
//     //         ),
//     //         Value::Map(..) | Value::FunctionRef(..) => {
//     //             return Err((
//     //                 format!("{} isn't a valid CSS value.", right.inspect(self.span)?),
//     //                 self.span,
//     //             )
//     //                 .into())
//     //         }
//     //         Value::Color(..) => {
//     //             return Err((
//     //                 format!(
//     //                     "Undefined operation \"{}{} + {}\".",
//     //                     num.inspect(),
//     //                     unit,
//     //                     right.inspect(self.span)?
//     //                 ),
//     //                 self.span,
//     //             )
//     //                 .into())
//     //         }
//     //     },
//     //     Value::Color(c) => match right {
//     //         Value::String(s, q) => Value::String(format!("{}{}", c, s), q),
//     //         Value::Null => Value::String(c.to_string(), QuoteKind::None),
//     //         Value::List(..) => Value::String(
//     //             format!(
//     //                 "{}{}",
//     //                 c,
//     //                 right.to_css_string(self.span, self.parser.options.is_compressed())?
//     //             ),
//     //             QuoteKind::None,
//     //         ),
//     //         _ => {
//     //             return Err((
//     //                 format!(
//     //                     "Undefined operation \"{} + {}\".",
//     //                     c,
//     //                     right.inspect(self.span)?
//     //                 ),
//     //                 self.span,
//     //             )
//     //                 .into())
//     //         }
//     //     },
//     //     Value::String(text, quotes) => match right {
//     //         Value::String(text2, ..) => Value::String(text + &text2, quotes),
//     //         _ => Value::String(
//     //             text + &right.to_css_string(self.span, self.parser.options.is_compressed())?,
//     //             quotes,
//     //         ),
//     //     },
//     //     Value::List(..) | Value::ArgList(..) => match right {
//     //         Value::String(s, q) => Value::String(
//     //             format!(
//     //                 "{}{}",
//     //                 left.to_css_string(self.span, self.parser.options.is_compressed())?,
//     //                 s
//     //             ),
//     //             q,
//     //         ),
//     //         _ => Value::String(
//     //             format!(
//     //                 "{}{}",
//     //                 left.to_css_string(self.span, self.parser.options.is_compressed())?,
//     //                 right.to_css_string(self.span, self.parser.options.is_compressed())?
//     //             ),
//     //             QuoteKind::None,
//     //         ),
//     //     },
//     // })
//     todo!()
// }

// fn sub(
//     &self,
//     left: HigherIntermediateValue,
//     right: HigherIntermediateValue,
// ) -> SassResult<Value> {
//     let left = match left {
//         HigherIntermediateValue::Literal(v) => v,
//         v => panic!("{:?}", v),
//     };
//     let right = match right {
//         HigherIntermediateValue::Literal(v) => v,
//         v => panic!("{:?}", v),
//     };
//     // Ok(match left {
//     //     Value::Null => Value::String(
//     //         format!(
//     //             "-{}",
//     //             right.to_css_string(self.span, self.parser.options.is_compressed())?
//     //         ),
//     //         QuoteKind::None,
//     //     ),
//     //     v @ Value::Dimension(None, ..) => v,
//     //     Value::Dimension(Some(num), unit, _) => match right {
//     //         v @ Value::Dimension(None, ..) => v,
//     //         Value::Dimension(Some(num2), unit2, _) => {
//     //             if !unit.comparable(&unit2) {
//     //                 return Err((
//     //                     format!("Incompatible units {} and {}.", unit2, unit),
//     //                     self.span,
//     //                 )
//     //                     .into());
//     //             }
//     //             if unit == unit2 {
//     //                 Value::Dimension(Some(num - num2), unit, true)
//     //             } else if unit == Unit::None {
//     //                 Value::Dimension(Some(num - num2), unit2, true)
//     //             } else if unit2 == Unit::None {
//     //                 Value::Dimension(Some(num - num2), unit, true)
//     //             } else {
//     //                 Value::Dimension(Some(num - num2.convert(&unit2, &unit)), unit, true)
//     //             }
//     //         }
//     //         Value::List(..)
//     //         | Value::String(..)
//     //         | Value::Important
//     //         | Value::True
//     //         | Value::False
//     //         | Value::ArgList(..) => Value::String(
//     //             format!(
//     //                 "{}{}-{}",
//     //                 num.to_string(self.parser.options.is_compressed()),
//     //                 unit,
//     //                 right.to_css_string(self.span, self.parser.options.is_compressed())?
//     //             ),
//     //             QuoteKind::None,
//     //         ),
//     //         Value::Map(..) | Value::FunctionRef(..) => {
//     //             return Err((
//     //                 format!("{} isn't a valid CSS value.", right.inspect(self.span)?),
//     //                 self.span,
//     //             )
//     //                 .into())
//     //         }
//     //         Value::Color(..) => {
//     //             return Err((
//     //                 format!(
//     //                     "Undefined operation \"{}{} - {}\".",
//     //                     num.inspect(),
//     //                     unit,
//     //                     right.inspect(self.span)?
//     //                 ),
//     //                 self.span,
//     //             )
//     //                 .into())
//     //         }
//     //         Value::Null => Value::String(
//     //             format!(
//     //                 "{}{}-",
//     //                 num.to_string(self.parser.options.is_compressed()),
//     //                 unit
//     //             ),
//     //             QuoteKind::None,
//     //         ),
//     //     },
//     //     Value::Color(c) => match right {
//     //         Value::String(s, q) => {
//     //             Value::String(format!("{}-{}{}{}", c, q, s, q), QuoteKind::None)
//     //         }
//     //         Value::Null => Value::String(format!("{}-", c), QuoteKind::None),
//     //         Value::Dimension(..) | Value::Color(..) => {
//     //             return Err((
//     //                 format!(
//     //                     "Undefined operation \"{} - {}\".",
//     //                     c,
//     //                     right.inspect(self.span)?
//     //                 ),
//     //                 self.span,
//     //             )
//     //                 .into())
//     //         }
//     //         _ => Value::String(
//     //             format!(
//     //                 "{}-{}",
//     //                 c,
//     //                 right.to_css_string(self.span, self.parser.options.is_compressed())?
//     //             ),
//     //             QuoteKind::None,
//     //         ),
//     //     },
//     //     Value::String(..) => Value::String(
//     //         format!(
//     //             "{}-{}",
//     //             left.to_css_string(self.span, self.parser.options.is_compressed())?,
//     //             right.to_css_string(self.span, self.parser.options.is_compressed())?
//     //         ),
//     //         QuoteKind::None,
//     //     ),
//     //     _ => match right {
//     //         Value::String(s, q) => Value::String(
//     //             format!(
//     //                 "{}-{}{}{}",
//     //                 left.to_css_string(self.span, self.parser.options.is_compressed())?,
//     //                 q,
//     //                 s,
//     //                 q
//     //             ),
//     //             QuoteKind::None,
//     //         ),
//     //         Value::Null => Value::String(
//     //             format!(
//     //                 "{}-",
//     //                 left.to_css_string(self.span, self.parser.options.is_compressed())?
//     //             ),
//     //             QuoteKind::None,
//     //         ),
//     //         _ => Value::String(
//     //             format!(
//     //                 "{}-{}",
//     //                 left.to_css_string(self.span, self.parser.options.is_compressed())?,
//     //                 right.to_css_string(self.span, self.parser.options.is_compressed())?
//     //             ),
//     //             QuoteKind::None,
//     //         ),
//     //     },
//     // })
//     todo!()
// }

// fn mul(
//     &self,
//     left: HigherIntermediateValue,
//     right: HigherIntermediateValue,
// ) -> SassResult<Value> {
//     let left = match left {
//         HigherIntermediateValue::Literal(v) => v,
//         v => panic!("{:?}", v),
//     };
//     let right = match right {
//         HigherIntermediateValue::Literal(v) => v,
//         v => panic!("{:?}", v),
//     };
//     Ok(match left {
//         Value::Dimension(None, ..) => todo!(),
//         Value::Dimension(Some(num), unit, _) => match right {
//             Value::Dimension(None, ..) => todo!(),
//             Value::Dimension(Some(num2), unit2, _) => {
//                 if unit == Unit::None {
//                     Value::Dimension(Some(num * num2), unit2, true)
//                 } else if unit2 == Unit::None {
//                     Value::Dimension(Some(num * num2), unit, true)
//                 } else {
//                     Value::Dimension(Some(num * num2), unit * unit2, true)
//                 }
//             }
//             _ => {
//                 return Err((
//                     format!(
//                         "Undefined operation \"{}{} * {}\".",
//                         num.inspect(),
//                         unit,
//                         right.inspect(self.span)?
//                     ),
//                     self.span,
//                 )
//                     .into())
//             }
//         },
//         _ => {
//             return Err((
//                 format!(
//                     "Undefined operation \"{} * {}\".",
//                     left.inspect(self.span)?,
//                     right.inspect(self.span)?
//                 ),
//                 self.span,
//             )
//                 .into())
//         }
//     })
// }

// fn div(
//     &self,
//     left: HigherIntermediateValue,
//     right: HigherIntermediateValue,
//     in_parens: bool,
// ) -> SassResult<Value> {
//     let left = match left {
//         HigherIntermediateValue::Literal(v) => v,
//         v => panic!("{:?}", v),
//     };
//     let right = match right {
//         HigherIntermediateValue::Literal(v) => v,
//         v => panic!("{:?}", v),
//     };
//     todo!()
//     // Ok(match left {
//     //     Value::Null => Value::String(
//     //         format!(
//     //             "/{}",
//     //             right.to_css_string(self.span, self.parser.options.is_compressed())?
//     //         ),
//     //         QuoteKind::None,
//     //     ),
//     //     Value::Dimension(None, ..) => todo!(),
//     //     Value::Dimension(Some(num), unit, should_divide1) => match right {
//     //         Value::Dimension(None, ..) => todo!(),
//     //         Value::Dimension(Some(num2), unit2, should_divide2) => {
//     //             if should_divide1 || should_divide2 || in_parens {
//     //                 if num.is_zero() && num2.is_zero() {
//     //                     return Ok(Value::Dimension(None, Unit::None, true));
//     //                 }

//     //                 if num2.is_zero() {
//     //                     // todo: Infinity and -Infinity
//     //                     return Err(("Infinity not yet implemented.", self.span).into());
//     //                 }

//     //                 // `unit(1em / 1em)` => `""`
//     //                 if unit == unit2 {
//     //                     Value::Dimension(Some(num / num2), Unit::None, true)

//     //                 // `unit(1 / 1em)` => `"em^-1"`
//     //                 } else if unit == Unit::None {
//     //                     Value::Dimension(Some(num / num2), Unit::None / unit2, true)

//     //                 // `unit(1em / 1)` => `"em"`
//     //                 } else if unit2 == Unit::None {
//     //                     Value::Dimension(Some(num / num2), unit, true)

//     //                 // `unit(1in / 1px)` => `""`
//     //                 } else if unit.comparable(&unit2) {
//     //                     Value::Dimension(
//     //                         Some(num / num2.convert(&unit2, &unit)),
//     //                         Unit::None,
//     //                         true,
//     //                     )
//     //                 // `unit(1em / 1px)` => `"em/px"`
//     //                 // todo: this should probably be its own variant
//     //                 // within the `Value` enum
//     //                 } else {
//     //                     // todo: remember to account for `Mul` and `Div`
//     //                     // todo!("non-comparable inverse units")
//     //                     return Err((
//     //                         "Division of non-comparable units not yet supported.",
//     //                         self.span,
//     //                     )
//     //                         .into());
//     //                 }
//     //             } else {
//     //                 Value::String(
//     //                     format!(
//     //                         "{}{}/{}{}",
//     //                         num.to_string(self.parser.options.is_compressed()),
//     //                         unit,
//     //                         num2.to_string(self.parser.options.is_compressed()),
//     //                         unit2
//     //                     ),
//     //                     QuoteKind::None,
//     //                 )
//     //             }
//     //         }
//     //         Value::String(s, q) => Value::String(
//     //             format!(
//     //                 "{}{}/{}{}{}",
//     //                 num.to_string(self.parser.options.is_compressed()),
//     //                 unit,
//     //                 q,
//     //                 s,
//     //                 q
//     //             ),
//     //             QuoteKind::None,
//     //         ),
//     //         Value::List(..)
//     //         | Value::True
//     //         | Value::False
//     //         | Value::Important
//     //         | Value::Color(..)
//     //         | Value::ArgList(..) => Value::String(
//     //             format!(
//     //                 "{}{}/{}",
//     //                 num.to_string(self.parser.options.is_compressed()),
//     //                 unit,
//     //                 right.to_css_string(self.span, self.parser.options.is_compressed())?
//     //             ),
//     //             QuoteKind::None,
//     //         ),
//     //         Value::Null => Value::String(
//     //             format!(
//     //                 "{}{}/",
//     //                 num.to_string(self.parser.options.is_compressed()),
//     //                 unit
//     //             ),
//     //             QuoteKind::None,
//     //         ),
//     //         Value::Map(..) | Value::FunctionRef(..) => {
//     //             return Err((
//     //                 format!("{} isn't a valid CSS value.", right.inspect(self.span)?),
//     //                 self.span,
//     //             )
//     //                 .into())
//     //         }
//     //     },
//     //     Value::Color(c) => match right {
//     //         Value::String(s, q) => {
//     //             Value::String(format!("{}/{}{}{}", c, q, s, q), QuoteKind::None)
//     //         }
//     //         Value::Null => Value::String(format!("{}/", c), QuoteKind::None),
//     //         Value::Dimension(..) | Value::Color(..) => {
//     //             return Err((
//     //                 format!(
//     //                     "Undefined operation \"{} / {}\".",
//     //                     c,
//     //                     right.inspect(self.span)?
//     //                 ),
//     //                 self.span,
//     //             )
//     //                 .into())
//     //         }
//     //         _ => Value::String(
//     //             format!(
//     //                 "{}/{}",
//     //                 c,
//     //                 right.to_css_string(self.span, self.parser.options.is_compressed())?
//     //             ),
//     //             QuoteKind::None,
//     //         ),
//     //     },
//     //     Value::String(s1, q1) => match right {
//     //         Value::String(s2, q2) => Value::String(
//     //             format!("{}{}{}/{}{}{}", q1, s1, q1, q2, s2, q2),
//     //             QuoteKind::None,
//     //         ),
//     //         Value::Important
//     //         | Value::True
//     //         | Value::False
//     //         | Value::Dimension(..)
//     //         | Value::Color(..)
//     //         | Value::List(..)
//     //         | Value::ArgList(..) => Value::String(
//     //             format!(
//     //                 "{}{}{}/{}",
//     //                 q1,
//     //                 s1,
//     //                 q1,
//     //                 right.to_css_string(self.span, self.parser.options.is_compressed())?
//     //             ),
//     //             QuoteKind::None,
//     //         ),
//     //         Value::Null => Value::String(format!("{}{}{}/", q1, s1, q1), QuoteKind::None),
//     //         Value::Map(..) | Value::FunctionRef(..) => {
//     //             return Err((
//     //                 format!("{} isn't a valid CSS value.", right.inspect(self.span)?),
//     //                 self.span,
//     //             )
//     //                 .into())
//     //         }
//     //     },
//     //     _ => match right {
//     //         Value::String(s, q) => Value::String(
//     //             format!(
//     //                 "{}/{}{}{}",
//     //                 left.to_css_string(self.span, self.parser.options.is_compressed())?,
//     //                 q,
//     //                 s,
//     //                 q
//     //             ),
//     //             QuoteKind::None,
//     //         ),
//     //         Value::Null => Value::String(
//     //             format!(
//     //                 "{}/",
//     //                 left.to_css_string(self.span, self.parser.options.is_compressed())?
//     //             ),
//     //             QuoteKind::None,
//     //         ),
//     //         _ => Value::String(
//     //             format!(
//     //                 "{}/{}",
//     //                 left.to_css_string(self.span, self.parser.options.is_compressed())?,
//     //                 right.to_css_string(self.span, self.parser.options.is_compressed())?
//     //             ),
//     //             QuoteKind::None,
//     //         ),
//     //     },
//     // })
// }

// fn rem(
//     &self,
//     left: HigherIntermediateValue,
//     right: HigherIntermediateValue,
// ) -> SassResult<Value> {
//     let left = match left {
//         HigherIntermediateValue::Literal(v) => v,
//         v => panic!("{:?}", v),
//     };
//     let right = match right {
//         HigherIntermediateValue::Literal(v) => v,
//         v => panic!("{:?}", v),
//     };
//     Ok(match left {
//         v @ Value::Dimension(None, ..) => v,
//         Value::Dimension(Some(n), u, _) => match right {
//             v @ Value::Dimension(None, ..) => v,
//             Value::Dimension(Some(n2), u2, _) => {
//                 if !u.comparable(&u2) {
//                     return Err(
//                         (format!("Incompatible units {} and {}.", u, u2), self.span).into()
//                     );
//                 }

//                 if n2.is_zero() {
//                     return Ok(Value::Dimension(
//                         None,
//                         if u == Unit::None { u2 } else { u },
//                         true,
//                     ));
//                 }

//                 if u == u2 {
//                     Value::Dimension(Some(n % n2), u, true)
//                 } else if u == Unit::None {
//                     Value::Dimension(Some(n % n2), u2, true)
//                 } else if u2 == Unit::None {
//                     Value::Dimension(Some(n % n2), u, true)
//                 } else {
//                     Value::Dimension(Some(n), u, true)
//                 }
//             }
//             _ => {
//                 return Err((
//                     format!(
//                         "Undefined operation \"{} % {}\".",
//                         Value::Dimension(Some(n), u, true).inspect(self.span)?,
//                         right.inspect(self.span)?
//                     ),
//                     self.span,
//                 )
//                     .into())
//             }
//         },
//         _ => {
//             return Err((
//                 format!(
//                     "Undefined operation \"{} % {}\".",
//                     left.inspect(self.span)?,
//                     right.inspect(self.span)?
//                 ),
//                 self.span,
//             )
//                 .into())
//         }
//     })
// }

// fn and(left: HigherIntermediateValue, right: HigherIntermediateValue) -> Value {
//     let left = match left {
//         HigherIntermediateValue::Literal(v) => v,
//         v => panic!("{:?}", v),
//     };
//     let right = match right {
//         HigherIntermediateValue::Literal(v) => v,
//         v => panic!("{:?}", v),
//     };
//     if left.is_true() {
//         right
//     } else {
//         left
//     }
// }

// fn or(left: HigherIntermediateValue, right: HigherIntermediateValue) -> Value {
//     let left = match left {
//         HigherIntermediateValue::Literal(v) => v,
//         v => panic!("{:?}", v),
//     };
//     let right = match right {
//         HigherIntermediateValue::Literal(v) => v,
//         v => panic!("{:?}", v),
//     };
//     if left.is_true() {
//         left
//     } else {
//         right
//     }
// }

// pub fn equal(left: HigherIntermediateValue, right: HigherIntermediateValue) -> Value {
//     let left = match left {
//         HigherIntermediateValue::Literal(v) => v,
//         v => panic!("{:?}", v),
//     };
//     let right = match right {
//         HigherIntermediateValue::Literal(v) => v,
//         v => panic!("{:?}", v),
//     };
//     Value::bool(left == right)
// }

// fn not_equal(left: HigherIntermediateValue, right: HigherIntermediateValue) -> Value {
//     let left = match left {
//         HigherIntermediateValue::Literal(v) => v,
//         v => panic!("{:?}", v),
//     };
//     let right = match right {
//         HigherIntermediateValue::Literal(v) => v,
//         v => panic!("{:?}", v),
//     };
//     Value::bool(left.not_equals(&right))
// }

// fn cmp(
//     &self,
//     left: HigherIntermediateValue,
//     op: Op,
//     right: HigherIntermediateValue,
// ) -> SassResult<Value> {
//     todo!()
//     // let left = match left {
//     //     HigherIntermediateValue::Literal(v) => v,
//     //     v => panic!("{:?}", v),
//     // };
//     // let right = match right {
//     //     HigherIntermediateValue::Literal(v) => v,
//     //     v => panic!("{:?}", v),
//     // };

//     // let ordering = left.cmp(&right, self.span, op)?;

//     // Ok(match op {
//     //     Op::GreaterThan => match ordering {
//     //         Ordering::Greater => Value::True,
//     //         Ordering::Less | Ordering::Equal => Value::False,
//     //     },
//     //     Op::GreaterThanEqual => match ordering {
//     //         Ordering::Greater | Ordering::Equal => Value::True,
//     //         Ordering::Less => Value::False,
//     //     },
//     //     Op::LessThan => match ordering {
//     //         Ordering::Less => Value::True,
//     //         Ordering::Greater | Ordering::Equal => Value::False,
//     //     },
//     //     Op::LessThanEqual => match ordering {
//     //         Ordering::Less | Ordering::Equal => Value::True,
//     //         Ordering::Greater => Value::False,
//     //     },
//     //     _ => unreachable!(),
//     // })
// }

// pub fn greater_than(
//     &self,
//     left: HigherIntermediateValue,
//     right: HigherIntermediateValue,
// ) -> SassResult<Value> {
//     self.cmp(left, Op::GreaterThan, right)
// }

// fn greater_than_or_equal(
//     &self,
//     left: HigherIntermediateValue,
//     right: HigherIntermediateValue,
// ) -> SassResult<Value> {
//     self.cmp(left, Op::GreaterThanEqual, right)
// }

// pub fn less_than(
//     &self,
//     left: HigherIntermediateValue,
//     right: HigherIntermediateValue,
// ) -> SassResult<Value> {
//     self.cmp(left, Op::LessThan, right)
// }

// fn less_than_or_equal(
//     &self,
//     left: HigherIntermediateValue,
//     right: HigherIntermediateValue,
// ) -> SassResult<Value> {
//     self.cmp(left, Op::LessThanEqual, right)
// }
// }
