#![allow(unused_variables)]

use std::cmp::Ordering;

use codemap::Span;

use crate::{
    common::{BinaryOp, QuoteKind},
    error::SassResult,
    serializer::serialize_number,
    unit::Unit,
    value::{SassNumber, Value},
    Options,
};

pub(crate) fn add(left: Value, right: Value, options: &Options, span: Span) -> SassResult<Value> {
    Ok(match left {
        Value::Calculation(..) => match right {
            Value::String(s, quotes) => Value::String(
                format!(
                    "{}{}",
                    left.to_css_string(span, options.is_compressed())?,
                    s
                ),
                quotes,
            ),
            _ => {
                return Err((
                    format!(
                        "Undefined operation \"{} + {}\".",
                        left.inspect(span)?,
                        right.inspect(span)?
                    ),
                    span,
                )
                    .into())
            }
        },
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
        Value::Dimension { num: n, .. } if n.is_nan() => todo!(),
        Value::Dimension {
            num,
            unit,
            as_slash: _,
        } => match right {
            Value::Dimension { num: n, .. } if n.is_nan() => todo!(),
            Value::Dimension {
                num: num2,
                unit: unit2,
                as_slash: _,
            } => {
                if !unit.comparable(&unit2) {
                    return Err(
                        (format!("Incompatible units {} and {}.", unit2, unit), span).into(),
                    );
                }
                if unit == unit2 {
                    Value::Dimension {
                        num: num + num2,
                        unit,
                        as_slash: None,
                    }
                } else if unit == Unit::None {
                    Value::Dimension {
                        num: num + num2,
                        unit: unit2,
                        as_slash: None,
                    }
                } else if unit2 == Unit::None {
                    Value::Dimension {
                        num: num + num2,
                        unit,
                        as_slash: None,
                    }
                } else {
                    Value::Dimension {
                        num: num + num2.convert(&unit2, &unit),
                        unit,
                        as_slash: None,
                    }
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
            Value::Color(..) | Value::Calculation(..) => {
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
        c @ Value::Color(..) => match right {
            // todo: we really cant add to any other types?
            Value::String(..) | Value::Null | Value::List(..) => Value::String(
                format!(
                    "{}{}",
                    c.to_css_string(span, options.is_compressed())?,
                    right.to_css_string(span, options.is_compressed())?,
                ),
                QuoteKind::None,
            ),
            _ => {
                return Err((
                    format!(
                        "Undefined operation \"{} + {}\".",
                        c.inspect(span)?,
                        right.inspect(span)?
                    ),
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
        Value::Calculation(..) => {
            return Err((
                format!(
                    "Undefined operation \"{} - {}\".",
                    left.inspect(span)?,
                    right.inspect(span)?
                ),
                span,
            )
                .into())
        }
        Value::Null => Value::String(
            format!("-{}", right.to_css_string(span, options.is_compressed())?),
            QuoteKind::None,
        ),
        Value::Dimension {
            num,
            unit,
            as_slash: _,
        } => match right {
            Value::Dimension {
                num: num2,
                unit: unit2,
                as_slash: _,
            } => {
                if !unit.comparable(&unit2) {
                    return Err(
                        (format!("Incompatible units {} and {}.", unit2, unit), span).into(),
                    );
                }
                if unit == unit2 {
                    Value::Dimension {
                        num: num - num2,
                        unit,
                        as_slash: None,
                    }
                } else if unit == Unit::None {
                    Value::Dimension {
                        num: num - num2,
                        unit: unit2,
                        as_slash: None,
                    }
                } else if unit2 == Unit::None {
                    Value::Dimension {
                        num: num - num2,
                        unit,
                        as_slash: None,
                    }
                } else {
                    Value::Dimension {
                        num: num - num2.convert(&unit2, &unit),
                        unit,
                        as_slash: None,
                    }
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
            Value::Color(..) | Value::Calculation(..) => {
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
        c @ Value::Color(..) => match right {
            Value::Dimension { .. } | Value::Color(..) => {
                return Err((
                    format!(
                        "Undefined operation \"{} - {}\".",
                        c.inspect(span)?,
                        right.inspect(span)?
                    ),
                    span,
                )
                    .into())
            }
            _ => Value::String(
                format!(
                    "{}-{}",
                    c.to_css_string(span, options.is_compressed())?,
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
        Value::Dimension { num: n, .. } if n.is_nan() => todo!(),
        Value::Dimension {
            num,
            unit,
            as_slash: _,
        } => match right {
            Value::Dimension { num: n, .. } if n.is_nan() => todo!(),
            Value::Dimension {
                num: num2,
                unit: unit2,
                as_slash: _,
            } => {
                if unit == Unit::None {
                    Value::Dimension {
                        num: num * num2,
                        unit: unit2,
                        as_slash: None,
                    }
                } else if unit2 == Unit::None {
                    Value::Dimension {
                        num: num * num2,
                        unit,
                        as_slash: None,
                    }
                } else {
                    Value::Dimension {
                        num: num * num2,
                        unit: unit * unit2,
                        as_slash: None,
                    }
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

// todo: simplify matching
pub(crate) fn div(left: Value, right: Value, options: &Options, span: Span) -> SassResult<Value> {
    Ok(match left {
        Value::Dimension {
            num,
            unit,
            as_slash: as_slash1,
        } => match right {
            Value::Dimension {
                num: num2,
                unit: unit2,
                as_slash: as_slash2,
            } => {
                // `unit(1em / 1em)` => `""`
                if unit == unit2 {
                    Value::Dimension {
                        num: num / num2,
                        unit: Unit::None,
                        as_slash: None,
                    }

                // `unit(1 / 1em)` => `"em^-1"`
                } else if unit == Unit::None {
                    Value::Dimension {
                        num: num / num2,
                        unit: Unit::None / unit2,
                        as_slash: None,
                    }

                // `unit(1em / 1)` => `"em"`
                } else if unit2 == Unit::None {
                    Value::Dimension {
                        num: num / num2,
                        unit,
                        as_slash: None,
                    }

                // `unit(1in / 1px)` => `""`
                } else if unit.comparable(&unit2) {
                    Value::Dimension {
                        num: num / num2.convert(&unit2, &unit),
                        unit: Unit::None,
                        as_slash: None,
                    }
                // `unit(1em / 1px)` => `"em/px"`
                } else {
                    Value::Dimension {
                        num: num / num2,
                        unit: unit / unit2,
                        as_slash: None,
                    }
                }
            }
            Value::List(..)
            | Value::True
            | Value::False
            | Value::Color(..)
            | Value::ArgList(..)
            | Value::Null
            | Value::String(..)
            | Value::Calculation(..) => Value::String(
                format!(
                    "{}/{}",
                    serialize_number(
                        &SassNumber {
                            num: num.0,
                            unit,
                            as_slash: as_slash1
                        },
                        options,
                        span
                    )?,
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
        },
        c @ Value::Color(..) => match right {
            Value::Dimension { .. } | Value::Color(..) => {
                return Err((
                    format!(
                        "Undefined operation \"{} / {}\".",
                        c.inspect(span)?,
                        right.inspect(span)?
                    ),
                    span,
                )
                    .into())
            }
            _ => Value::String(
                format!(
                    "{}/{}",
                    c.to_css_string(span, options.is_compressed())?,
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
            | Value::Dimension { .. }
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
        _ => Value::String(
            format!(
                "{}/{}",
                left.to_css_string(span, options.is_compressed())?,
                right.to_css_string(span, options.is_compressed())?
            ),
            QuoteKind::None,
        ),
    })
}

pub(crate) fn rem(left: Value, right: Value, options: &Options, span: Span) -> SassResult<Value> {
    Ok(match left {
        Value::Dimension { num: n, .. } if n.is_nan() => todo!(),
        Value::Dimension {
            num: n,
            unit: u,
            as_slash: _,
        } => match right {
            Value::Dimension { num: n, .. } if n.is_nan() => todo!(),
            Value::Dimension {
                num: n2,
                unit: u2,
                as_slash: _,
            } => {
                if !u.comparable(&u2) {
                    return Err((format!("Incompatible units {} and {}.", u, u2), span).into());
                }

                let new_num = n % n2.convert(&u2, &u);
                let new_unit = if u == u2 {
                    u
                } else if u == Unit::None {
                    u2
                } else if u2 == Unit::None {
                    u
                } else {
                    u
                };
                Value::Dimension {
                    num: new_num,
                    unit: new_unit,
                    as_slash: None,
                }
            }
            _ => {
                return Err((
                    format!(
                        "Undefined operation \"{} % {}\".",
                        Value::Dimension {
                            num: n,
                            unit: u,
                            as_slash: None
                        }
                        .inspect(span)?,
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
