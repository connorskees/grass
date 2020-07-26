use super::{Builtin, GlobalFunctionMap};

use crate::{
    args::CallArgs, common::QuoteKind, error::SassResult, parse::Parser, unit::Unit, value::Number,
    value::Value,
};

pub(crate) fn alpha(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    match args.get_err(0, "color")? {
        Value::Color(c) => Ok(Value::Dimension(Some(c.alpha()), Unit::None, true)),
        Value::Dimension(None, ..) => todo!(),
        v => Err((
            format!("$color: {} is not a color.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn opacity(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    match args.get_err(0, "color")? {
        Value::Color(c) => Ok(Value::Dimension(Some(c.alpha()), Unit::None, true)),
        Value::Dimension(Some(num), unit, _) => Ok(Value::String(
            format!("opacity({}{})", num, unit),
            QuoteKind::None,
        )),
        Value::Dimension(None, ..) => todo!(),
        v => Err((
            format!("$color: {} is not a color.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

// todo: unify `opacify` and `fade_in`
fn opacify(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(2)?;
    let color = match args.get_err(0, "color")? {
        Value::Color(c) => c,
        v => {
            return Err((
                format!("$color: {} is not a color.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    let amount = match args.get_err(1, "amount")? {
        Value::Dimension(Some(n), u, _) => bound!(args, "amount", n, u, 0, 1),
        Value::Dimension(None, ..) => todo!(),
        v => {
            return Err((
                format!("$amount: {} is not a number.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::Color(Box::new(color.fade_in(amount))))
}

fn fade_in(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(2)?;
    let color = match args.get_err(0, "color")? {
        Value::Color(c) => c,
        v => {
            return Err((
                format!("$color: {} is not a color.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    let amount = match args.get_err(1, "amount")? {
        Value::Dimension(Some(n), u, _) => bound!(args, "amount", n, u, 0, 1),
        Value::Dimension(None, ..) => todo!(),
        v => {
            return Err((
                format!("$amount: {} is not a number.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::Color(Box::new(color.fade_in(amount))))
}

// todo: unify with `fade_out`
fn transparentize(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(2)?;
    let color = match args.get_err(0, "color")? {
        Value::Color(c) => c,
        v => {
            return Err((
                format!("$color: {} is not a color.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    let amount = match args.get_err(1, "amount")? {
        Value::Dimension(Some(n), u, _) => bound!(args, "amount", n, u, 0, 1),
        Value::Dimension(None, ..) => todo!(),
        v => {
            return Err((
                format!("$amount: {} is not a number.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::Color(Box::new(color.fade_out(amount))))
}

fn fade_out(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(2)?;
    let color = match args.get_err(0, "color")? {
        Value::Color(c) => c,
        v => {
            return Err((
                format!("$color: {} is not a color.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    let amount = match args.get_err(1, "amount")? {
        Value::Dimension(Some(n), u, _) => bound!(args, "amount", n, u, 0, 1),
        Value::Dimension(None, ..) => todo!(),
        v => {
            return Err((
                format!("$amount: {} is not a number.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::Color(Box::new(color.fade_out(amount))))
}

pub(crate) fn declare(f: &mut GlobalFunctionMap) {
    f.insert("alpha", Builtin::new(alpha));
    f.insert("opacity", Builtin::new(opacity));
    f.insert("opacify", Builtin::new(opacify));
    f.insert("fade-in", Builtin::new(fade_in));
    f.insert("transparentize", Builtin::new(transparentize));
    f.insert("fade-out", Builtin::new(fade_out));
}
