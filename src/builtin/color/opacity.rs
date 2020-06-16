use super::{Builtin, GlobalFunctionMap};

use crate::args::CallArgs;
use crate::common::QuoteKind;
use crate::error::SassResult;
use crate::parse::Parser;
use crate::unit::Unit;
use crate::value::Number;
use crate::value::Value;

fn alpha(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    match parser.arg(&mut args, 0, "color")? {
        Value::Color(c) => Ok(Value::Dimension(c.alpha(), Unit::None)),
        v => Err((
            format!("$color: {} is not a color.", v.to_css_string(args.span())?),
            args.span(),
        )
            .into()),
    }
}

fn opacity(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(1)?;
    match parser.arg(&mut args, 0, "color")? {
        Value::Color(c) => Ok(Value::Dimension(c.alpha(), Unit::None)),
        Value::Dimension(num, unit) => Ok(Value::String(
            format!("opacity({}{})", num, unit),
            QuoteKind::None,
        )),
        v => Err((
            format!("$color: {} is not a color.", v.to_css_string(args.span())?),
            args.span(),
        )
            .into()),
    }
}

fn opacify(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(2)?;
    let color = match parser.arg(&mut args, 0, "color")? {
        Value::Color(c) => c,
        v => {
            return Err((
                format!("$color: {} is not a color.", v.to_css_string(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    let amount = match parser.arg(&mut args, 1, "amount")? {
        Value::Dimension(n, u) => bound!(args, "amount", n, u, 0, 1),
        v => {
            return Err((
                format!(
                    "$amount: {} is not a number.",
                    v.to_css_string(args.span())?
                ),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::Color(Box::new(color.fade_in(amount))))
}

fn fade_in(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(2)?;
    let color = match parser.arg(&mut args, 0, "color")? {
        Value::Color(c) => c,
        v => {
            return Err((
                format!("$color: {} is not a color.", v.to_css_string(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    let amount = match parser.arg(&mut args, 1, "amount")? {
        Value::Dimension(n, u) => bound!(args, "amount", n, u, 0, 1),
        v => {
            return Err((
                format!(
                    "$amount: {} is not a number.",
                    v.to_css_string(args.span())?
                ),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::Color(Box::new(color.fade_in(amount))))
}

fn transparentize(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(2)?;
    let color = match parser.arg(&mut args, 0, "color")? {
        Value::Color(c) => c,
        v => {
            return Err((
                format!("$color: {} is not a color.", v.to_css_string(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    let amount = match parser.arg(&mut args, 1, "amount")? {
        Value::Dimension(n, u) => bound!(args, "amount", n, u, 0, 1),
        v => {
            return Err((
                format!(
                    "$amount: {} is not a number.",
                    v.to_css_string(args.span())?
                ),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::Color(Box::new(color.fade_out(amount))))
}

fn fade_out(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(2)?;
    let color = match parser.arg(&mut args, 0, "color")? {
        Value::Color(c) => c,
        v => {
            return Err((
                format!("$color: {} is not a color.", v.to_css_string(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    let amount = match parser.arg(&mut args, 1, "amount")? {
        Value::Dimension(n, u) => bound!(args, "amount", n, u, 0, 1),
        v => {
            return Err((
                format!(
                    "$amount: {} is not a number.",
                    v.to_css_string(args.span())?
                ),
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
