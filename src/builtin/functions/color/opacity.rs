use super::{Builtin, GlobalFunctionMap};

use crate::{
    args::CallArgs, common::QuoteKind, error::SassResult, parse::Parser, unit::Unit, value::Number,
    value::Value,
};

/// Check if `s` matches the regex `^[a-zA-Z]+\s*=`
fn is_ms_filter(s: &str) -> bool {
    let mut bytes = s.bytes();

    if !bytes.next().map_or(false, |c| c.is_ascii_alphabetic()) {
        return false;
    }

    bytes
        .skip_while(u8::is_ascii_alphabetic)
        .find(|c| !matches!(c, b' ' | b'\t' | b'\n'))
        == Some(b'=')
}

#[cfg(test)]
mod test {
    use super::is_ms_filter;
    #[test]
    fn test_is_ms_filter() {
        assert!(is_ms_filter("a=a"));
        assert!(is_ms_filter("a="));
        assert!(is_ms_filter("a  \t\n  =a"));
        assert!(!is_ms_filter("a  \t\n  a=a"));
        assert!(!is_ms_filter("aa"));
        assert!(!is_ms_filter("   aa"));
        assert!(!is_ms_filter("=a"));
        assert!(!is_ms_filter("1=a"));
    }
}

pub(crate) fn alpha(mut args: CallArgs, parser: &mut Parser) -> SassResult<Value> {
    if args.len() <= 1 {
        match args.get_err(0, "color")? {
            Value::Color(c) => Ok(Value::Dimension(Some(c.alpha()), Unit::None, true)),
            Value::String(s, QuoteKind::None) if is_ms_filter(&s) => {
                Ok(Value::String(format!("alpha({})", s), QuoteKind::None))
            }
            v => Err((
                format!("$color: {} is not a color.", v.inspect(args.span())?),
                args.span(),
            )
                .into()),
        }
    } else {
        let err = args.max_args(1);
        let args = args
            .get_variadic()?
            .into_iter()
            .map(|arg| match arg.node {
                Value::String(s, QuoteKind::None) if is_ms_filter(&s) => Ok(s),
                _ => {
                    err.clone()?;
                    unreachable!()
                }
            })
            .collect::<SassResult<Vec<String>>>()?;

        Ok(Value::String(
            format!("alpha({})", args.join(", "),),
            QuoteKind::None,
        ))
    }
}

pub(crate) fn opacity(mut args: CallArgs, parser: &mut Parser) -> SassResult<Value> {
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
fn opacify(mut args: CallArgs, parser: &mut Parser) -> SassResult<Value> {
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

fn fade_in(mut args: CallArgs, parser: &mut Parser) -> SassResult<Value> {
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
fn transparentize(mut args: CallArgs, parser: &mut Parser) -> SassResult<Value> {
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

fn fade_out(mut args: CallArgs, parser: &mut Parser) -> SassResult<Value> {
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
