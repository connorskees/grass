use crate::builtin::builtin_imports::*;

// todo: figure out better way for this
pub(crate) fn IF_ARGUMENTS() -> ArgumentDeclaration {
    ArgumentDeclaration {
        args: vec![
            Argument {
                name: Identifier::from("condition"),
                default: None,
            },
            Argument {
                name: Identifier::from("if-true"),
                default: None,
            },
            Argument {
                name: Identifier::from("if-false"),
                default: None,
            },
        ],
        rest: None,
    }
}

// pub(crate) static IF_ARGUMENTS: Lazy<ArgumentDeclaration> = Lazy::new(|| ArgumentDeclaration {
//     args: vec![
//         Argument {
//             name: Identifier::from("condition"),
//             default: None,
//         },
//         Argument {
//             name: Identifier::from("if-true"),
//             default: None,
//         },
//         Argument {
//             name: Identifier::from("if-false"),
//             default: None,
//         },
//     ],
//     rest: None,
// });

fn if_(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(3)?;
    if args.get_err(0, "condition")?.is_true() {
        Ok(args.get_err(1, "if-true")?)
    } else {
        Ok(args.get_err(2, "if-false")?)
    }
}

pub(crate) fn feature_exists(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    match args.get_err(0, "feature")? {
        #[allow(clippy::match_same_arms)]
        Value::String(s, _) => Ok(match s.as_str() {
            // A local variable will shadow a global variable unless
            // `!global` is used.
            "global-variable-shadowing" => Value::True,
            // the @extend rule will affect selectors nested in pseudo-classes
            // like :not()
            "extend-selector-pseudoclass" => Value::True,
            // Full support for unit arithmetic using units defined in the
            // [Values and Units Level 3][] spec.
            "units-level-3" => Value::True,
            // The Sass `@error` directive is supported.
            "at-error" => Value::True,
            // The "Custom Properties Level 1" spec is supported. This means
            // that custom properties are parsed statically, with only
            // interpolation treated as SassScript.
            "custom-property" => Value::True,
            _ => Value::False,
        }),
        v => Err((
            format!("$feature: {} is not a string.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn unit(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    let unit = match args.get_err(0, "number")? {
        Value::Dimension {
            num: _,
            unit: u,
            as_slash: _,
        } => u.to_string(),
        v => {
            return Err((
                format!("$number: {} is not a number.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    Ok(Value::String(unit, QuoteKind::Quoted))
}

pub(crate) fn type_of(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    let value = args.get_err(0, "value")?;
    Ok(Value::String(value.kind().to_owned(), QuoteKind::None))
}

pub(crate) fn unitless(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    Ok(match args.get_err(0, "number")? {
        Value::Dimension {
            num: _,
            unit: Unit::None,
            as_slash: _,
        } => Value::True,
        Value::Dimension { .. } => Value::False,
        v => {
            return Err((
                format!("$number: {} is not a number.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    })
}

pub(crate) fn inspect(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    Ok(Value::String(
        args.get_err(0, "value")?.inspect(args.span())?.into_owned(),
        QuoteKind::None,
    ))
}

pub(crate) fn variable_exists(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    match args.get_err(0, "name")? {
        Value::String(s, _) => Ok(Value::bool(parser.env.var_exists(s.into(), None)?)),
        v => Err((
            format!("$name: {} is not a string.", v.inspect(args.span())?),
            args.span(),
        )
            .into()),
    }
}

pub(crate) fn global_variable_exists(
    mut args: ArgumentResult,
    parser: &mut Visitor,
) -> SassResult<Value> {
    args.max_args(2)?;

    let name: Identifier = match args.get_err(0, "name")? {
        Value::String(s, _) => s.into(),
        v => {
            return Err((
                format!("$name: {} is not a string.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    let module = match args.default_arg(1, "module", Value::Null) {
        Value::String(s, _) => Some(s),
        Value::Null => None,
        v => {
            return Err((
                format!("$module: {} is not a string.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    Ok(Value::bool(if let Some(module_name) = module {
        (*(*parser.env.modules)
            .borrow()
            .get(module_name.into(), args.span())?)
        .borrow()
        .var_exists(name)
    } else {
        (*parser.env.global_vars()).borrow().contains_key(&name)
    }))
}

pub(crate) fn mixin_exists(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(2)?;
    let name: Identifier = match args.get_err(0, "name")? {
        Value::String(s, _) => s.into(),
        v => {
            return Err((
                format!("$name: {} is not a string.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    let module = match args.default_arg(1, "module", Value::Null) {
        Value::String(s, _) => Some(s),
        Value::Null => None,
        v => {
            return Err((
                format!("$module: {} is not a string.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    Ok(Value::bool(if let Some(module_name) = module {
        (*(*parser.env.modules)
            .borrow()
            .get(module_name.into(), args.span())?)
        .borrow()
        .mixin_exists(name)
    } else {
        parser.env.mixin_exists(name)
    }))
}

pub(crate) fn function_exists(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(2)?;

    let name: Identifier = match args.get_err(0, "name")? {
        Value::String(s, _) => s.into(),
        v => {
            return Err((
                format!("$name: {} is not a string.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    let module = match args.default_arg(1, "module", Value::Null) {
        Value::String(s, _) => Some(s),
        Value::Null => None,
        v => {
            return Err((
                format!("$module: {} is not a string.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    Ok(Value::bool(if let Some(module_name) = module {
        (*(*parser.env.modules)
            .borrow()
            .get(module_name.into(), args.span())?)
        .borrow()
        .fn_exists(name)
    } else {
        parser.env.fn_exists(name)
    }))
}

pub(crate) fn get_function(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(3)?;
    let name: Identifier = match args.get_err(0, "name")? {
        Value::String(s, _) => s.into(),
        v => {
            return Err((
                format!("$name: {} is not a string.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };
    let css = args.default_arg(1, "css", Value::False).is_true();
    let module = match args.default_arg(2, "module", Value::Null) {
        Value::String(s, ..) => Some(s),
        Value::Null => None,
        v => {
            return Err((
                format!("$module: {} is not a string.", v.inspect(args.span())?),
                args.span(),
            )
                .into())
        }
    };

    let func = if let Some(module_name) = module {
        if css {
            return Err((
                "$css and $module may not both be passed at once.",
                args.span(),
            )
                .into());
        }

        parser.env.get_fn(
            name,
            Some(Spanned {
                node: module_name.into(),
                span: args.span(),
            }),
        )?
    } else {
        match parser.env.get_fn(name, None)? {
            Some(f) => Some(f),
            None => match GLOBAL_FUNCTIONS.get(name.as_str()) {
                Some(f) => Some(SassFunction::Builtin(f.clone(), name)),
                None => None,
            },
        }
    };

    match func {
        Some(func) => Ok(Value::FunctionRef(func)),
        None => Err((format!("Function not found: {}", name), args.span()).into()),
    }
}

pub(crate) fn call(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    let span = args.span();
    let func = match args.get_err(0, "function")? {
        Value::FunctionRef(f) => f,
        v => {
            return Err((
                format!(
                    "$function: {} is not a function reference.",
                    v.inspect(span)?
                ),
                span,
            )
                .into())
        }
    };

    args.remove_positional(0).unwrap();

    parser.run_function_callable_with_maybe_evaled(func, MaybeEvaledArguments::Evaled(args), span)
}

#[allow(clippy::needless_pass_by_value)]
pub(crate) fn content_exists(args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(0)?;
    if !parser.flags.in_mixin() {
        return Err((
            "content-exists() may only be called within a mixin.",
            parser.parser.span_before,
        )
            .into());
    }
    Ok(Value::bool(parser.env.content.is_some()))
}

pub(crate) fn keywords(mut args: ArgumentResult, parser: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;

    let span = args.span();

    let args = match args.get_err(0, "args")? {
        Value::ArgList(args) => args,
        v => {
            return Err((
                format!("$args: {} is not an argument list.", v.inspect(span)?),
                span,
            )
                .into())
        }
    };

    return Ok(Value::Map(SassMap::new_with(
        args.into_keywords()
            .into_iter()
            .map(|(name, val)| {
                (
                    Value::String(name.to_string(), QuoteKind::None).span(span),
                    val,
                )
            })
            .collect(),
    )));
}

pub(crate) fn declare(f: &mut GlobalFunctionMap) {
    f.insert("if", Builtin::new(if_));
    f.insert("feature-exists", Builtin::new(feature_exists));
    f.insert("unit", Builtin::new(unit));
    f.insert("type-of", Builtin::new(type_of));
    f.insert("unitless", Builtin::new(unitless));
    f.insert("inspect", Builtin::new(inspect));
    f.insert("variable-exists", Builtin::new(variable_exists));
    f.insert(
        "global-variable-exists",
        Builtin::new(global_variable_exists),
    );
    f.insert("mixin-exists", Builtin::new(mixin_exists));
    f.insert("function-exists", Builtin::new(function_exists));
    f.insert("get-function", Builtin::new(get_function));
    f.insert("call", Builtin::new(call));
    f.insert("content-exists", Builtin::new(content_exists));
    f.insert("keywords", Builtin::new(keywords));
}
