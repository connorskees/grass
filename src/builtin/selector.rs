#![allow(unused_variables, unused_mut)]

use super::{Builtin, GlobalFunctionMap};

use crate::args::CallArgs;
use crate::error::SassResult;
use crate::scope::Scope;
use crate::selector::{ComplexSelector, ComplexSelectorComponent, Selector, SelectorList};
use crate::value::Value;

fn is_superselector(
    mut args: CallArgs,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<Value> {
    args.max_args(2)?;
    let parent_selector = arg!(args, scope, super_selector, 0, "super").to_selector(
        args.span(),
        scope,
        super_selector,
        "super",
        false,
    )?;
    let child_selector = arg!(args, scope, super_selector, 1, "sub").to_selector(
        args.span(),
        scope,
        super_selector,
        "sub",
        false,
    )?;

    Ok(Value::bool(
        parent_selector.is_super_selector(&child_selector),
    ))
}

fn simple_selectors(
    mut args: CallArgs,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<Value> {
    args.max_args(1)?;
    todo!("built-in fn simple-selectors")
}

fn selector_parse(
    mut args: CallArgs,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<Value> {
    args.max_args(1)?;
    Ok(arg!(args, scope, super_selector, 0, "selector")
        .to_selector(args.span(), scope, super_selector, "selector", false)?
        .into_value())
}

fn selector_nest(args: CallArgs, scope: &Scope, super_selector: &Selector) -> SassResult<Value> {
    let span = args.span();
    let selectors = args.get_variadic(scope, super_selector)?;
    if selectors.is_empty() {
        return Err(("$selectors: At least one selector must be passed.", span).into());
    }

    Ok(selectors
        .into_iter()
        .map(|sel| {
            sel.node
                .to_selector(span, scope, super_selector, "selectors", true)
        })
        .collect::<SassResult<Vec<Selector>>>()?
        .into_iter()
        .fold(Selector::new(), |parent, child| {
            child.resolve_parent_selectors(&parent, true)
        })
        .into_value())
}

fn selector_append(
    mut args: CallArgs,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<Value> {
    let span = args.span();
    let selectors = args.get_variadic(scope, super_selector)?;
    if selectors.is_empty() {
        return Err(("$selectors: At least one selector must be passed.", span).into());
    }

    let mut parsed_selectors = selectors
        .into_iter()
        .map(|s| {
            let tmp = s
                .node
                .to_selector(span, scope, super_selector, "selectors", false)?;
            if tmp.contains_parent_selector() {
                Err(("Parent selectors aren't allowed here.", span).into())
            } else {
                Ok(tmp)
            }
        })
        .collect::<SassResult<Vec<Selector>>>()?;

    let first = parsed_selectors.remove(0);
    Ok(parsed_selectors
        .into_iter()
        .try_fold(first, |parent, child| -> SassResult<Selector> {
            Ok(Selector(SelectorList {
                components: child
                    .0
                    .components
                    .into_iter()
                    .map(|complex| -> SassResult<ComplexSelector> {
                        let compound = complex.components.first();
                        if let Some(ComplexSelectorComponent::Compound(compound)) = compound {
                            let mut components = vec![match compound.clone().prepend_parent() {
                                Some(v) => ComplexSelectorComponent::Compound(v),
                                None => {
                                    return Err((
                                        format!("Can't append {} to {}.", complex, parent),
                                        span,
                                    )
                                        .into())
                                }
                            }];
                            components.extend(complex.components.into_iter().skip(1));
                            Ok(ComplexSelector {
                                components,
                                line_break: false,
                            })
                        } else {
                            Err((format!("Can't append {} to {}.", complex, parent), span).into())
                        }
                    })
                    .collect::<SassResult<Vec<ComplexSelector>>>()?,
            })
            .resolve_parent_selectors(&parent, false))
        })?
        .into_value())
}

fn selector_extend(
    mut args: CallArgs,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<Value> {
    args.max_args(3)?;
    todo!("built-in fn selector-extend")
}

fn selector_replace(
    mut args: CallArgs,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<Value> {
    args.max_args(3)?;
    todo!("built-in fn selector-replace")
}

fn selector_unify(
    mut args: CallArgs,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<Value> {
    args.max_args(2)?;
    let selector1 = arg!(args, scope, super_selector, 0, "selector1").to_selector(
        args.span(),
        scope,
        super_selector,
        "selector1",
        false,
    )?;

    if selector1.contains_parent_selector() {
        return Err((
            "$selector1: Parent selectors aren't allowed here.",
            args.span(),
        )
            .into());
    }

    let selector2 = arg!(args, scope, super_selector, 1, "selector2").to_selector(
        args.span(),
        scope,
        super_selector,
        "selector2",
        false,
    )?;

    if selector2.contains_parent_selector() {
        return Err((
            "$selector2: Parent selectors aren't allowed here.",
            args.span(),
        )
            .into());
    }

    Ok(match selector1.unify(&selector2) {
        Some(sel) => sel.into_value(),
        None => Value::Null,
    })
}

pub(crate) fn declare(f: &mut GlobalFunctionMap) {
    f.insert("is-superselector", Builtin::new(is_superselector));
    f.insert("simple-selectors", Builtin::new(simple_selectors));
    f.insert("selector-parse", Builtin::new(selector_parse));
    f.insert("selector-nest", Builtin::new(selector_nest));
    f.insert("selector-append", Builtin::new(selector_append));
    f.insert("selector-extend", Builtin::new(selector_extend));
    f.insert("selector-replace", Builtin::new(selector_replace));
    f.insert("selector-unify", Builtin::new(selector_unify));
}
