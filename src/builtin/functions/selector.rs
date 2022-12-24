use crate::builtin::builtin_imports::*;

use crate::selector::{
    ComplexSelector, ComplexSelectorComponent, ExtensionStore, SelectorList,
};
use crate::serializer::serialize_selector_list;

pub(crate) fn is_superselector(
    mut args: ArgumentResult,
    visitor: &mut Visitor,
) -> SassResult<Value> {
    args.max_args(2)?;
    let parent_selector = args
        .get_err(0, "super")?
        .to_selector(visitor, "super", false)?;
    let child_selector = args.get_err(1, "sub")?.to_selector(visitor, "sub", false)?;

    Ok(Value::bool(
        parent_selector.is_superselector(&child_selector),
    ))
}

pub(crate) fn simple_selectors(
    mut args: ArgumentResult,
    visitor: &mut Visitor,
) -> SassResult<Value> {
    args.max_args(1)?;
    // todo: Value::to_compound_selector
    let selector = args
        .get_err(0, "selector")?
        .to_selector(visitor, "selector", false)?;

    if selector.components.len() != 1 {
        return Err(("$selector: expected selector.", args.span()).into());
    }

    let compound = if let Some(ComplexSelectorComponent::Compound(compound)) =
        selector.components[0].components.first().cloned()
    {
        compound
    } else {
        todo!()
    };

    Ok(Value::List(
        compound
            .components
            .into_iter()
            .map(|simple| Value::String(simple.to_string(), QuoteKind::None))
            .collect(),
        ListSeparator::Comma,
        Brackets::None,
    ))
}

pub(crate) fn selector_parse(mut args: ArgumentResult, visitor: &mut Visitor) -> SassResult<Value> {
    args.max_args(1)?;
    Ok(args
        .get_err(0, "selector")?
        .to_selector(visitor, "selector", false)
        .map_err(|_| ("$selector: expected selector.", args.span()))?
        .to_sass_list())
}

pub(crate) fn selector_nest(args: ArgumentResult, visitor: &mut Visitor) -> SassResult<Value> {
    let span = args.span();
    let selectors = args.get_variadic()?;
    if selectors.is_empty() {
        return Err(("$selectors: At least one selector must be passed.", span).into());
    }

    Ok(selectors
        .into_iter()
        .map(|sel| sel.node.to_selector(visitor, "selectors", true))
        .collect::<SassResult<Vec<_>>>()?
        .into_iter()
        .try_fold(
            SelectorList::new(span),
            |parent, child| -> SassResult<SelectorList> {
                child.resolve_parent_selectors(Some(parent), true)
            },
        )?
        .to_sass_list())
}

pub(crate) fn selector_append(args: ArgumentResult, visitor: &mut Visitor) -> SassResult<Value> {
    let span = args.span();
    let selectors = args.get_variadic()?;
    if selectors.is_empty() {
        return Err(("$selectors: At least one selector must be passed.", span).into());
    }

    let mut parsed_selectors = selectors
        .into_iter()
        .map(|s| s.node.to_selector(visitor, "selectors", false))
        .collect::<SassResult<Vec<_>>>()?;

    let first = parsed_selectors.remove(0);
    Ok(parsed_selectors
        .into_iter()
        .try_fold(first, |parent, child| -> SassResult<_> {
            SelectorList {
                components: child
                    .components
                    .into_iter()
                    .map(|complex| -> SassResult<ComplexSelector> {
                        let compound = complex.components.first();
                        if let Some(ComplexSelectorComponent::Compound(compound)) = compound {
                            let mut components = vec![match compound.clone().prepend_parent() {
                                Some(v) => ComplexSelectorComponent::Compound(v),
                                None => {
                                    return Err((
                                        format!(
                                            "Can't append {} to {}.",
                                            complex,
                                            serialize_selector_list(
                                                &parent,
                                                visitor.parser.options,
                                                span
                                            )
                                        ),
                                        span,
                                    )
                                        .into())
                                }
                            }];
                            components.extend(complex.components.into_iter().skip(1));
                            Ok(ComplexSelector::new(components, false))
                        } else {
                            Err((
                                format!(
                                    "Can't append {} to {}.",
                                    complex,
                                    serialize_selector_list(&parent, visitor.parser.options, span)
                                ),
                                span,
                            )
                                .into())
                        }
                    })
                    .collect::<SassResult<Vec<ComplexSelector>>>()?,
                span,
            }
            .resolve_parent_selectors(Some(parent.clone()), false)
        })?
        .to_sass_list())
}

pub(crate) fn selector_extend(
    mut args: ArgumentResult,
    visitor: &mut Visitor,
) -> SassResult<Value> {
    args.max_args(3)?;
    let selector = args
        .get_err(0, "selector")?
        .to_selector(visitor, "selector", false)?;
    let target = args
        .get_err(1, "extendee")?
        .to_selector(visitor, "extendee", false)?;
    let source = args
        .get_err(2, "extender")?
        .to_selector(visitor, "extender", false)?;

    Ok(ExtensionStore::extend(selector, source, target, args.span())?.to_sass_list())
}

pub(crate) fn selector_replace(
    mut args: ArgumentResult,
    visitor: &mut Visitor,
) -> SassResult<Value> {
    args.max_args(3)?;
    let selector = args
        .get_err(0, "selector")?
        .to_selector(visitor, "selector", true)?;
    let target = args
        .get_err(1, "original")?
        .to_selector(visitor, "original", true)?;
    let source = args
        .get_err(2, "replacement")?
        .to_selector(visitor, "replacement", true)?;
    Ok(ExtensionStore::replace(selector, source, target, args.span())?.to_sass_list())
}

pub(crate) fn selector_unify(mut args: ArgumentResult, visitor: &mut Visitor) -> SassResult<Value> {
    args.max_args(2)?;
    let selector1 = args
        .get_err(0, "selector1")?
        .to_selector(visitor, "selector1", true)?;

    if selector1.contains_parent_selector() {
        return Err((
            "$selector1: Parent selectors aren't allowed here.",
            args.span(),
        )
            .into());
    }

    let selector2 = args
        .get_err(1, "selector2")?
        .to_selector(visitor, "selector2", true)?;

    if selector2.contains_parent_selector() {
        return Err((
            "$selector2: Parent selectors aren't allowed here.",
            args.span(),
        )
            .into());
    }

    Ok(match selector1.unify(&selector2) {
        Some(sel) => sel.to_sass_list(),
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
