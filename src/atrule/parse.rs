use codemap::Spanned;

use peekmore::PeekMoreIterator;

use super::AtRule;

use crate::error::SassResult;
use crate::scope::{global_var_exists, insert_global_var, Scope};
use crate::selector::Selector;
use crate::{eat_expr, Expr, RuleSet, Stmt, Token};

pub(crate) fn eat_stmts<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    scope: &mut Scope,
    super_selector: &Selector,
    at_root: bool,
    content: Option<&[Spanned<Stmt>]>,
) -> SassResult<Vec<Spanned<Stmt>>> {
    let mut stmts = Vec::new();
    while let Some(expr) = eat_expr(toks, scope, super_selector, content)? {
        let span = expr.span;
        match expr.node {
            Expr::AtRule(a) => stmts.push(Stmt::AtRule(a).span(span)),
            Expr::Style(s) => stmts.push(Stmt::Style(s).span(span)),
            Expr::Styles(s) => stmts.extend(
                s.into_iter()
                    .map(Box::new)
                    .map(Stmt::Style)
                    .map(|style| Spanned { node: style, span }),
            ),
            Expr::MixinDecl(..) | Expr::FunctionDecl(..) => todo!(),
            Expr::Selector(selector) => {
                let rules = eat_stmts(
                    toks,
                    scope,
                    &selector.resolve_parent_selectors(super_selector, true),
                    at_root,
                    content,
                )?;
                stmts.push(
                    Stmt::RuleSet(RuleSet {
                        super_selector: super_selector.clone(),
                        selector,
                        rules,
                    })
                    .span(span),
                );
            }
            //TODO: refactor handling of `Expr::VariableDecl`, as most is already handled in `eat_expr`
            Expr::VariableDecl(name, val) => {
                if at_root && global_var_exists(&name) {
                    insert_global_var(&name, *val.clone())?;
                }
                scope.insert_var(&name, *val)?;
            }
            Expr::MultilineComment(s) => stmts.push(Stmt::MultilineComment(s).span(span)),
        }
    }
    Ok(stmts)
}

pub(crate) fn eat_stmts_at_root<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    scope: &mut Scope,
    super_selector: &Selector,
    mut nesting: usize,
    is_some: bool,
    content: Option<&[Spanned<Stmt>]>,
) -> SassResult<Vec<Spanned<Stmt>>> {
    let mut stmts = Vec::new();
    while let Some(expr) = eat_expr(toks, scope, super_selector, content)? {
        let span = expr.span;
        match expr.node {
            Expr::AtRule(a) => stmts.push(Stmt::AtRule(a).span(span)),
            Expr::Style(s) => stmts.push(Stmt::Style(s).span(span)),
            Expr::Styles(s) => stmts.extend(
                s.into_iter()
                    .map(Box::new)
                    .map(Stmt::Style)
                    .map(|style| Spanned { node: style, span }),
            ),
            Expr::MixinDecl(..) | Expr::FunctionDecl(..) => todo!(),
            Expr::Selector(mut selector) => {
                selector =
                    selector.resolve_parent_selectors(super_selector, nesting > 1 || is_some);
                nesting += 1;
                let rules = eat_stmts_at_root(toks, scope, &selector, nesting, true, content)?;
                nesting -= 1;
                stmts.push(
                    Stmt::RuleSet(RuleSet {
                        super_selector: if nesting > 1 {
                            super_selector.clone()
                        } else {
                            Selector::new()
                        },
                        selector,
                        rules,
                    })
                    .span(span),
                );
            }
            Expr::VariableDecl(name, val) => {
                scope.insert_var(&name, *val)?;
            }
            Expr::MultilineComment(s) => stmts.push(Stmt::MultilineComment(s).span(span)),
        }
    }
    Ok(stmts)
}

pub(crate) fn ruleset_eval<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    scope: &mut Scope,
    super_selector: &Selector,
    at_root: bool,
    content: Option<&[Spanned<Stmt>]>,
    stmts: &mut Vec<Spanned<Stmt>>,
) -> SassResult<()> {
    for stmt in eat_stmts(toks, scope, super_selector, at_root, content)? {
        match stmt.node {
            Stmt::AtRule(AtRule::For(f)) => {
                stmts.extend(f.ruleset_eval(scope, super_selector, content)?)
            }
            Stmt::AtRule(AtRule::Each(e)) => {
                stmts.extend(e.ruleset_eval(scope, super_selector, content)?)
            }
            Stmt::AtRule(AtRule::While(w)) => {
                // TODO: should at_root be false? scoping
                stmts.extend(w.ruleset_eval(scope, super_selector, at_root, content)?)
            }
            Stmt::AtRule(AtRule::Include(s)) => stmts.extend(s),
            Stmt::AtRule(AtRule::If(i)) => stmts.extend(i.eval(scope, super_selector, content)?),
            Stmt::AtRule(AtRule::Content) => {
                if let Some(c) = content {
                    stmts.extend(c.iter().cloned());
                } else {
                    return Err((
                        "@content is only allowed within mixin declarations.",
                        stmt.span,
                    )
                        .into());
                }
            }
            _ => stmts.push(stmt),
        }
    }
    Ok(())
}
