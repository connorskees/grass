use codemap::Spanned;

use peekmore::PeekMoreIterator;

use crate::error::SassResult;
use crate::scope::{global_var_exists, insert_global_var, Scope};
use crate::selector::Selector;
use crate::{eat_expr, Expr, RuleSet, Stmt, Token};

pub(crate) fn eat_stmts<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    scope: &mut Scope,
    super_selector: &Selector,
    at_root: bool,
) -> SassResult<Vec<Spanned<Stmt>>> {
    let mut stmts = Vec::new();
    while let Some(expr) = eat_expr(toks, scope, super_selector)? {
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
                let rules = eat_stmts(toks, scope, &super_selector.zip(&selector), at_root)?;
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
) -> SassResult<Vec<Spanned<Stmt>>> {
    let mut stmts = Vec::new();
    while let Some(expr) = eat_expr(toks, scope, super_selector)? {
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
                if nesting > 1 || is_some {
                    selector = super_selector.zip(&selector);
                } else {
                    selector = Selector::replace(super_selector, selector);
                }
                nesting += 1;
                let rules = eat_stmts_at_root(toks, scope, &selector, nesting, true)?;
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
