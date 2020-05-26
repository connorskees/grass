use codemap::{Span, Spanned};

use peekmore::PeekMoreIterator;

use super::parse::ruleset_eval;
use crate::error::SassResult;
use crate::scope::Scope;
use crate::selector::Selector;
use crate::utils::{devour_whitespace, parse_interpolation};
use crate::{RuleSet, Stmt, Token};

#[derive(Debug, Clone)]
pub(crate) struct Media {
    pub super_selector: Selector,
    pub params: String,
    pub body: Vec<Spanned<Stmt>>,
}

impl Media {
    pub fn from_tokens<I: Iterator<Item = Token>>(
        toks: &mut PeekMoreIterator<I>,
        scope: &mut Scope,
        super_selector: &Selector,
        kind_span: Span,
        content: Option<&[Spanned<Stmt>]>,
    ) -> SassResult<Media> {
        let mut params = String::new();
        while let Some(tok) = toks.next() {
            match tok.kind {
                '{' => break,
                '#' => {
                    if let Some(Token { kind: '{', pos }) = toks.peek().cloned() {
                        toks.next();
                        let interpolation = parse_interpolation(toks, scope, super_selector, pos)?;
                        params.push_str(&interpolation.node.to_css_string(interpolation.span)?);
                        continue;
                    } else {
                        params.push(tok.kind);
                    }
                }
                '\n' | ' ' | '\t' => {
                    devour_whitespace(toks);
                    params.push(' ');
                    continue;
                }
                _ => {}
            }
            params.push(tok.kind);
        }

        if params.is_empty() {
            return Err(("Expected identifier.", kind_span).into());
        }

        let mut raw_body = Vec::new();
        ruleset_eval(toks, scope, super_selector, false, content, &mut raw_body)?;
        let mut rules = Vec::with_capacity(raw_body.len());
        let mut body = Vec::new();

        for stmt in raw_body {
            match stmt.node {
                Stmt::Style(..) => body.push(stmt),
                _ => rules.push(stmt),
            }
        }

        if !super_selector.is_empty() {
            body = vec![Spanned {
                node: Stmt::RuleSet(RuleSet {
                    selector: super_selector.clone(),
                    rules: body,
                    super_selector: Selector::new(),
                }),
                span: kind_span,
            }];
        }
        body.append(&mut rules);

        Ok(Media {
            super_selector: Selector::new(),
            params: params.trim().to_owned(),
            body,
        })
    }
}
