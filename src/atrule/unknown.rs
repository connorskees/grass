use std::iter::Peekable;

use super::parse::eat_stmts;
use crate::common::{Scope, Symbol};
use crate::error::SassResult;
use crate::selector::Selector;
use crate::utils::{devour_whitespace, parse_interpolation};
use crate::{RuleSet, Stmt, Token, TokenKind};

#[derive(Debug, Clone)]
pub(crate) struct UnknownAtRule {
    pub name: String,
    pub super_selector: Selector,
    pub params: String,
    pub body: Vec<Stmt>,
}

impl UnknownAtRule {
    pub fn from_tokens<I: Iterator<Item = Token>>(
        toks: &mut Peekable<I>,
        name: &str,
        scope: &mut Scope,
        super_selector: &Selector,
    ) -> SassResult<UnknownAtRule> {
        let mut params = String::new();
        while let Some(tok) = toks.next() {
            match tok.kind {
                TokenKind::Symbol(Symbol::OpenCurlyBrace) => break,
                TokenKind::Interpolation => {
                    params.push_str(&parse_interpolation(toks, scope, super_selector)?.to_string());
                    continue;
                }
                TokenKind::Variable(..) => params.push('$'),
                TokenKind::Whitespace(..) => {
                    devour_whitespace(toks);
                    params.push(' ');
                    continue;
                }
                _ => {}
            }
            params.push_str(&tok.kind.to_string());
        }

        let raw_body = eat_stmts(toks, scope, super_selector)?;
        let mut body = Vec::with_capacity(raw_body.len());
        body.push(Stmt::RuleSet(RuleSet::new()));
        let mut rules = Vec::new();
        for stmt in raw_body {
            match stmt {
                s @ Stmt::Style(..) => rules.push(s),
                s => body.push(s),
            }
        }

        body[0] = Stmt::RuleSet(RuleSet {
            selector: super_selector.clone(),
            rules,
            super_selector: Selector::new(),
        });

        Ok(UnknownAtRule {
            name: name.to_owned(),
            super_selector: Selector::new(),
            params: params.trim().to_owned(),
            body,
        })
    }
}
