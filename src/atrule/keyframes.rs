use codemap::{Span, Spanned};

use peekmore::PeekMoreIterator;

use super::parse::ruleset_eval;
use crate::error::SassResult;
use crate::scope::Scope;
use crate::selector::Selector;
use crate::utils::{devour_whitespace, eat_number};
use crate::value::Number;
use crate::{RuleSet, Stmt, Token};

#[derive(Debug, Clone)]
pub(crate) struct Keyframes {
    pub super_selector: Selector,
    pub name: String,
    pub block_list: Vec<(Vec<Spanned<KeyframeSelector>>, Vec<Spanned<Stmt>>)>,
}

#[derive(Debug, Clone)]
pub(crate) enum KeyframeSelector {
    From,
    To,
    Percentage(Number),
}

impl Keyframes {
    pub fn from_tokens<I: Iterator<Item = Token>>(
        toks: &mut PeekMoreIterator<I>,
        scope: &mut Scope,
        super_selector: &Selector,
        kind_span: Span,
        content: Option<&[Spanned<Stmt>]>,
    ) -> SassResult<Keyframes> {
        // XXX naive implementation, just take until {
        // <keyframes-name> = <custom-ident> | <string>
        let mut name = String::new();
        devour_whitespace(toks);
        while let Some(tok) = toks.next() {
            match tok.kind {
                '\'' => todo!("<string> not implemented for <keyframes-name>"),
                '"' => todo!("<string> not implemented for <keyframes-name>"),
                '{' => break,
                ' ' | '\t' => {
                    devour_whitespace(toks);
                    name.push(' ');
                }
                kind => name.push(kind),
            }
        }
        devour_whitespace(toks);

        let mut block_list = Vec::new();
        while let Ok(keyframe_selector) =
            keyframe_selector(toks, scope, super_selector, kind_span, content)
        {
            // XXX duplicated from media
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

            block_list.push((keyframe_selector, body));
        }

        dbg!(&block_list);
        devour_whitespace(toks);
        match toks.next() {
            Some(Token { kind: '}', .. }) => Ok(Keyframes {
                super_selector: Selector::new(),
                name,
                block_list,
            }),
            Some(Token { pos, .. }) => Err(("Expected identifier.", pos).into()),
            None => Err(("Expected identifier.", kind_span).into()), // fix span
        }
    }
}

fn keyframe_selector<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    scope: &mut Scope,
    super_selector: &Selector,
    kind_span: Span,
    content: Option<&[Spanned<Stmt>]>,
) -> SassResult<Vec<Spanned<KeyframeSelector>>> {
    let mut keyframe_selector = Vec::new();
    loop {
        keyframe_selector.push(match dbg!(toks.peek()) {
            Some(Token { kind: 'f', .. }) => {
                let span = toks.next().unwrap().pos;
                match toks.next() {
                    Some(Token { kind: 'r', .. }) => match toks.next() {
                        Some(Token { kind: 'o', .. }) => match toks.next() {
                            Some(Token { kind: 'm', pos }) => Spanned {
                                node: KeyframeSelector::From,
                                span: span.merge(pos),
                            },
                            _ => return Err(("Invalid identifier.", span).into()),
                        },
                        _ => return Err(("Invalid identifier.", span).into()),
                    },
                    _ => return Err(("Invalid identifier.", span).into()),
                }
            }
            Some(Token { kind: 't', .. }) => {
                let span = toks.next().unwrap().pos;
                match toks.next() {
                    Some(Token { kind: 'o', pos }) => Spanned {
                        node: KeyframeSelector::To,
                        span: span.merge(pos),
                    },
                    _ => return Err(("Invalid identifier.", span).into()),
                }
            }
            Some(_) => match eat_number(toks) {
                Ok(Spanned { node, mut span }) => match toks.next() {
                    Some(Token { kind: '%', pos }) => {
                        span = span.merge(pos);
                        todo!("Percentage");
                    }
                    _ => return Err(("Invalid identifier.", span).into()),
                },
                Ok(Spanned { span, .. }) => return Err(("Invalid identifier.", span).into()),
                Err(_) => return Err(("Invalid identifier.", kind_span).into()),
            },
            None => return Err(("Invalid identifier.", kind_span).into()),
        });

        // repeated
        devour_whitespace(toks);
        match toks.peek() {
            Some(Token { kind: '{', .. }) => return Ok(keyframe_selector),
            Some(Token { kind: ',', .. }) => {
                toks.next();
                devour_whitespace(toks);
            }
            Some(Token { pos, .. }) => return Err(("Invalid identifier.", *pos).into()),
            None => return Err(("Invalid identifier.", kind_span).into()),
        }
    }
}
