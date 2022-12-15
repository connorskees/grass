use codemap::{Span, Spanned};

use crate::token::Token;

use super::AstExpr;

#[derive(Debug, Clone)]
pub(crate) struct Interpolation {
    pub contents: Vec<InterpolationPart>,
    pub span: Span,
}

impl Interpolation {
    pub fn new(span: Span) -> Self {
        Self {
            contents: Vec::new(),
            span,
        }
    }

    pub fn new_with_expr(e: AstExpr, span: Span) -> Self {
        Self {
            contents: vec![InterpolationPart::Expr(e)],
            span,
        }
    }

    pub fn new_plain(s: String, span: Span) -> Self {
        Self {
            contents: vec![InterpolationPart::String(s)],
            span,
        }
    }

    pub fn add_expr(&mut self, expr: Spanned<AstExpr>) {
        self.contents.push(InterpolationPart::Expr(expr.node));
        self.span = self.span.merge(expr.span);
    }

    pub fn add_string(&mut self, s: Spanned<String>) {
        match self.contents.last_mut() {
            Some(InterpolationPart::String(existing)) => *existing += &s.node,
            _ => self.contents.push(InterpolationPart::String(s.node)),
        }
        self.span = self.span.merge(s.span);
    }

    pub fn add_token(&mut self, tok: Token) {
        match self.contents.last_mut() {
            Some(InterpolationPart::String(existing)) => existing.push(tok.kind),
            _ => self
                .contents
                .push(InterpolationPart::String(tok.kind.to_string())),
        }
        self.span = self.span.merge(tok.pos);
    }

    pub fn add_char(&mut self, c: char) {
        match self.contents.last_mut() {
            Some(InterpolationPart::String(existing)) => existing.push(c),
            _ => self.contents.push(InterpolationPart::String(c.to_string())),
        }
    }

    pub fn add_interpolation(&mut self, mut other: Self) {
        self.span = self.span.merge(other.span);
        self.contents.append(&mut other.contents);
    }

    pub fn initial_plain(&self) -> &str {
        match self.contents.first() {
            Some(InterpolationPart::String(s)) => s,
            _ => "",
        }
    }

    pub fn as_plain(&self) -> Option<&str> {
        if self.contents.is_empty() {
            Some("")
        } else if self.contents.len() > 1 {
            None
        } else {
            match self.contents.first()? {
                InterpolationPart::String(s) => Some(s),
                InterpolationPart::Expr(..) => None,
            }
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum InterpolationPart {
    String(String),
    Expr(AstExpr),
}
