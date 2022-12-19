use std::iter::Iterator;

use codemap::{Span, Spanned};

use crate::{
    color::Color,
    common::{BinaryOp, Brackets, Identifier, ListSeparator, QuoteKind, UnaryOp},
    unit::Unit,
    value::{CalculationName, Number},
};

use super::{ArgumentInvocation, AstSupportsCondition, Interpolation, InterpolationPart};

/// Represented by the `if` function
#[derive(Debug, Clone)]
pub(crate) struct Ternary(pub ArgumentInvocation);

#[derive(Debug, Clone)]
pub(crate) struct ListExpr {
    pub elems: Vec<Spanned<AstExpr>>,
    pub separator: ListSeparator,
    pub brackets: Brackets,
}

#[derive(Debug, Clone)]
pub(crate) struct FunctionCallExpr {
    pub namespace: Option<Spanned<Identifier>>,
    pub name: Identifier,
    pub arguments: Box<ArgumentInvocation>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct InterpolatedFunction {
    pub name: Interpolation,
    pub arguments: Box<ArgumentInvocation>,
    pub span: Span,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct AstSassMap(pub Vec<(AstExpr, AstExpr)>);

#[derive(Debug, Clone)]
pub(crate) enum AstExpr {
    BinaryOp {
        lhs: Box<Self>,
        op: BinaryOp,
        rhs: Box<Self>,
        allows_slash: bool,
        span: Span,
    },
    True,
    False,
    Calculation {
        name: CalculationName,
        args: Vec<Self>,
    },
    Color(Box<Color>),
    FunctionCall(FunctionCallExpr),
    If(Box<Ternary>),
    InterpolatedFunction(InterpolatedFunction),
    List(ListExpr),
    Map(AstSassMap),
    Null,
    Number {
        n: Number,
        unit: Unit,
    },
    Paren(Box<Self>),
    ParentSelector,
    String(StringExpr, Span),
    Supports(Box<AstSupportsCondition>),
    UnaryOp(UnaryOp, Box<Self>),
    Variable {
        name: Spanned<Identifier>,
        namespace: Option<Spanned<Identifier>>,
    },
}

// todo: make quotes bool
// todo: track span inside
#[derive(Debug, Clone)]
pub(crate) struct StringExpr(pub Interpolation, pub QuoteKind);

impl StringExpr {
    fn quote_inner_text(
        text: &str,
        quote: char,
        buffer: &mut Interpolation,
        // default=false
        is_static: bool,
    ) {
        let mut chars = text.chars().peekable();
        while let Some(char) = chars.next() {
            if char == '\n' || char == '\r' {
                buffer.add_char('\\');
                buffer.add_char('a');
                if let Some(next) = chars.peek() {
                    if next.is_ascii_whitespace() || next.is_ascii_hexdigit() {
                        buffer.add_char(' ');
                    }
                }
            } else {
                if char == quote
                    || char == '\\'
                    || (is_static && char == '#' && chars.peek() == Some(&'{'))
                {
                    buffer.add_char('\\');
                }
                buffer.add_char(char);
            }
        }
    }

    fn best_quote<'a>(strings: impl Iterator<Item = &'a str>) -> char {
        let mut contains_double_quote = false;
        for s in strings {
            for c in s.chars() {
                if c == '\'' {
                    return '"';
                }
                if c == '"' {
                    contains_double_quote = true;
                }
            }
        }
        if contains_double_quote {
            '\''
        } else {
            '"'
        }
    }

    pub fn as_interpolation(self, span: Span, is_static: bool) -> Interpolation {
        if self.1 == QuoteKind::None {
            return self.0;
        }

        let quote = Self::best_quote(self.0.contents.iter().filter_map(|c| match c {
            InterpolationPart::Expr(..) => None,
            InterpolationPart::String(text) => Some(text.as_str()),
        }));

        let mut buffer = Interpolation::new();
        buffer.add_char(quote);

        for value in self.0.contents {
            match value {
                InterpolationPart::Expr(e) => buffer.add_expr(Spanned { node: e, span }),
                InterpolationPart::String(text) => {
                    Self::quote_inner_text(&text, quote, &mut buffer, is_static)
                }
            }
        }

        buffer.add_char(quote);

        buffer
    }
}

impl AstExpr {
    pub fn is_variable(&self) -> bool {
        matches!(self, Self::Variable { .. })
    }

    pub fn is_slash_operand(&self) -> bool {
        match self {
            Self::Number { .. } | Self::Calculation { .. } => true,
            Self::BinaryOp { allows_slash, .. } => *allows_slash,
            _ => false,
        }
    }

    pub fn slash(left: Self, right: Self, span: Span) -> Self {
        Self::BinaryOp {
            lhs: Box::new(left),
            op: BinaryOp::Div,
            rhs: Box::new(right),
            allows_slash: true,
            span,
        }
    }

    pub const fn span(self, span: Span) -> Spanned<Self> {
        Spanned { node: self, span }
    }
}
