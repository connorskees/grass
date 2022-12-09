use std::{
    collections::{BTreeMap, BTreeSet},
    iter::Iterator,
    mem,
};

use num_bigint::BigInt;
use num_rational::{BigRational, Rational64};
use num_traits::{pow, One, ToPrimitive};

use codemap::{Span, Spanned};

use crate::{
    builtin::GLOBAL_FUNCTIONS,
    color::{Color, NAMED_COLORS},
    common::{unvendor, BinaryOp, Brackets, Identifier, ListSeparator, QuoteKind, UnaryOp},
    error::SassResult,
    lexer::Lexer,
    unit::Unit,
    utils::{as_hex, is_name, ParsedNumber},
    value::{Number, SassFunction, SassMap, Value},
    Token,
};

use super::{common::ContextFlags, Interpolation, InterpolationPart, Parser};

pub(crate) type Predicate<'a> = &'a dyn Fn(&mut Parser<'_, '_>) -> bool;

#[derive(Debug, Clone, Copy)]
pub(crate) enum Calculation {
    Calc,
    Min,
    Max,
    Clamp,
}

/// Represented by the `if` function
#[derive(Debug, Clone)]
pub(crate) struct Ternary(pub ArgumentInvocation);

#[derive(Debug, Clone)]
pub(crate) enum AstExpr {
    BinaryOp {
        lhs: Box<Self>,
        op: BinaryOp,
        rhs: Box<Self>,
        allows_slash: bool,
    },
    True,
    False,
    Calculation {
        name: Calculation,
        args: Vec<Self>,
    },
    Color(Box<Color>),
    FunctionRef(SassFunction),
    FunctionCall {
        namespace: Option<String>,
        name: Identifier,
        arguments: Box<ArgumentInvocation>,
    },
    If(Box<Ternary>),
    InterpolatedFunction {
        name: Interpolation,
        arguments: Box<ArgumentInvocation>,
    },
    List {
        elems: Vec<Spanned<Self>>,
        separator: ListSeparator,
        brackets: Brackets,
    },
    Map(AstSassMap),
    Null,
    Number {
        n: Number,
        unit: Unit,
    },
    Paren(Box<Self>),
    ParentSelector,
    String(StringExpr),
    UnaryOp(UnaryOp, Box<Self>),
    Value(Value),
    Variable {
        name: Identifier,
        namespace: Option<String>,
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
            InterpolationPart::Expr(e) => None,
            InterpolationPart::String(text) => Some(text.as_str()),
        }));
        let mut buffer = Interpolation::new(span);
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

    pub fn slash(left: Self, right: Self) -> Self {
        Self::BinaryOp {
            lhs: Box::new(left),
            op: BinaryOp::Div,
            rhs: Box::new(right),
            allows_slash: true,
        }
    }

    pub const fn span(self, span: Span) -> Spanned<Self> {
        Spanned { node: self, span }
    }
}

#[derive(Debug, Clone, Default)]
pub(crate) struct AstSassMap(pub Vec<(AstExpr, AstExpr)>);

#[derive(Debug, Clone)]
pub(crate) struct Argument {
    pub name: Identifier,
    pub default: Option<AstExpr>,
}

#[derive(Debug, Clone)]
pub(crate) struct ArgumentDeclaration {
    pub args: Vec<Argument>,
    pub rest: Option<Identifier>,
}

impl ArgumentDeclaration {
    pub fn empty() -> Self {
        Self {
            args: Vec::new(),
            rest: None,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ArgumentInvocation {
    pub positional: Vec<AstExpr>,
    pub named: BTreeMap<Identifier, AstExpr>,
    pub rest: Option<AstExpr>,
    pub keyword_rest: Option<AstExpr>,
}

impl ArgumentInvocation {
    pub fn empty() -> Self {
        Self {
            positional: Vec::new(),
            named: BTreeMap::new(),
            rest: None,
            keyword_rest: None,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ArgumentResult {
    pub positional: Vec<Value>,
    pub named: BTreeMap<Identifier, Value>,
    pub separator: ListSeparator,
    pub span: Span,
    // todo: hack
    pub touched: BTreeSet<usize>,
}

impl ArgumentResult {
    pub fn new(span: Span) -> Self {
        // CallArgs(HashMap::new(), span)
        todo!()
    }

    // pub fn to_css_string(self, is_compressed: bool) -> SassResult<Spanned<String>> {
    // let mut string = String::with_capacity(2 + self.len() * 10);
    // string.push('(');
    // let mut span = self.1;

    // if self.is_empty() {
    //     return Ok(Spanned {
    //         node: "()".to_owned(),
    //         span,
    //     });
    // }

    // let args = match self.get_variadic() {
    //     Ok(v) => v,
    //     Err(..) => {
    //         return Err(("Plain CSS functions don't support keyword arguments.", span).into())
    //     }
    // };

    // string.push_str(
    //     &args
    //         .iter()
    //         .map(|a| {
    //             span = span.merge(a.span);
    //             a.node.to_css_string(a.span, is_compressed)
    //         })
    //         .collect::<SassResult<Vec<Cow<'static, str>>>>()?
    //         .join(", "),
    // );
    // string.push(')');
    // Ok(Spanned { node: string, span })
    // todo!()
    // }

    /// Get argument by name
    ///
    /// Removes the argument
    pub fn get_named<T: Into<Identifier>>(&mut self, val: T) -> Option<Spanned<Value>> {
        self.named.remove(&val.into()).map(|n| Spanned {
            node: n,
            span: self.span,
        })
        // self.0.remove(&CallArg::Named(val.into()))
        // todo!()
    }

    /// Get a positional argument by 0-indexed position
    ///
    /// Removes the argument
    pub fn get_positional(&mut self, idx: usize) -> Option<Spanned<Value>> {
        let val = self.positional.get(idx).cloned().map(|n| Spanned {
            node: n,
            span: self.span,
        });
        self.touched.insert(idx);
        val
        // self.0.remove(&CallArg::Positional(val))
        // todo!()
    }

    pub fn get<T: Into<Identifier>>(&mut self, position: usize, name: T) -> Option<Spanned<Value>> {
        match self.get_named(name) {
            Some(v) => Some(v),
            None => self.get_positional(position),
        }
    }

    pub fn get_err(&mut self, position: usize, name: &'static str) -> SassResult<Value> {
        match self.get_named(name) {
            Some(v) => Ok(v.node),
            None => match self.get_positional(position) {
                Some(v) => Ok(v.node),
                None => Err((format!("Missing argument ${}.", name), self.span()).into()),
            },
        }
        // todo!()
    }

    // / Decrement all positional arguments by 1
    // /
    // / This is used by builtin function `call` to pass
    // / positional arguments to the other function
    // pub fn decrement(self) -> CallArgs {
    //     // CallArgs(
    //     //     self.0
    //     //         .into_iter()
    //     //         .map(|(k, v)| (k.decrement(), v))
    //     //         .collect(),
    //     //     self.1,
    //     // )
    //     todo!()
    // }

    pub const fn span(&self) -> Span {
        self.span
    }

    pub fn len(&self) -> usize {
        self.positional.len() + self.named.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn min_args(&self, min: usize) -> SassResult<()> {
        let len = self.len();
        if len < min {
            if min == 1 {
                return Err(("At least one argument must be passed.", self.span()).into());
            }
            todo!("min args greater than one")
        }
        Ok(())
    }

    pub fn max_args(&self, max: usize) -> SassResult<()> {
        let len = self.len();
        if len > max {
            let mut err = String::with_capacity(50);
            #[allow(clippy::format_push_string)]
            err.push_str(&format!("Only {} argument", max));
            if max != 1 {
                err.push('s');
            }
            err.push_str(" allowed, but ");
            err.push_str(&len.to_string());
            err.push(' ');
            if len == 1 {
                err.push_str("was passed.");
            } else {
                err.push_str("were passed.");
            }
            return Err((err, self.span()).into());
        }
        Ok(())
        // todo!()
    }

    pub fn default_arg(&mut self, position: usize, name: &'static str, default: Value) -> Value {
        match self.get(position, name) {
            Some(val) => val.node,
            None => default,
        }
    }

    pub fn positional_arg(&mut self, position: usize) -> Option<Spanned<Value>> {
        self.get_positional(position)
    }

    pub fn default_named_arg(&mut self, name: &'static str, default: Value) -> Value {
        match self.get_named(name) {
            Some(val) => val.node,
            None => default,
        }
    }

    // args: ArgumentDeclaration
    pub fn get_variadic(self) -> SassResult<Vec<Spanned<Value>>> {
        // todo: i think we do give a proper error here
        assert!(self.named.is_empty());

        let Self {
            positional,
            span,
            touched,
            ..
        } = self;

        // todo: complete hack, we shouldn't have the `touched` set
        let mut args = positional
            .into_iter()
            .enumerate()
            .filter(|(idx, _)| !touched.contains(idx))
            .map(|(_, a)| Spanned {
                node: a,
                span: span,
            })
            .collect();

        // let mut vals = Vec::new();
        // let mut args = match self
        //     .0
        //     .into_iter()
        //     .map(|(a, v)| Ok((a.position()?, v)))
        //     .collect::<Result<Vec<(usize, SassResult<Spanned<Value>>)>, String>>()
        // {
        //     Ok(v) => v,
        //     Err(e) => return Err((format!("No argument named ${}.", e), self.1).into()),
        // };

        // args.sort_by(|(a1, _), (a2, _)| a1.cmp(a2));

        // for (_, arg) in args {
        //     vals.push(arg?);
        // }

        // Ok(vals)
        // todo!()
        let span = self.span;

        Ok(args)
        // Ok(args
        //     .into_iter()
        //     .map(|a| Spanned { node: a, span })
        //     .collect())
    }
}

fn is_hex_color(interpolation: &Interpolation) -> bool {
    if let Some(plain) = interpolation.as_plain() {
        if ![3, 4, 6, 8].contains(&plain.len()) {
            return false;
        }

        return plain.chars().all(|c| c.is_ascii_hexdigit());
    }

    false
}

pub(crate) struct ValueParser<'c> {
    comma_expressions: Option<Vec<Spanned<AstExpr>>>,
    space_expressions: Option<Vec<Spanned<AstExpr>>>,
    binary_operators: Option<Vec<BinaryOp>>,
    operands: Option<Vec<Spanned<AstExpr>>>,
    allow_slash: bool,
    single_expression: Option<Spanned<AstExpr>>,
    in_parentheses: bool,
    inside_bracketed_list: bool,
    single_equals: bool,
    parse_until: Option<Predicate<'c>>,
}

impl<'c> ValueParser<'c> {
    pub fn parse_expression(
        parser: &mut Parser,
        parse_until: Option<Predicate<'c>>,
        inside_bracketed_list: bool,
        single_equals: bool,
    ) -> SassResult<Spanned<AstExpr>> {
        let mut value_parser = Self::new(parser, parse_until, inside_bracketed_list, single_equals);

        if let Some(parse_until) = value_parser.parse_until {
            if parse_until(parser) {
                return Err(("Expected expression.", parser.span_before).into());
            }
        }

        let before_bracket = if value_parser.inside_bracketed_list {
            let start = parser.toks.cursor();

            parser.expect_char('[')?;
            parser.whitespace_or_comment();

            if parser.consume_char_if_exists(']') {
                return Ok(AstExpr::List {
                    elems: Vec::new(),
                    separator: ListSeparator::Undecided,
                    brackets: Brackets::Bracketed,
                }
                // todo: lexer.span_from(span)
                .span(parser.span_before));
            }

            Some(start)
        } else {
            None
        };

        value_parser.single_expression = Some(value_parser.parse_single_expression(parser)?);

        value_parser.parse_value(parser)
    }

    pub fn new(
        parser: &mut Parser,
        parse_until: Option<Predicate<'c>>,
        inside_bracketed_list: bool,
        single_equals: bool,
    ) -> Self {
        Self {
            comma_expressions: None,
            space_expressions: None,
            binary_operators: None,
            operands: None,
            allow_slash: true,
            in_parentheses: false,
            single_expression: None,
            parse_until,
            inside_bracketed_list,
            single_equals,
        }
    }

    /// Parse a value from a stream of tokens
    ///
    /// This function will cease parsing if the predicate returns true.
    pub(crate) fn parse_value(&mut self, parser: &mut Parser) -> SassResult<Spanned<AstExpr>> {
        parser.whitespace();

        let span = match parser.toks.peek() {
            Some(Token { pos, .. }) => pos,
            None => return Err(("Expected expression.", parser.span_before).into()),
        };

        let start = parser.toks.cursor();

        let was_in_parens = parser.flags.in_parens();

        loop {
            parser.whitespace_or_comment();

            if let Some(parse_until) = self.parse_until {
                if parse_until(parser) {
                    break;
                }
            }

            let first = parser.toks.peek();

            match first {
                Some(Token { kind: '(', .. }) => {
                    let expr = self.parse_paren_expr(parser)?;
                    self.add_single_expression(expr, parser)?;
                }
                Some(Token { kind: '[', .. }) => {
                    self.add_single_expression(todo!(), parser)?;
                }
                Some(Token { kind: '$', .. }) => {
                    let expr = self.parse_variable(parser)?;
                    self.add_single_expression(expr, parser)?;
                }
                Some(Token { kind: '&', .. }) => {
                    let expr = self.parse_selector(parser)?;
                    self.add_single_expression(expr, parser)?;
                }
                Some(Token { kind: '"', .. }) | Some(Token { kind: '\'', .. }) => {
                    let expr = parser
                        .parse_interpolated_string()?
                        .map_node(AstExpr::String);
                    self.add_single_expression(expr, parser)?;
                }
                Some(Token { kind: '#', .. }) => {
                    let expr = self.parse_hash(parser)?;
                    self.add_single_expression(expr, parser)?;
                }
                Some(Token { kind: '=', .. }) => {
                    parser.toks.next();
                    if self.single_equals
                        && !matches!(parser.toks.peek(), Some(Token { kind: '=', .. }))
                    {
                        self.add_operator(
                            Spanned {
                                node: BinaryOp::SingleEq,
                                span: parser.span_before,
                            },
                            parser,
                        )?;
                    } else {
                        parser.expect_char('=')?;
                        self.add_operator(
                            Spanned {
                                node: BinaryOp::Equal,
                                span: parser.span_before,
                            },
                            parser,
                        )?;
                    }
                }
                Some(Token { kind: '!', .. }) => match parser.toks.peek_n(1) {
                    Some(Token { kind: '=', .. }) => {
                        parser.toks.next();
                        parser.toks.next();
                        self.add_operator(
                            Spanned {
                                node: BinaryOp::NotEqual,
                                span: parser.span_before,
                            },
                            parser,
                        )?;
                    }
                    Some(Token { kind, .. })
                        if kind.is_ascii_whitespace() || kind == 'i' || kind == 'I' =>
                    {
                        let expr = self.parse_important_expr(parser)?;
                        self.add_single_expression(expr, parser)?;
                    }
                    None => {
                        let expr = self.parse_important_expr(parser)?;
                        self.add_single_expression(expr, parser)?;
                    }
                    Some(..) => break,
                },
                Some(Token { kind: '<', .. }) => {
                    parser.toks.next();
                    self.add_operator(
                        Spanned {
                            node: if parser.consume_char_if_exists('=') {
                                BinaryOp::LessThanEqual
                            } else {
                                BinaryOp::LessThan
                            },
                            span: parser.span_before,
                        },
                        parser,
                    )?;
                }
                Some(Token { kind: '>', .. }) => {
                    parser.toks.next();
                    self.add_operator(
                        Spanned {
                            node: if parser.consume_char_if_exists('=') {
                                BinaryOp::GreaterThanEqual
                            } else {
                                BinaryOp::GreaterThan
                            },
                            span: parser.span_before,
                        },
                        parser,
                    )?;
                }
                Some(Token { kind: '*', pos }) => {
                    parser.toks.next();
                    self.add_operator(
                        Spanned {
                            node: BinaryOp::Mul,
                            span: pos,
                        },
                        parser,
                    )?;
                }
                Some(Token { kind: '+', .. }) => {
                    if self.single_expression.is_none() {
                        let expr = self.parse_unary_operation(parser)?;
                        self.add_single_expression(expr, parser)?;
                    } else {
                        parser.toks.next();
                        self.add_operator(
                            Spanned {
                                node: BinaryOp::Plus,
                                span: parser.span_before,
                            },
                            parser,
                        )?;
                    }
                }
                Some(Token { kind: '-', .. }) => {
                    if matches!(
                        parser.toks.peek_n(1),
                        Some(Token {
                            kind: '0'..='9' | '.',
                            ..
                        })
                    ) && (self.single_expression.is_none()
                        || matches!(
                            parser.toks.peek_previous(),
                            Some(Token {
                                kind: ' ' | '\t' | '\n' | '\r',
                                ..
                            })
                        ))
                    {
                        let expr = self.parse_number(parser)?;
                        self.add_single_expression(expr, parser)?;
                    } else if parser.looking_at_interpolated_identifier() {
                        let expr = self.parse_identifier_like(parser)?;
                        self.add_single_expression(expr, parser)?;
                    } else if self.single_expression.is_none() {
                        let expr = self.parse_unary_operation(parser)?;
                        self.add_single_expression(expr, parser)?;
                    } else {
                        parser.toks.next();
                        self.add_operator(
                            Spanned {
                                node: BinaryOp::Minus,
                                span: parser.span_before,
                            },
                            parser,
                        )?;
                    }
                }
                Some(Token { kind: '/', .. }) => {
                    if self.single_expression.is_none() {
                        let expr = self.parse_unary_operation(parser)?;
                        self.add_single_expression(expr, parser)?;
                    } else {
                        parser.toks.next();
                        self.add_operator(
                            Spanned {
                                node: BinaryOp::Div,
                                span: parser.span_before,
                            },
                            parser,
                        )?;
                    }
                }
                Some(Token { kind: '%', pos }) => {
                    parser.toks.next();
                    self.add_operator(
                        Spanned {
                            node: BinaryOp::Rem,
                            span: pos,
                        },
                        parser,
                    )?;
                }
                Some(Token {
                    kind: '0'..='9', ..
                }) => {
                    let expr = self.parse_number(parser)?;
                    self.add_single_expression(expr, parser)?;
                }
                Some(Token { kind: '.', .. }) => {
                    if matches!(parser.toks.peek_n(1), Some(Token { kind: '.', .. })) {
                        break;
                    }
                    let expr = self.parse_number(parser)?;
                    self.add_single_expression(expr, parser)?;
                }
                Some(Token { kind: 'a', .. }) => {
                    if !parser.flags.in_plain_css() && parser.scan_identifier("and", false) {
                        self.add_operator(
                            Spanned {
                                node: BinaryOp::And,
                                span: parser.span_before,
                            },
                            parser,
                        )?;
                    } else {
                        let expr = self.parse_identifier_like(parser)?;
                        self.add_single_expression(expr, parser)?;
                    }
                }
                Some(Token { kind: 'o', .. }) => {
                    if !parser.flags.in_plain_css() && parser.scan_identifier("or", false) {
                        self.add_operator(
                            Spanned {
                                node: BinaryOp::Or,
                                span: parser.span_before,
                            },
                            parser,
                        )?;
                    } else {
                        let expr = self.parse_identifier_like(parser)?;
                        self.add_single_expression(expr, parser)?;
                    }
                }
                Some(Token { kind: 'u', .. }) | Some(Token { kind: 'U', .. }) => {
                    if matches!(parser.toks.peek_n(1), Some(Token { kind: '+', .. })) {
                        let expr = self.parse_unicode_range(parser)?;
                        self.add_single_expression(expr, parser)?;
                    } else {
                        let expr = self.parse_identifier_like(parser)?;
                        self.add_single_expression(expr, parser)?;
                    }
                }
                Some(Token {
                    kind: 'b'..='z', ..
                })
                | Some(Token {
                    kind: 'B'..='Z', ..
                })
                | Some(Token { kind: '_', .. })
                | Some(Token { kind: '\\', .. })
                | Some(Token {
                    kind: '\u{80}'..=std::char::MAX,
                    ..
                }) => {
                    let expr = self.parse_identifier_like(parser)?;
                    self.add_single_expression(expr, parser)?;
                }
                Some(Token { kind: ',', .. }) => {
                    // If we discover we're parsing a list whose first element is a
                    // division operation, and we're in parentheses, reparse outside of a
                    // paren context. This ensures that `(1/2, 1)` doesn't perform division
                    // on its first element.
                    if parser.flags.in_parens() {
                        parser.flags.set(ContextFlags::IN_PARENS, false);
                        if self.allow_slash {
                            self.reset_state(parser);
                            continue;
                        }
                    }

                    if self.single_expression.is_none() {
                        todo!("Expected expression.")
                    }

                    self.resolve_space_expressions(parser)?;

                    // [resolveSpaceExpressions can modify [singleExpression_], but it
                    // can't set it to null`.
                    self.comma_expressions
                        .get_or_insert_with(Default::default)
                        .push(self.single_expression.take().unwrap());
                    parser.toks.next();
                    self.allow_slash = true;
                }
                Some(..) | None => break,
            }
        }

        if self.inside_bracketed_list {
            parser.expect_char(']')?;
        }

        if self.comma_expressions.is_some() {
            self.resolve_space_expressions(parser)?;

            self.in_parentheses = was_in_parens;

            if let Some(single_expression) = self.single_expression.take() {
                self.comma_expressions
                    .as_mut()
                    .unwrap()
                    .push(single_expression);
            }

            return Ok(AstExpr::List {
                elems: self.comma_expressions.take().unwrap(),
                separator: ListSeparator::Comma,
                brackets: if self.inside_bracketed_list {
                    Brackets::Bracketed
                } else {
                    Brackets::None
                },
            }
            .span(span));
        } else if self.inside_bracketed_list && self.space_expressions.is_some() {
            self.resolve_operations()?;

            self.space_expressions
                .as_mut()
                .unwrap()
                .push(self.single_expression.take().unwrap());

            return Ok(AstExpr::List {
                elems: self.space_expressions.take().unwrap(),
                separator: ListSeparator::Space,
                brackets: Brackets::Bracketed,
            }
            .span(span));
        } else {
            self.resolve_space_expressions(parser)?;

            if self.inside_bracketed_list {
                return Ok(AstExpr::List {
                    elems: vec![self.single_expression.take().unwrap()],
                    separator: ListSeparator::Undecided,
                    brackets: Brackets::Bracketed,
                }
                .span(span));
            }

            return Ok(self.single_expression.take().unwrap());
        }
    }

    fn parse_single_expression(&mut self, parser: &mut Parser) -> SassResult<Spanned<AstExpr>> {
        let first = parser.toks.peek();

        match first {
            Some(Token { kind: '(', .. }) => self.parse_paren_expr(parser),
            Some(Token { kind: '/', .. }) => self.parse_unary_operation(parser),
            Some(Token { kind: '[', .. }) => Self::parse_expression(parser, None, true, false),
            Some(Token { kind: '$', .. }) => self.parse_variable(parser),
            Some(Token { kind: '&', .. }) => self.parse_selector(parser),
            Some(Token { kind: '"', .. }) | Some(Token { kind: '\'', .. }) => Ok(parser
                .parse_interpolated_string()?
                .map_node(AstExpr::String)),
            Some(Token { kind: '#', .. }) => self.parse_hash(parser),
            Some(Token { kind: '+', .. }) => self.parse_plus_expr(parser),
            Some(Token { kind: '-', .. }) => self.parse_minus_expr(parser),
            Some(Token { kind: '!', .. }) => self.parse_important_expr(parser),
            Some(Token { kind: 'u', .. }) | Some(Token { kind: 'U', .. }) => {
                if matches!(parser.toks.peek_n(1), Some(Token { kind: '+', .. })) {
                    self.parse_unicode_range(parser)
                } else {
                    self.parse_identifier_like(parser)
                }
            }
            Some(Token {
                kind: '0'..='9', ..
            })
            | Some(Token { kind: '.', .. }) => self.parse_number(parser),
            Some(Token {
                kind: 'a'..='z', ..
            })
            | Some(Token {
                kind: 'A'..='Z', ..
            })
            | Some(Token { kind: '_', .. })
            | Some(Token { kind: '\\', .. })
            | Some(Token {
                kind: '\u{80}'..=std::char::MAX,
                ..
            }) => self.parse_identifier_like(parser),
            Some(..) | None => {
                return Err(("Expected expression.", parser.toks.current_span()).into())
            }
        }
    }

    fn resolve_one_operation(&mut self) -> SassResult<()> {
        let operator = self.binary_operators.as_mut().unwrap().pop().unwrap();
        let operands = self.operands.as_mut().unwrap();

        let left = operands.pop().unwrap();
        let right = match self.single_expression.take() {
            Some(val) => val,
            None => return Err(("Expected expression.", left.span).into()),
        };

        let span = left.span.merge(right.span);

        if self.allow_slash
            && !self.in_parentheses
            && operator == BinaryOp::Div
            && left.node.is_slash_operand()
            && right.node.is_slash_operand()
        {
            self.single_expression = Some(AstExpr::slash(left.node, right.node).span(span))
        } else {
            self.single_expression = Some(
                AstExpr::BinaryOp {
                    lhs: Box::new(left.node),
                    op: operator,
                    rhs: Box::new(right.node),
                    allows_slash: false,
                }
                .span(span),
            );
            self.allow_slash = false
        }

        Ok(())
    }

    fn resolve_operations(&mut self) -> SassResult<()> {
        loop {
            let should_break = match self.binary_operators.as_ref() {
                Some(bin) => bin.is_empty(),
                None => true,
            };

            if should_break {
                break;
            }

            self.resolve_one_operation()?;
        }

        Ok(())
    }

    fn add_single_expression(
        &mut self,
        expression: Spanned<AstExpr>,
        parser: &mut Parser,
    ) -> SassResult<()> {
        if self.single_expression.is_some() {
            if self.in_parentheses {
                self.in_parentheses = false;

                if self.allow_slash {
                    self.reset_state(parser)?;

                    return Ok(());
                }
            }

            if self.space_expressions.is_none() {
                self.space_expressions = Some(Vec::new());
            }

            self.resolve_operations()?;

            self.space_expressions
                .as_mut()
                .unwrap()
                .push(self.single_expression.take().unwrap());

            self.allow_slash = true;
        }

        self.single_expression = Some(expression);

        Ok(())
    }

    fn add_operator(&mut self, op: Spanned<BinaryOp>, parser: &mut Parser) -> SassResult<()> {
        if parser.flags.in_plain_css() && op.node != BinaryOp::Div && op.node != BinaryOp::SingleEq
        {
            return Err(("Operators aren't allowed in plain CSS.", op.span).into());
        }

        self.allow_slash = self.allow_slash && op.node == BinaryOp::Div;

        if self.binary_operators.is_none() {
            self.binary_operators = Some(Vec::new());
        }

        if self.operands.is_none() {
            self.operands = Some(Vec::new());
        }

        while let Some(last_op) = self.binary_operators.as_ref().unwrap_or(&Vec::new()).last() {
            if last_op.precedence() < op.precedence() {
                break;
            }

            self.resolve_one_operation()?;
        }
        self.binary_operators
            .get_or_insert_with(Default::default)
            .push(op.node);

        match self.single_expression.take() {
            Some(expr) => {
                self.operands.get_or_insert_with(Vec::new).push(expr);
            }
            None => return Err(("Expected expression.", op.span).into()),
        }

        parser.whitespace_or_comment();

        self.single_expression = Some(self.parse_single_expression(parser)?);

        Ok(())
    }

    fn resolve_space_expressions(&mut self, parser: &mut Parser) -> SassResult<()> {
        self.resolve_operations()?;

        if let Some(mut space_expressions) = self.space_expressions.take() {
            let single_expression = match self.single_expression.take() {
                Some(val) => val,
                None => return Err(("Expected expression.", parser.toks.current_span()).into()),
            };

            let span = single_expression.span;

            space_expressions.push(single_expression);

            self.single_expression = Some(
                AstExpr::List {
                    elems: space_expressions,
                    separator: ListSeparator::Space,
                    brackets: Brackets::None,
                }
                .span(span),
            );
        }

        Ok(())
    }

    fn parse_map(
        &mut self,
        parser: &mut Parser,
        first: Spanned<AstExpr>,
    ) -> SassResult<Spanned<AstExpr>> {
        let mut pairs = vec![(first.node, parser.parse_expression_until_comma(false)?.node)];

        while parser.consume_char_if_exists(',') {
            parser.whitespace_or_comment();
            if !parser.looking_at_expression() {
                break;
            }

            let key = parser.parse_expression_until_comma(false)?;
            parser.expect_char(':')?;
            parser.whitespace_or_comment();
            let value = parser.parse_expression_until_comma(false)?;
            pairs.push((key.node, value.node));
        }

        parser.expect_char(')')?;

        Ok(AstExpr::Map(AstSassMap(pairs)).span(parser.span_before))
    }

    fn parse_paren_expr(&mut self, parser: &mut Parser) -> SassResult<Spanned<AstExpr>> {
        if parser.flags.in_plain_css() {
            todo!("Parentheses aren't allowed in plain CSS.")
        }

        let was_in_parentheses = parser.flags.in_parens();
        parser.flags.set(ContextFlags::IN_PARENS, true);

        parser.expect_char('(')?;
        parser.whitespace_or_comment();
        if !parser.looking_at_expression() {
            parser.expect_char(')')?;
            return Ok(AstExpr::List {
                elems: Vec::new(),
                separator: ListSeparator::Undecided,
                brackets: Brackets::None,
            }
            .span(parser.span_before));
        }

        let first = parser.parse_expression_until_comma(false)?;
        if parser.consume_char_if_exists(':') {
            parser.whitespace_or_comment();
            parser
                .flags
                .set(ContextFlags::IN_PARENS, was_in_parentheses);
            return self.parse_map(parser, first);
        }

        if !parser.consume_char_if_exists(',') {
            parser.expect_char(')')?;
            parser
                .flags
                .set(ContextFlags::IN_PARENS, was_in_parentheses);
            return Ok(AstExpr::Paren(Box::new(first.node)).span(first.span));
        }

        parser.whitespace_or_comment();

        let mut expressions = vec![first];

        loop {
            if !parser.looking_at_expression() {
                break;
            }
            expressions.push(parser.parse_expression_until_comma(false)?);
            if !parser.consume_char_if_exists(',') {
                break;
            }
            parser.whitespace_or_comment();
        }

        parser.expect_char(')')?;

        parser
            .flags
            .set(ContextFlags::IN_PARENS, was_in_parentheses);

        Ok(AstExpr::List {
            elems: expressions,
            separator: ListSeparator::Comma,
            brackets: Brackets::None,
        }
        .span(parser.span_before))
    }

    fn parse_variable(&mut self, parser: &mut Parser) -> SassResult<Spanned<AstExpr>> {
        let name = parser.parse_variable_name()?;

        if parser.flags.in_plain_css() {
            todo!("Sass variables aren't allowed in plain CSS.")
        }

        Ok(AstExpr::Variable {
            name: Identifier::from(name),
            namespace: None,
        }
        .span(parser.span_before))
    }

    fn parse_selector(&mut self, parser: &mut Parser) -> SassResult<Spanned<AstExpr>> {
        if parser.flags.in_plain_css() {
            todo!("The parent selector isn't allowed in plain CSS.")
        }

        parser.expect_char('&')?;

        if parser.toks.next_char_is('&') {
            //   warn(
            //       'In Sass, "&&" means two copies of the parent selector. You '
            //       'probably want to use "and" instead.',
            //       scanner.spanFrom(start));

            todo!()
        }

        Ok(AstExpr::ParentSelector.span(parser.span_before))
        //     if (plainCss) {
        //   scanner.error("The parent selector isn't allowed in plain CSS.",
        //       length: 1);
        // }

        // var start = scanner.state;
        // scanner.expectChar($ampersand);

        // if (scanner.scanChar($ampersand)) {
        //   scanner.position--;
        // }

        // return SelectorExpression(scanner.spanFrom(start));
        // todo!()
    }

    fn parse_hash(&mut self, parser: &mut Parser) -> SassResult<Spanned<AstExpr>> {
        debug_assert!(matches!(parser.toks.peek(), Some(Token { kind: '#', .. })));

        if matches!(parser.toks.peek_n(1), Some(Token { kind: '{', .. })) {
            return self.parse_identifier_like(parser);
        }

        parser.expect_char('#')?;

        if matches!(
            parser.toks.peek(),
            Some(Token {
                kind: '0'..='9',
                ..
            })
        ) {
            let color = self.parse_hex_color_contents(parser)?;
            return Ok(AstExpr::Color(Box::new(color)).span(parser.span_before));
        }

        let after_hash = parser.toks.cursor();
        let ident = parser.parse_interpolated_identifier()?;
        if is_hex_color(&ident) {
            parser.toks.set_cursor(after_hash);
            let color = self.parse_hex_color_contents(parser)?;
            return Ok(AstExpr::Color(Box::new(color)).span(parser.span_before));
        }

        let mut buffer = Interpolation::new(parser.span_before);

        buffer.add_token(Token {
            kind: '#',
            pos: parser.span_before,
        });
        buffer.add_interpolation(ident);

        Ok(AstExpr::String(StringExpr(buffer, QuoteKind::None)).span(parser.span_before))

        //     assert(scanner.peekChar() == $hash);
        // if (scanner.peekChar(1) == $lbrace) return identifierLike();

        // var start = scanner.state;
        // scanner.expectChar($hash);

        // var first = scanner.peekChar();
        // if (first != null && isDigit(first)) {
        //   return ColorExpression(_hexColorContents(start), scanner.spanFrom(start));
        // }

        // var afterHash = scanner.state;
        // var identifier = interpolatedIdentifier();
        // if (_isHexColor(identifier)) {
        //   scanner.state = afterHash;
        //   return ColorExpression(_hexColorContents(start), scanner.spanFrom(start));
        // }

        // var buffer = InterpolationBuffer();
        // buffer.writeCharCode($hash);
        // buffer.addInterpolation(identifier);
        // return StringExpression(buffer.interpolation(scanner.spanFrom(start)));
        // todo!()
    }

    fn parse_hex_digit(&mut self, parser: &mut Parser) -> SassResult<u32> {
        match parser.toks.peek() {
            Some(Token { kind, .. }) if kind.is_ascii_hexdigit() => {
                parser.toks.next();
                Ok(as_hex(kind))
            }
            _ => todo!("Expected hex digit."),
        }
    }

    fn parse_hex_color_contents(&mut self, parser: &mut Parser) -> SassResult<Color> {
        let start = parser.toks.cursor();

        let digit1 = self.parse_hex_digit(parser)?;
        let digit2 = self.parse_hex_digit(parser)?;
        let digit3 = self.parse_hex_digit(parser)?;

        let red: u32;
        let green: u32;
        let blue: u32;
        let mut alpha: f64 = 1.0;

        if !parser.next_is_hex() {
            // #abc
            red = (digit1 << 4) + digit1;
            green = (digit2 << 4) + digit2;
            blue = (digit3 << 4) + digit3;
        } else {
            let digit4 = self.parse_hex_digit(parser)?;

            if !parser.next_is_hex() {
                // #abcd
                red = (digit1 << 4) + digit1;
                green = (digit2 << 4) + digit2;
                blue = (digit3 << 4) + digit3;
                alpha = ((digit4 << 4) + digit4) as f64 / 0xff as f64;
            } else {
                red = (digit1 << 4) + digit2;
                green = (digit3 << 4) + digit4;
                blue = (self.parse_hex_digit(parser)? << 4) + self.parse_hex_digit(parser)?;

                if parser.next_is_hex() {
                    alpha = ((self.parse_hex_digit(parser)? << 4) + self.parse_hex_digit(parser)?)
                        as f64
                        / 0xff as f64;
                }
            }
        }

        Ok(Color::new_rgba(
            Number::from(red),
            Number::from(green),
            Number::from(blue),
            Number::from(alpha),
            // todo:
            //     // Don't emit four- or eight-digit hex colors as hex, since that's not
            //     // yet well-supported in browsers.
            parser.toks.raw_text(start - 1),
        ))
    }

    fn parse_unary_operation(&mut self, parser: &mut Parser) -> SassResult<Spanned<AstExpr>> {
        let operator = self.expect_unary_operator(parser)?;

        if parser.flags.in_plain_css() && operator != UnaryOp::Div {
            todo!("Operators aren't allowed in plain CSS.");
        }

        parser.whitespace_or_comment();

        let operand = self.parse_single_expression(parser)?;

        Ok(AstExpr::UnaryOp(operator, Box::new(operand.node)).span(parser.span_before))
    }

    fn expect_unary_operator(&mut self, parser: &mut Parser) -> SassResult<UnaryOp> {
        Ok(match parser.toks.next() {
            Some(Token { kind: '+', .. }) => UnaryOp::Plus,
            Some(Token { kind: '-', .. }) => UnaryOp::Neg,
            Some(Token { kind: '/', .. }) => UnaryOp::Div,
            _ => todo!("Expected unary operator."),
        })
    }

    fn parse_number(&mut self, parser: &mut Parser) -> SassResult<Spanned<AstExpr>> {
        let mut number = String::new();

        if !parser.consume_char_if_exists('+') && parser.consume_char_if_exists('-') {
            number.push('-')
        }

        number.push_str(&parser.parse_whole_number());

        if let Some(dec) = self.try_decimal(parser, !number.is_empty())? {
            number.push_str(&dec);
        }

        if let Some(exp) = self.try_exponent(parser)? {
            number.push_str(&exp);
        }

        let number: f64 = number.parse().unwrap();

        let unit = if parser.consume_char_if_exists('%') {
            Unit::Percent
        } else if parser.looking_at_identifier()
            && (!matches!(parser.toks.peek(), Some(Token { kind: '-', .. }))
                || !matches!(parser.toks.peek_n(1), Some(Token { kind: '-', .. })))
        {
            Unit::from(parser.__parse_identifier(false, true)?)
        } else {
            Unit::None
        };

        Ok(AstExpr::Number {
            n: Number::from(number),
            unit,
        }
        .span(parser.span_before))
    }

    fn try_decimal(
        &mut self,
        parser: &mut Parser,
        allow_trailing_dot: bool,
    ) -> SassResult<Option<String>> {
        if !matches!(parser.toks.peek(), Some(Token { kind: '.', .. })) {
            return Ok(None);
        }

        if let Some(Token { kind, .. }) = parser.toks.peek_n(1) {
            if !kind.is_ascii_digit() {
                if allow_trailing_dot {
                    return Ok(None);
                }
                todo!("Expected digit.")
            }
        }

        let mut buffer = String::new();

        parser.expect_char('.')?;
        buffer.push('.');

        while let Some(Token { kind, .. }) = parser.toks.peek() {
            if !kind.is_ascii_digit() {
                break;
            }
            buffer.push(kind);
            parser.toks.next();
        }

        Ok(Some(buffer))
    }

    fn try_exponent(&mut self, parser: &mut Parser) -> SassResult<Option<String>> {
        let mut buffer = String::new();

        match parser.toks.peek() {
            Some(Token {
                kind: 'e' | 'E', ..
            }) => buffer.push('e'),
            _ => return Ok(None),
        }

        let next = match parser.toks.peek_n(1) {
            Some(Token {
                kind: kind @ ('0'..='9' | '-' | '+'),
                ..
            }) => kind,
            _ => return Ok(None),
        };

        parser.toks.next();

        if next == '+' || next == '-' {
            parser.toks.next();
            buffer.push(next);
        }

        match parser.toks.peek() {
            Some(Token {
                kind: '0'..='9', ..
            }) => {}
            _ => todo!("Expected digit."),
        }

        while let Some(tok) = parser.toks.peek() {
            if !tok.kind.is_ascii_digit() {
                break;
            }

            buffer.push(tok.kind);

            parser.toks.next();
        }

        Ok(Some(buffer))
    }

    fn parse_plus_expr(&mut self, parser: &mut Parser) -> SassResult<Spanned<AstExpr>> {
        debug_assert!(matches!(parser.toks.peek(), Some(Token { kind: '+', .. })));
        match parser.toks.peek_n(1) {
            Some(Token {
                kind: '0'..='9' | '.',
                ..
            }) => self.parse_number(parser),
            _ => self.parse_unary_operation(parser),
        }
    }

    // todo: i bet we can make minus expr crash somehow
    fn parse_minus_expr(&mut self, parser: &mut Parser) -> SassResult<Spanned<AstExpr>> {
        assert!(matches!(parser.toks.peek(), Some(Token { kind: '-', .. })));

        if matches!(
            parser.toks.peek_n(1),
            Some(Token {
                kind: '0'..='9' | '.',
                ..
            })
        ) {
            return self.parse_number(parser);
        }

        if parser.looking_at_interpolated_identifier() {
            return self.parse_identifier_like(parser);
        }

        self.parse_unary_operation(parser)
    }

    fn parse_important_expr(&mut self, parser: &mut Parser) -> SassResult<Spanned<AstExpr>> {
        parser.expect_char('!')?;
        parser.whitespace_or_comment();
        parser.expect_identifier("important", true)?;

        Ok(AstExpr::String(StringExpr(
            Interpolation::new_plain("!important".to_owned(), parser.span_before),
            QuoteKind::None,
        ))
        .span(parser.span_before))
    }

    fn parse_identifier_like(&mut self, parser: &mut Parser) -> SassResult<Spanned<AstExpr>> {
        let start = parser.toks.cursor();

        let identifier = parser.parse_interpolated_identifier()?;

        let plain = identifier.as_plain();
        let lower = plain.map(|s| s.to_ascii_lowercase());

        if let Some(plain) = plain {
            if plain == "if" && parser.toks.next_char_is('(') {
                let call_args = parser.parse_argument_invocation(false, false)?;
                return Ok(AstExpr::If(Box::new(Ternary(call_args.node))).span(call_args.span));
            } else if plain == "not" {
                parser.whitespace_or_comment();

                let value = self.parse_single_expression(parser)?;

                return Ok(
                    AstExpr::UnaryOp(UnaryOp::Not, Box::new(value.node)).span(parser.span_before)
                );
            }

            let lower_ref = lower.as_ref().unwrap();

            if !parser.toks.next_char_is('(') {
                match plain {
                    "null" => return Ok(AstExpr::Null.span(parser.span_before)),
                    "true" => return Ok(AstExpr::True.span(parser.span_before)),
                    "false" => return Ok(AstExpr::False.span(parser.span_before)),
                    _ => {}
                }

                if let Some(color) = NAMED_COLORS.get_by_name(lower_ref.as_str()) {
                    return Ok(AstExpr::Color(Box::new(Color::new(
                        color[0],
                        color[1],
                        color[2],
                        color[3],
                        plain.to_owned(),
                    )))
                    .span(parser.span_before));
                }

                if let Some(func) = self.try_parse_special_function(parser, lower_ref)? {
                    return Ok(func);
                }
            }
        }

        match parser.toks.peek() {
            Some(Token { kind: '.', .. }) => {
                if matches!(parser.toks.peek_n(1), Some(Token { kind: '.', .. })) {
                    return Ok(AstExpr::String(StringExpr(identifier, QuoteKind::None))
                        .span(parser.span_before));
                }
                parser.toks.next();

                match plain {
                    Some(s) => return self.namespaced_expression(s),
                    None => todo!("Interpolation isn't allowed in namespaces."),
                }
            }
            Some(Token { kind: '(', .. }) => {
                if let Some(plain) = plain {
                    let arguments =
                        parser.parse_argument_invocation(false, lower.as_deref() == Some("var"))?;

                    Ok(AstExpr::FunctionCall {
                        namespace: None,
                        name: Identifier::from(plain),
                        arguments: Box::new(arguments.node),
                    }
                    .span(parser.span_before))
                } else {
                    let arguments = parser.parse_argument_invocation(false, false)?;
                    Ok(AstExpr::InterpolatedFunction {
                        name: identifier,
                        arguments: Box::new(arguments.node),
                    }
                    .span(parser.span_before))
                }
            }
            _ => {
                Ok(AstExpr::String(StringExpr(identifier, QuoteKind::None))
                    .span(parser.span_before))
            }
        }
    }

    fn namespaced_expression(&mut self, namespace: &str) -> SassResult<Spanned<AstExpr>> {
        todo!()
    }

    fn parse_unicode_range(&mut self, parser: &mut Parser) -> SassResult<Spanned<AstExpr>> {
        todo!()
    }

    fn try_parse_special_function(
        &mut self,
        parser: &mut Parser,
        name: &str,
    ) -> SassResult<Option<Spanned<AstExpr>>> {
        if matches!(parser.toks.peek(), Some(Token { kind: '(', .. })) {
            if let Some(calculation) = self.try_parse_calculation(parser, name)? {
                return Ok(Some(calculation));
            }
        }

        let normalized = unvendor(name);

        let mut buffer = Interpolation::new(parser.span_before);

        match normalized {
            "calc" | "element" | "expression" => {
                //     if (!scanner.scanChar($lparen)) return null;
                //     buffer = InterpolationBuffer()
                //       ..write(name)
                //       ..writeCharCode($lparen);

                todo!()
            }
            "progid" => {
                //     if (!scanner.scanChar($colon)) return null;
                //     buffer = InterpolationBuffer()
                //       ..write(name)
                //       ..writeCharCode($colon);
                //     var next = scanner.peekChar();
                //     while (next != null && (isAlphabetic(next) || next == $dot)) {
                //       buffer.writeCharCode(scanner.readChar());
                //       next = scanner.peekChar();
                //     }
                //     scanner.expectChar($lparen);
                //     buffer.writeCharCode($lparen);

                todo!()
            }
            "url" => {
                //     return _tryUrlContents(start)
                //         .andThen((contents) => StringExpression(contents));

                todo!()
            }
            _ => return Ok(None),
        }

        buffer.add_interpolation(parser.parse_interpolated_declaration_value(false, true, true)?);
        parser.expect_char(')')?;
        buffer.add_token(Token {
            kind: '(',
            pos: parser.span_before,
        });

        Ok(Some(
            AstExpr::String(StringExpr(buffer, QuoteKind::None)).span(parser.span_before),
        ))
    }

    fn try_parse_calculation(
        &mut self,
        parser: &mut Parser,
        name: &str,
    ) -> SassResult<Option<Spanned<AstExpr>>> {
        //      assert(scanner.peekChar() == $lparen);
        // switch (name) {
        //   case "calc":
        //     var arguments = _calculationArguments(1);
        //     return CalculationExpression(name, arguments, scanner.spanFrom(start));

        //   case "min":
        //   case "max":
        //     // min() and max() are parsed as calculations if possible, and otherwise
        //     // are parsed as normal Sass functions.
        //     var beforeArguments = scanner.state;
        //     List<Expression> arguments;
        //     try {
        //       arguments = _calculationArguments();
        //     } on FormatException catch (_) {
        //       scanner.state = beforeArguments;
        //       return null;
        //     }

        //     return CalculationExpression(name, arguments, scanner.spanFrom(start));

        //   case "clamp":
        //     var arguments = _calculationArguments(3);
        //     return CalculationExpression(name, arguments, scanner.spanFrom(start));

        //   default:
        //     return null;
        // }
        todo!()
    }

    fn reset_state(&mut self, parser: &mut Parser) -> SassResult<()> {
        self.comma_expressions = None;
        self.space_expressions = None;
        self.binary_operators = None;
        self.operands = None;
        self.allow_slash = true;
        self.single_expression = Some(self.parse_single_expression(parser)?);

        Ok(())
    }
}
