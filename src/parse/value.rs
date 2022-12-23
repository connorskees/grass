use std::iter::Iterator;

use codemap::Spanned;

use crate::{
    ast::*,
    color::{Color, ColorFormat, NAMED_COLORS},
    common::{unvendor, BinaryOp, Brackets, Identifier, ListSeparator, QuoteKind, UnaryOp},
    error::SassResult,
    unit::Unit,
    utils::{as_hex, opposite_bracket},
    value::{CalculationName, Number},
    ContextFlags, Token,
};

use super::Parser;

pub(crate) type Predicate<'a> = &'a dyn Fn(&mut Parser<'_, '_>) -> SassResult<bool>;

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
    start: usize,
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
        let start = parser.toks.cursor();
        let mut value_parser = Self::new(parser, parse_until, inside_bracketed_list, single_equals);

        if let Some(parse_until) = value_parser.parse_until {
            if parse_until(parser)? {
                return Err(("Expected expression.", parser.toks.current_span()).into());
            }
        }

        let before_bracket = if value_parser.inside_bracketed_list {
            let start = parser.toks.cursor();

            parser.expect_char('[')?;
            parser.whitespace()?;

            if parser.scan_char(']') {
                return Ok(AstExpr::List(ListExpr {
                    elems: Vec::new(),
                    separator: ListSeparator::Undecided,
                    brackets: Brackets::Bracketed,
                })
                .span(parser.toks.span_from(start)));
            }

            Some(start)
        } else {
            None
        };

        value_parser.single_expression = Some(value_parser.parse_single_expression(parser)?);

        let mut value = value_parser.parse_value(parser)?;
        value.span = parser.toks.span_from(start);

        Ok(value)
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
            start: parser.toks.cursor(),
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
        parser.whitespace()?;

        let start = parser.toks.cursor();

        let was_in_parens = parser.flags.in_parens();

        loop {
            parser.whitespace()?;

            if let Some(parse_until) = self.parse_until {
                if parse_until(parser)? {
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
                    let expr = parser.parse_expression(None, Some(true), None)?;
                    self.add_single_expression(expr, parser)?;
                }
                Some(Token { kind: '$', .. }) => {
                    let expr = Self::parse_variable(parser)?;
                    self.add_single_expression(expr, parser)?;
                }
                Some(Token { kind: '&', .. }) => {
                    let expr = Self::parse_selector(parser)?;
                    self.add_single_expression(expr, parser)?;
                }
                Some(Token { kind: '"', .. }) | Some(Token { kind: '\'', .. }) => {
                    let expr = parser
                        .parse_interpolated_string()?
                        .map_node(|s| AstExpr::String(s, parser.toks.span_from(start)));
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
                        let expr = Self::parse_important_expr(parser)?;
                        self.add_single_expression(expr, parser)?;
                    }
                    None => {
                        let expr = Self::parse_important_expr(parser)?;
                        self.add_single_expression(expr, parser)?;
                    }
                    Some(..) => break,
                },
                Some(Token { kind: '<', .. }) => {
                    parser.toks.next();
                    self.add_operator(
                        Spanned {
                            node: if parser.scan_char('=') {
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
                            node: if parser.scan_char('=') {
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
                    if !parser.is_plain_css && parser.scan_identifier("and", false)? {
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
                    if !parser.is_plain_css && parser.scan_identifier("or", false)? {
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
                        let expr = Self::parse_unicode_range(parser)?;
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
                    kind: 'A'..='Z', ..
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
                            self.reset_state(parser)?;
                            continue;
                        }
                    }

                    if self.single_expression.is_none() {
                        return Err(("Expected expression.", parser.toks.current_span()).into());
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

            parser.flags.set(ContextFlags::IN_PARENS, was_in_parens);

            if let Some(single_expression) = self.single_expression.take() {
                self.comma_expressions
                    .as_mut()
                    .unwrap()
                    .push(single_expression);
            }

            Ok(AstExpr::List(ListExpr {
                elems: self.comma_expressions.take().unwrap(),
                separator: ListSeparator::Comma,
                brackets: if self.inside_bracketed_list {
                    Brackets::Bracketed
                } else {
                    Brackets::None
                },
            })
            .span(parser.span_before))
        } else if self.inside_bracketed_list && self.space_expressions.is_some() {
            self.resolve_operations(parser)?;

            self.space_expressions
                .as_mut()
                .unwrap()
                .push(self.single_expression.take().unwrap());

            Ok(AstExpr::List(ListExpr {
                elems: self.space_expressions.take().unwrap(),
                separator: ListSeparator::Space,
                brackets: Brackets::Bracketed,
            })
            .span(parser.span_before))
        } else {
            self.resolve_space_expressions(parser)?;

            if self.inside_bracketed_list {
                return Ok(AstExpr::List(ListExpr {
                    elems: vec![self.single_expression.take().unwrap()],
                    separator: ListSeparator::Undecided,
                    brackets: Brackets::Bracketed,
                })
                .span(parser.span_before));
            }

            Ok(self.single_expression.take().unwrap())
        }
    }

    fn parse_single_expression(&mut self, parser: &mut Parser) -> SassResult<Spanned<AstExpr>> {
        let start = parser.toks.cursor();
        let first = parser.toks.peek();

        match first {
            Some(Token { kind: '(', .. }) => self.parse_paren_expr(parser),
            Some(Token { kind: '/', .. }) => self.parse_unary_operation(parser),
            Some(Token { kind: '[', .. }) => Self::parse_expression(parser, None, true, false),
            Some(Token { kind: '$', .. }) => Self::parse_variable(parser),
            Some(Token { kind: '&', .. }) => Self::parse_selector(parser),
            Some(Token { kind: '"', .. }) | Some(Token { kind: '\'', .. }) => Ok(parser
                .parse_interpolated_string()?
                .map_node(|s| AstExpr::String(s, parser.toks.span_from(start)))),
            Some(Token { kind: '#', .. }) => self.parse_hash(parser),
            Some(Token { kind: '+', .. }) => self.parse_plus_expr(parser),
            Some(Token { kind: '-', .. }) => self.parse_minus_expr(parser),
            Some(Token { kind: '!', .. }) => Self::parse_important_expr(parser),
            Some(Token { kind: 'u', .. }) | Some(Token { kind: 'U', .. }) => {
                if matches!(parser.toks.peek_n(1), Some(Token { kind: '+', .. })) {
                    Self::parse_unicode_range(parser)
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
            Some(..) | None => Err(("Expected expression.", parser.toks.current_span()).into()),
        }
    }

    fn resolve_one_operation(&mut self, parser: &mut Parser) -> SassResult<()> {
        let operator = self.binary_operators.as_mut().unwrap().pop().unwrap();
        let operands = self.operands.as_mut().unwrap();

        let left = operands.pop().unwrap();
        let right = match self.single_expression.take() {
            Some(val) => val,
            None => return Err(("Expected expression.", left.span).into()),
        };

        let span = left.span.merge(right.span);

        if self.allow_slash
            && !parser.flags.in_parens()
            && operator == BinaryOp::Div
            && left.node.is_slash_operand()
            && right.node.is_slash_operand()
        {
            self.single_expression = Some(AstExpr::slash(left.node, right.node, span).span(span));
        } else {
            self.single_expression = Some(
                AstExpr::BinaryOp {
                    lhs: Box::new(left.node),
                    op: operator,
                    rhs: Box::new(right.node),
                    allows_slash: false,
                    span,
                }
                .span(span),
            );
            self.allow_slash = false;
        }

        Ok(())
    }

    fn resolve_operations(&mut self, parser: &mut Parser) -> SassResult<()> {
        loop {
            let should_break = match self.binary_operators.as_ref() {
                Some(bin) => bin.is_empty(),
                None => true,
            };

            if should_break {
                break;
            }

            self.resolve_one_operation(parser)?;
        }

        Ok(())
    }

    fn add_single_expression(
        &mut self,
        expression: Spanned<AstExpr>,
        parser: &mut Parser,
    ) -> SassResult<()> {
        if self.single_expression.is_some() {
            // If we discover we're parsing a list whose first element is a division
            // operation, and we're in parentheses, reparse outside of a paren
            // context. This ensures that `(1/2 1)` doesn't perform division on its
            // first element.
            if parser.flags.in_parens() {
                parser.flags.set(ContextFlags::IN_PARENS, false);

                if self.allow_slash {
                    self.reset_state(parser)?;

                    return Ok(());
                }
            }

            if self.space_expressions.is_none() {
                self.space_expressions = Some(Vec::new());
            }

            self.resolve_operations(parser)?;

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
        if parser.is_plain_css && op.node != BinaryOp::Div && op.node != BinaryOp::SingleEq {
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

            self.resolve_one_operation(parser)?;
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

        parser.whitespace()?;

        self.single_expression = Some(self.parse_single_expression(parser)?);

        Ok(())
    }

    fn resolve_space_expressions(&mut self, parser: &mut Parser) -> SassResult<()> {
        self.resolve_operations(parser)?;

        if let Some(mut space_expressions) = self.space_expressions.take() {
            let single_expression = match self.single_expression.take() {
                Some(val) => val,
                None => return Err(("Expected expression.", parser.toks.current_span()).into()),
            };

            let span = single_expression.span;

            space_expressions.push(single_expression);

            self.single_expression = Some(
                AstExpr::List(ListExpr {
                    elems: space_expressions,
                    separator: ListSeparator::Space,
                    brackets: Brackets::None,
                })
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
        let mut pairs = vec![(first, parser.parse_expression_until_comma(false)?.node)];

        while parser.scan_char(',') {
            parser.whitespace()?;
            if !parser.looking_at_expression() {
                break;
            }

            let key = parser.parse_expression_until_comma(false)?;
            parser.expect_char(':')?;
            parser.whitespace()?;
            let value = parser.parse_expression_until_comma(false)?;
            pairs.push((key, value.node));
        }

        parser.expect_char(')')?;

        Ok(AstExpr::Map(AstSassMap(pairs)).span(parser.span_before))
    }

    fn parse_paren_expr(&mut self, parser: &mut Parser) -> SassResult<Spanned<AstExpr>> {
        if parser.is_plain_css {
            return Err((
                "Parentheses aren't allowed in plain CSS.",
                parser.toks.current_span(),
            )
                .into());
        }

        let was_in_parentheses = parser.flags.in_parens();
        parser.flags.set(ContextFlags::IN_PARENS, true);

        parser.expect_char('(')?;
        parser.whitespace()?;
        if !parser.looking_at_expression() {
            parser.expect_char(')')?;
            parser
                .flags
                .set(ContextFlags::IN_PARENS, was_in_parentheses);
            return Ok(AstExpr::List(ListExpr {
                elems: Vec::new(),
                separator: ListSeparator::Undecided,
                brackets: Brackets::None,
            })
            .span(parser.span_before));
        }

        let first = parser.parse_expression_until_comma(false)?;
        if parser.scan_char(':') {
            parser.whitespace()?;
            parser
                .flags
                .set(ContextFlags::IN_PARENS, was_in_parentheses);
            return self.parse_map(parser, first);
        }

        if !parser.scan_char(',') {
            parser.expect_char(')')?;
            parser
                .flags
                .set(ContextFlags::IN_PARENS, was_in_parentheses);
            return Ok(AstExpr::Paren(Box::new(first.node)).span(first.span));
        }

        parser.whitespace()?;

        let mut expressions = vec![first];

        loop {
            if !parser.looking_at_expression() {
                break;
            }
            expressions.push(parser.parse_expression_until_comma(false)?);
            if !parser.scan_char(',') {
                break;
            }
            parser.whitespace()?;
        }

        parser.expect_char(')')?;

        parser
            .flags
            .set(ContextFlags::IN_PARENS, was_in_parentheses);

        Ok(AstExpr::List(ListExpr {
            elems: expressions,
            separator: ListSeparator::Comma,
            brackets: Brackets::None,
        })
        .span(parser.span_before))
    }

    fn parse_variable(parser: &mut Parser) -> SassResult<Spanned<AstExpr>> {
        let start = parser.toks.cursor();
        let name = parser.parse_variable_name()?;

        if parser.is_plain_css {
            return Err((
                "Sass variables aren't allowed in plain CSS.",
                parser.toks.span_from(start),
            )
                .into());
        }

        Ok(AstExpr::Variable {
            name: Spanned {
                node: Identifier::from(name),
                span: parser.toks.span_from(start),
            },
            namespace: None,
        }
        .span(parser.span_before))
    }

    fn parse_selector(parser: &mut Parser) -> SassResult<Spanned<AstExpr>> {
        if parser.is_plain_css {
            return Err((
                "The parent selector isn't allowed in plain CSS.",
                parser.toks.current_span(),
            )
                .into());
        }

        let start = parser.toks.cursor();

        parser.expect_char('&')?;

        if parser.toks.next_char_is('&') {
            // todo: emit a warning here
            //   warn(
            //       'In Sass, "&&" means two copies of the parent selector. You '
            //       'probably want to use "and" instead.',
            //       scanner.spanFrom(start));
            //   scanner.position--;
        }

        Ok(AstExpr::ParentSelector.span(parser.toks.span_from(start)))
    }

    fn parse_hash(&mut self, parser: &mut Parser) -> SassResult<Spanned<AstExpr>> {
        let start = parser.toks.cursor();
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

        let mut buffer = Interpolation::new();

        buffer.add_token(Token {
            kind: '#',
            pos: parser.span_before,
        });
        buffer.add_interpolation(ident);

        let span = parser.toks.span_from(start);

        Ok(AstExpr::String(StringExpr(buffer, QuoteKind::None), span).span(span))
    }

    fn parse_hex_digit(parser: &mut Parser) -> SassResult<u32> {
        match parser.toks.peek() {
            Some(Token { kind, .. }) if kind.is_ascii_hexdigit() => {
                parser.toks.next();
                Ok(as_hex(kind))
            }
            _ => Err(("Expected hex digit.", parser.toks.current_span()).into()),
        }
    }

    fn parse_hex_color_contents(&mut self, parser: &mut Parser) -> SassResult<Color> {
        let start = parser.toks.cursor();

        let digit1 = Self::parse_hex_digit(parser)?;
        let digit2 = Self::parse_hex_digit(parser)?;
        let digit3 = Self::parse_hex_digit(parser)?;

        let red: u32;
        let green: u32;
        let blue: u32;
        let mut alpha: f64 = 1.0;

        if parser.next_is_hex() {
            let digit4 = Self::parse_hex_digit(parser)?;

            if parser.next_is_hex() {
                red = (digit1 << 4) + digit2;
                green = (digit3 << 4) + digit4;
                blue = (Self::parse_hex_digit(parser)? << 4) + Self::parse_hex_digit(parser)?;

                if parser.next_is_hex() {
                    alpha = ((Self::parse_hex_digit(parser)? << 4) + Self::parse_hex_digit(parser)?)
                        as f64
                        / 0xff as f64;
                }
            } else {
                // #abcd
                red = (digit1 << 4) + digit1;
                green = (digit2 << 4) + digit2;
                blue = (digit3 << 4) + digit3;
                alpha = ((digit4 << 4) + digit4) as f64 / 0xff as f64;
            }
        } else {
            // #abc
            red = (digit1 << 4) + digit1;
            green = (digit2 << 4) + digit2;
            blue = (digit3 << 4) + digit3;
        }

        Ok(Color::new_rgba(
            Number::from(red),
            Number::from(green),
            Number::from(blue),
            Number::from(alpha),
            // todo:
            //     // Don't emit four- or eight-digit hex colors as hex, since that's not
            //     // yet well-supported in browsers.
            ColorFormat::Literal(parser.toks.raw_text(start - 1)),
        ))
    }

    fn parse_unary_operation(&mut self, parser: &mut Parser) -> SassResult<Spanned<AstExpr>> {
        let op_span = parser.toks.current_span();
        let operator = self.expect_unary_operator(parser)?;

        if parser.is_plain_css && operator != UnaryOp::Div {
            return Err(("Operators aren't allowed in plain CSS.", op_span).into());
        }

        parser.whitespace()?;

        let operand = self.parse_single_expression(parser)?;

        Ok(AstExpr::UnaryOp(operator, Box::new(operand.node))
            .span(op_span.merge(parser.toks.current_span())))
    }

    fn expect_unary_operator(&mut self, parser: &mut Parser) -> SassResult<UnaryOp> {
        let span = parser.toks.current_span();
        Ok(match parser.toks.next() {
            Some(Token { kind: '+', .. }) => UnaryOp::Plus,
            Some(Token { kind: '-', .. }) => UnaryOp::Neg,
            Some(Token { kind: '/', .. }) => UnaryOp::Div,
            Some(..) | None => return Err(("Expected unary operator.", span).into()),
        })
    }

    fn consume_natural_number(&mut self, parser: &mut Parser) -> SassResult<()> {
        if !matches!(
            parser.toks.next(),
            Some(Token {
                kind: '0'..='9',
                ..
            })
        ) {
            return Err(("Expected digit.", parser.toks.prev_span()).into());
        }

        while matches!(
            parser.toks.peek(),
            Some(Token {
                kind: '0'..='9',
                ..
            })
        ) {
            parser.toks.next();
        }

        Ok(())
    }

    fn parse_number(&mut self, parser: &mut Parser) -> SassResult<Spanned<AstExpr>> {
        let start = parser.toks.cursor();

        if !parser.scan_char('+') {
            parser.scan_char('-');
        }

        let after_sign = parser.toks.cursor();

        if !parser.toks.next_char_is('.') {
            self.consume_natural_number(parser)?;
        }

        Self::try_decimal(parser, parser.toks.cursor() != after_sign)?;
        Self::try_exponent(parser)?;

        let number: f64 = parser.toks.raw_text(start).parse().unwrap();

        let unit = if parser.scan_char('%') {
            Unit::Percent
        } else if parser.looking_at_identifier()
            && (!matches!(parser.toks.peek(), Some(Token { kind: '-', .. }))
                || !matches!(parser.toks.peek_n(1), Some(Token { kind: '-', .. })))
        {
            Unit::from(parser.parse_identifier(false, true)?)
        } else {
            Unit::None
        };

        Ok(AstExpr::Number {
            n: Number::from(number),
            unit,
        }
        .span(parser.span_before))
    }

    fn try_decimal(parser: &mut Parser, allow_trailing_dot: bool) -> SassResult<Option<String>> {
        if !matches!(parser.toks.peek(), Some(Token { kind: '.', .. })) {
            return Ok(None);
        }

        match parser.toks.peek_n(1) {
            Some(Token { kind, pos }) if !kind.is_ascii_digit() => {
                if allow_trailing_dot {
                    return Ok(None);
                }

                return Err(("Expected digit.", pos).into());
            }
            Some(..) => {}
            None => return Err(("Expected digit.", parser.toks.current_span()).into()),
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

    fn try_exponent(parser: &mut Parser) -> SassResult<Option<String>> {
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
            _ => return Err(("Expected digit.", parser.toks.current_span()).into()),
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
        debug_assert!(parser.toks.next_char_is('+'));

        match parser.toks.peek_n(1) {
            Some(Token {
                kind: '0'..='9' | '.',
                ..
            }) => self.parse_number(parser),
            _ => self.parse_unary_operation(parser),
        }
    }

    fn parse_minus_expr(&mut self, parser: &mut Parser) -> SassResult<Spanned<AstExpr>> {
        debug_assert!(parser.toks.next_char_is('-'));

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

    fn parse_important_expr(parser: &mut Parser) -> SassResult<Spanned<AstExpr>> {
        let start = parser.toks.cursor();
        parser.expect_char('!')?;
        parser.whitespace()?;
        parser.expect_identifier("important", false)?;

        let span = parser.toks.span_from(start);

        Ok(AstExpr::String(
            StringExpr(
                Interpolation::new_plain("!important".to_owned()),
                QuoteKind::None,
            ),
            span,
        )
        .span(span))
    }

    fn parse_identifier_like(&mut self, parser: &mut Parser) -> SassResult<Spanned<AstExpr>> {
        let start = parser.toks.cursor();

        let identifier = parser.parse_interpolated_identifier()?;

        let ident_span = parser.toks.span_from(start);

        let plain = identifier.as_plain();
        let lower = plain.map(str::to_ascii_lowercase);

        if let Some(plain) = plain {
            if plain == "if" && parser.toks.next_char_is('(') {
                let call_args = parser.parse_argument_invocation(false, false)?;
                let span = call_args.span;
                return Ok(AstExpr::If(Box::new(Ternary(call_args))).span(span));
            } else if plain == "not" {
                parser.whitespace()?;

                let value = self.parse_single_expression(parser)?;

                return Ok(AstExpr::UnaryOp(UnaryOp::Not, Box::new(value.node))
                    .span(parser.toks.span_from(start)));
            }

            let lower_ref = lower.as_ref().unwrap();

            if !parser.toks.next_char_is('(') {
                match plain {
                    "null" => return Ok(AstExpr::Null.span(parser.toks.span_from(start))),
                    "true" => return Ok(AstExpr::True.span(parser.toks.span_from(start))),
                    "false" => return Ok(AstExpr::False.span(parser.toks.span_from(start))),
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
                    .span(parser.toks.span_from(start)));
                }
            }

            if let Some(func) = self.try_parse_special_function(parser, lower_ref, start)? {
                return Ok(func);
            }
        }

        match parser.toks.peek() {
            Some(Token { kind: '.', .. }) => {
                if matches!(parser.toks.peek_n(1), Some(Token { kind: '.', .. })) {
                    return Ok(AstExpr::String(
                        StringExpr(identifier, QuoteKind::None),
                        parser.toks.span_from(start),
                    )
                    .span(parser.toks.span_from(start)));
                }
                parser.toks.next();

                match plain {
                    Some(s) => Self::namespaced_expression(
                        Spanned {
                            node: Identifier::from(s),
                            span: ident_span,
                        },
                        start,
                        parser,
                    ),
                    None => Err(("Interpolation isn't allowed in namespaces.", ident_span).into()),
                }
            }
            Some(Token { kind: '(', .. }) => {
                if let Some(plain) = plain {
                    let arguments =
                        parser.parse_argument_invocation(false, lower.as_deref() == Some("var"))?;

                    Ok(AstExpr::FunctionCall(FunctionCallExpr {
                        namespace: None,
                        name: Identifier::from(plain),
                        arguments: Box::new(arguments),
                        span: parser.toks.span_from(start),
                    })
                    .span(parser.toks.span_from(start)))
                } else {
                    let arguments = parser.parse_argument_invocation(false, false)?;
                    Ok(AstExpr::InterpolatedFunction(InterpolatedFunction {
                        name: identifier,
                        arguments: Box::new(arguments),
                        span: parser.toks.span_from(start),
                    })
                    .span(parser.toks.span_from(start)))
                }
            }
            _ => Ok(AstExpr::String(
                StringExpr(identifier, QuoteKind::None),
                parser.toks.span_from(start),
            )
            .span(parser.toks.span_from(start))),
        }
    }

    fn namespaced_expression(
        namespace: Spanned<Identifier>,
        start: usize,
        parser: &mut Parser,
    ) -> SassResult<Spanned<AstExpr>> {
        if parser.toks.next_char_is('$') {
            let name_start = parser.toks.cursor();
            let name = parser.parse_variable_name()?;
            let span = parser.toks.span_from(start);
            Parser::assert_public(&name, span)?;

            return Ok(AstExpr::Variable {
                name: Spanned {
                    node: Identifier::from(name),
                    span: parser.toks.span_from(name_start),
                },
                namespace: Some(namespace),
            }
            .span(span));
        }

        let name = parser.parse_public_identifier()?;
        let args = parser.parse_argument_invocation(false, false)?;
        let span = parser.toks.span_from(start);

        Ok(AstExpr::FunctionCall(FunctionCallExpr {
            namespace: Some(namespace),
            name: Identifier::from(name),
            arguments: Box::new(args),
            span: span,
        })
        .span(span))
    }

    fn parse_unicode_range(parser: &mut Parser) -> SassResult<Spanned<AstExpr>> {
        let start = parser.toks.cursor();
        parser.expect_ident_char('u', false)?;
        parser.expect_char('+')?;

        let mut first_range_length = 0;

        while let Some(next) = parser.toks.peek() {
            if !next.kind.is_ascii_hexdigit() {
                break;
            }

            parser.toks.next();
            first_range_length += 1;
        }

        let mut has_question_mark = false;

        while parser.scan_char('?') {
            has_question_mark = true;
            first_range_length += 1;
        }

        let span = parser.toks.span_from(start);
        if first_range_length == 0 {
            return Err(("Expected hex digit or \"?\".", parser.toks.current_span()).into());
        } else if first_range_length > 6 {
            return Err(("Expected at most 6 digits.", span).into());
        } else if has_question_mark {
            return Ok(AstExpr::String(
                StringExpr(
                    Interpolation::new_plain(parser.toks.raw_text(start)),
                    QuoteKind::None,
                ),
                span,
            )
            .span(span));
        }

        if parser.scan_char('-') {
            let second_range_start = parser.toks.cursor();
            let mut second_range_length = 0;

            while let Some(next) = parser.toks.peek() {
                if !next.kind.is_ascii_hexdigit() {
                    break;
                }

                parser.toks.next();
                second_range_length += 1;
            }

            if second_range_length == 0 {
                return Err(("Expected hex digit.", parser.toks.current_span()).into());
            } else if second_range_length > 6 {
                return Err((
                    "Expected at most 6 digits.",
                    parser.toks.span_from(second_range_start),
                )
                    .into());
            }
        }

        if parser.looking_at_interpolated_identifier_body() {
            return Err(("Expected end of identifier.", parser.toks.current_span()).into());
        }

        let span = parser.toks.span_from(start);

        Ok(AstExpr::String(
            StringExpr(
                Interpolation::new_plain(parser.toks.raw_text(start)),
                QuoteKind::None,
            ),
            span,
        )
        .span(span))
    }

    fn try_parse_url_contents(
        parser: &mut Parser,
        name: Option<String>,
    ) -> SassResult<Option<Interpolation>> {
        let start = parser.toks.cursor();

        if !parser.scan_char('(') {
            return Ok(None);
        }

        parser.whitespace_without_comments();

        // Match Ruby Sass's behavior: parse a raw URL() if possible, and if not
        // backtrack and re-parse as a function expression.
        let mut buffer = Interpolation::new();
        buffer.add_string(name.unwrap_or_else(|| "url".to_owned()));
        buffer.add_char('(');

        while let Some(next) = parser.toks.peek() {
            match next.kind {
                '\\' => {
                    buffer.add_string(parser.parse_escape(false)?);
                }
                '!' | '%' | '&' | '*'..='~' | '\u{80}'..=char::MAX => {
                    parser.toks.next();
                    buffer.add_token(next);
                }
                '#' => {
                    if matches!(parser.toks.peek_n(1), Some(Token { kind: '{', .. })) {
                        buffer.add_interpolation(parser.parse_single_interpolation()?);
                    } else {
                        parser.toks.next();
                        buffer.add_token(next);
                    }
                }
                ')' => {
                    parser.toks.next();
                    buffer.add_token(next);

                    return Ok(Some(buffer));
                }
                ' ' | '\t' | '\n' | '\r' => {
                    parser.whitespace_without_comments();

                    if !parser.toks.next_char_is(')') {
                        break;
                    }
                }
                _ => break,
            }
        }

        parser.toks.set_cursor(start);
        Ok(None)
    }

    fn try_parse_special_function(
        &mut self,
        parser: &mut Parser,
        name: &str,
        start: usize,
    ) -> SassResult<Option<Spanned<AstExpr>>> {
        if matches!(parser.toks.peek(), Some(Token { kind: '(', .. })) {
            if let Some(calculation) = self.try_parse_calculation(parser, name, start)? {
                return Ok(Some(calculation));
            }
        }

        let normalized = unvendor(name);

        let mut buffer;

        match normalized {
            "calc" | "element" | "expression" => {
                if !parser.scan_char('(') {
                    return Ok(None);
                }

                buffer = Interpolation::new_plain(name.to_owned());
                buffer.add_char('(');
            }
            "progid" => {
                if !parser.scan_char(':') {
                    return Ok(None);
                }
                buffer = Interpolation::new_plain(name.to_owned());
                buffer.add_char(':');

                while let Some(Token { kind, .. }) = parser.toks.peek() {
                    if !kind.is_alphabetic() && kind != '.' {
                        break;
                    }
                    buffer.add_char(kind);
                    parser.toks.next();
                }
                parser.expect_char('(')?;
                buffer.add_char('(');
            }
            "url" => {
                return Ok(Self::try_parse_url_contents(parser, None)?.map(|contents| {
                    AstExpr::String(
                        StringExpr(contents, QuoteKind::None),
                        parser.toks.span_from(start),
                    )
                    .span(parser.toks.span_from(start))
                }))
            }
            _ => return Ok(None),
        }

        buffer.add_interpolation(parser.parse_interpolated_declaration_value(false, true, true)?);
        parser.expect_char(')')?;
        buffer.add_char(')');

        Ok(Some(
            AstExpr::String(
                StringExpr(buffer, QuoteKind::None),
                parser.toks.span_from(start),
            )
            .span(parser.toks.span_from(start)),
        ))
    }

    fn contains_calculation_interpolation(parser: &mut Parser) -> SassResult<bool> {
        let mut parens = 0;
        let mut brackets = Vec::new();

        let start = parser.toks.cursor();

        while let Some(next) = parser.toks.peek() {
            match next.kind {
                '\\' => {
                    parser.toks.next();
                    // todo: i wonder if this can be broken (not for us but dart-sass)
                    parser.toks.next();
                }
                '/' => {
                    if !parser.scan_comment()? {
                        parser.toks.next();
                    }
                }
                '\'' | '"' => {
                    parser.parse_interpolated_string()?;
                }
                '#' => {
                    if parens == 0 && matches!(parser.toks.peek_n(1), Some(Token { kind: '{', .. }))
                    {
                        parser.toks.set_cursor(start);
                        return Ok(true);
                    }
                    parser.toks.next();
                }
                '(' | '{' | '[' => {
                    if next.kind == '(' {
                        parens += 1;
                    }
                    brackets.push(opposite_bracket(next.kind));
                    parser.toks.next();
                }
                ')' | '}' | ']' => {
                    if next.kind == ')' {
                        parens -= 1;
                    }
                    if brackets.is_empty() || brackets.pop() != Some(next.kind) {
                        parser.toks.set_cursor(start);
                        return Ok(false);
                    }
                    parser.toks.next();
                }
                _ => {
                    parser.toks.next();
                }
            }
        }

        parser.toks.set_cursor(start);
        Ok(false)
    }

    fn try_parse_calculation_interpolation(
        parser: &mut Parser,
        start: usize,
    ) -> SassResult<Option<AstExpr>> {
        Ok(if Self::contains_calculation_interpolation(parser)? {
            Some(AstExpr::String(
                StringExpr(
                    parser.parse_interpolated_declaration_value(false, false, true)?,
                    QuoteKind::None,
                ),
                parser.toks.span_from(start),
            ))
        } else {
            None
        })
    }

    fn parse_calculation_value(&mut self, parser: &mut Parser) -> SassResult<Spanned<AstExpr>> {
        match parser.toks.peek() {
            Some(Token {
                kind: '+' | '-' | '.' | '0'..='9',
                ..
            }) => self.parse_number(parser),
            Some(Token { kind: '$', .. }) => Self::parse_variable(parser),
            Some(Token { kind: '(', .. }) => {
                let start = parser.toks.cursor();
                parser.toks.next();

                let value = match Self::try_parse_calculation_interpolation(parser, start)? {
                    Some(v) => v,
                    None => {
                        parser.whitespace()?;
                        self.parse_calculation_sum(parser)?.node
                    }
                };

                parser.whitespace()?;
                parser.expect_char(')')?;

                Ok(AstExpr::Paren(Box::new(value)).span(parser.toks.span_from(start)))
            }
            _ if !parser.looking_at_identifier() => Err((
                "Expected number, variable, function, or calculation.",
                parser.toks.current_span(),
            )
                .into()),
            _ => {
                let start = parser.toks.cursor();
                let ident = parser.parse_identifier(false, false)?;
                let ident_span = parser.toks.span_from(start);
                if parser.scan_char('.') {
                    return Self::namespaced_expression(
                        Spanned {
                            node: Identifier::from(&ident),
                            span: ident_span,
                        },
                        start,
                        parser,
                    );
                }

                if !parser.toks.next_char_is('(') {
                    return Err(("Expected \"(\" or \".\".", parser.toks.current_span()).into());
                }

                let lowercase = ident.to_ascii_lowercase();
                let calculation = self.try_parse_calculation(parser, &lowercase, start)?;

                if let Some(calc) = calculation {
                    Ok(calc)
                } else if lowercase == "if" {
                    Ok(AstExpr::If(Box::new(Ternary(
                        parser.parse_argument_invocation(false, false)?,
                    )))
                    .span(parser.toks.span_from(start)))
                } else {
                    Ok(AstExpr::FunctionCall(FunctionCallExpr {
                        namespace: None,
                        name: Identifier::from(ident),
                        arguments: Box::new(parser.parse_argument_invocation(false, false)?),
                        span: parser.toks.span_from(start),
                    })
                    .span(parser.toks.span_from(start)))
                }
            }
        }
    }
    fn parse_calculation_product(&mut self, parser: &mut Parser) -> SassResult<Spanned<AstExpr>> {
        let mut product = self.parse_calculation_value(parser)?;

        loop {
            parser.whitespace()?;
            match parser.toks.peek() {
                Some(Token {
                    kind: op @ ('*' | '/'),
                    ..
                }) => {
                    parser.toks.next();
                    parser.whitespace()?;

                    let rhs = self.parse_calculation_value(parser)?;

                    let span = product.span.merge(rhs.span);

                    product.node = AstExpr::BinaryOp {
                        lhs: Box::new(product.node),
                        op: if op == '*' {
                            BinaryOp::Mul
                        } else {
                            BinaryOp::Div
                        },
                        rhs: Box::new(rhs.node),
                        allows_slash: false,
                        span,
                    };

                    product.span = span;
                }
                _ => return Ok(product),
            }
        }
    }
    fn parse_calculation_sum(&mut self, parser: &mut Parser) -> SassResult<Spanned<AstExpr>> {
        let mut sum = self.parse_calculation_product(parser)?;

        loop {
            match parser.toks.peek() {
                Some(Token {
                    kind: next @ ('+' | '-'),
                    pos,
                }) => {
                    if !matches!(
                        parser.toks.peek_n_backwards(1),
                        Some(Token {
                            kind: ' ' | '\t' | '\r' | '\n',
                            ..
                        })
                    ) || !matches!(
                        parser.toks.peek_n(1),
                        Some(Token {
                            kind: ' ' | '\t' | '\r' | '\n',
                            ..
                        })
                    ) {
                        return Err((
                            "\"+\" and \"-\" must be surrounded by whitespace in calculations.",
                            pos,
                        )
                            .into());
                    }

                    parser.toks.next();
                    parser.whitespace()?;

                    let rhs = self.parse_calculation_product(parser)?;

                    let span = sum.span.merge(rhs.span);

                    sum = AstExpr::BinaryOp {
                        lhs: Box::new(sum.node),
                        op: if next == '+' {
                            BinaryOp::Plus
                        } else {
                            BinaryOp::Minus
                        },
                        rhs: Box::new(rhs.node),
                        allows_slash: false,
                        span: span,
                    }
                    .span(span);
                }
                _ => return Ok(sum),
            }
        }
    }

    fn parse_calculation_arguments(
        &mut self,
        parser: &mut Parser,
        max_args: Option<usize>,
        start: usize,
    ) -> SassResult<Vec<AstExpr>> {
        parser.expect_char('(')?;
        if let Some(interpolation) = Self::try_parse_calculation_interpolation(parser, start)? {
            parser.expect_char(')')?;
            return Ok(vec![interpolation]);
        }

        parser.whitespace()?;
        let mut arguments = vec![self.parse_calculation_sum(parser)?.node];

        while (max_args.is_none() || arguments.len() < max_args.unwrap()) && parser.scan_char(',') {
            parser.whitespace()?;
            arguments.push(self.parse_calculation_sum(parser)?.node);
        }

        parser.expect_char_with_message(
            ')',
            if Some(arguments.len()) == max_args {
                r#""+", "-", "*", "/", or ")""#
            } else {
                r#""+", "-", "*", "/", ",", or ")""#
            },
        )?;

        Ok(arguments)
    }

    fn try_parse_calculation(
        &mut self,
        parser: &mut Parser,
        name: &str,
        start: usize,
    ) -> SassResult<Option<Spanned<AstExpr>>> {
        debug_assert!(parser.toks.next_char_is('('));

        Ok(Some(match name {
            "calc" => {
                let args = self.parse_calculation_arguments(parser, Some(1), start)?;

                AstExpr::Calculation {
                    name: CalculationName::Calc,
                    args,
                }
                .span(parser.span_before)
            }
            "min" | "max" => {
                // min() and max() are parsed as calculations if possible, and otherwise
                // are parsed as normal Sass functions.
                let before_args = parser.toks.cursor();

                let args = match self.parse_calculation_arguments(parser, None, start) {
                    Ok(args) => args,
                    Err(..) => {
                        parser.toks.set_cursor(before_args);
                        return Ok(None);
                    }
                };

                AstExpr::Calculation {
                    name: if name == "min" {
                        CalculationName::Min
                    } else {
                        CalculationName::Max
                    },
                    args,
                }
                .span(parser.toks.span_from(start))
            }
            "clamp" => {
                let args = self.parse_calculation_arguments(parser, Some(3), start)?;
                AstExpr::Calculation {
                    name: CalculationName::Clamp,
                    args,
                }
                .span(parser.toks.span_from(start))
            }
            _ => return Ok(None),
        }))
    }

    fn reset_state(&mut self, parser: &mut Parser) -> SassResult<()> {
        self.comma_expressions = None;
        self.space_expressions = None;
        self.binary_operators = None;
        self.operands = None;
        parser.toks.set_cursor(self.start);
        self.allow_slash = true;
        self.single_expression = Some(self.parse_single_expression(parser)?);

        Ok(())
    }
}
