use std::{
    cell::Cell,
    collections::{BTreeMap, HashSet},
    ffi::OsString,
    path::{Path, PathBuf},
};

use codemap::{CodeMap, Span, Spanned};

use crate::{
    ast::*,
    atrule::{
        keyframes::{Keyframes, KeyframesRuleSet},
        media::MediaRule,
        SupportsRule, UnknownAtRule,
    },
    common::{unvendor, Identifier, QuoteKind},
    error::SassResult,
    lexer::Lexer,
    selector::ExtendedSelector,
    style::Style,
    utils::{as_hex, is_name, is_name_start, is_plain_css_import, opposite_bracket},
    ContextFlags, Options, Token,
};

pub(crate) use keyframes::KeyframesSelectorParser;
pub(crate) use value::{add, cmp, div, mul, rem, single_eq, sub};

use self::value_new::{Predicate, ValueParser};

// mod args;
// pub mod common;
// mod control_flow;
// mod function;
mod ident;
// mod import;
mod keyframes;
mod media;
// mod mixin;
// mod module;
// mod style;
// mod throw_away;
mod value;
mod value_new;
// mod variable;
// pub mod visitor;

#[derive(Debug, Clone)]
pub(crate) enum Stmt {
    RuleSet {
        selector: ExtendedSelector,
        body: Vec<Self>,
    },
    Style(Style),
    Media(Box<MediaRule>, bool),
    UnknownAtRule(Box<UnknownAtRule>),
    Supports(Box<SupportsRule>),
    // AtRoot {
    //     body: Vec<Stmt>,
    // },
    Comment(String, Span),
    // Return(Box<Value>),
    Keyframes(Box<Keyframes>),
    KeyframesRuleSet(Box<KeyframesRuleSet>),
    /// A plain import such as `@import "foo.css";` or
    /// `@import url(https://fonts.google.com/foo?bar);`
    Import(String),
}

#[derive(Debug, Clone)]
enum DeclarationOrBuffer {
    Stmt(AstStmt),
    Buffer(Interpolation),
}

// todo: merge at_root and at_root_has_selector into an enum
pub(crate) struct Parser<'a, 'b> {
    pub toks: &'a mut Lexer<'b>,
    // todo: likely superfluous
    pub map: &'a mut CodeMap,
    pub path: &'a Path,
    pub is_plain_css: bool,
    // pub global_scope: &'a mut Scope,
    // pub scopes: &'a mut Scopes,
    // pub content_scopes: &'a mut Scopes,
    // pub super_selectors: &'a mut NeverEmptyVec<ExtendedSelector>,
    pub span_before: Span,
    // pub content: &'a mut Vec<Content>,
    pub flags: ContextFlags,
    /// Whether this parser is at the root of the document
    /// E.g. not inside a style, mixin, or function
    // pub at_root: bool,
    /// If this parser is inside an `@at-rule` block, this is whether or
    /// not the `@at-rule` block has a super selector
    // pub at_root_has_selector: bool,
    // pub extender: &'a mut Extender,
    pub options: &'a Options<'a>,
    // pub modules: &'a mut Modules,
    // pub module_config: &'a mut ModuleConfig,
}

/// Names that functions are not allowed to have
pub(super) const RESERVED_IDENTIFIERS: [&str; 8] = [
    "calc",
    "element",
    "expression",
    "url",
    "and",
    "or",
    "not",
    "clamp",
];

#[derive(Debug, Clone)]
enum VariableDeclOrInterpolation {
    VariableDecl(AstVariableDecl),
    Interpolation(Interpolation),
}

impl<'a, 'b> Parser<'a, 'b> {
    pub fn __parse(&mut self) -> SassResult<StyleSheet> {
        let mut style_sheet = StyleSheet::new();

        // Allow a byte-order mark at the beginning of the document.
        self.consume_char_if_exists('\u{feff}');

        // self.whitespace();
        // stmts.append(&mut self.load_modules()?);

        style_sheet.body = self.parse_statements(|parser| {
            if parser.next_matches("@charset") {
                parser.expect_char('@')?;
                parser.expect_identifier("charset", false)?;
                parser.whitespace_or_comment();
                parser.parse_string()?;
                return Ok(None);
            }

            Ok(Some(parser.__parse_stmt()?))
        })?;

        // while self.toks.peek().is_some() {
        //     style_sheet.body.push(self.__parse_stmt()?);
        //     self.whitespace();
        //     //     stmts.append(&mut self.parse_stmt()?);
        //     //     if self.flags.in_function() && !stmts.is_empty() {
        //     //         // return Ok(stmts);
        //     //     }
        //     //     self.at_root = true;
        // }

        Ok(style_sheet)
    }

    fn looking_at_expression(&mut self) -> bool {
        let character = if let Some(c) = self.toks.peek() {
            c
        } else {
            return false;
        };

        match character.kind {
            '.' => !matches!(self.toks.peek_n(1), Some(Token { kind: '.', .. })),
            '!' => match self.toks.peek_n(1) {
                Some(Token {
                    kind: 'i' | 'I', ..
                }) => true,
                Some(Token { kind, .. }) => kind.is_ascii_whitespace(),
                None => true,
            },
            '(' | '/' | '[' | '\'' | '"' | '#' | '+' | '-' | '\\' | '$' | '&' => true,
            c => is_name_start(c) || c.is_ascii_digit(),
        }
    }

    fn parse_statements(
        &mut self,
        statement: fn(&mut Self) -> SassResult<Option<AstStmt>>,
    ) -> SassResult<Vec<AstStmt>> {
        let mut stmts = Vec::new();
        self.whitespace();
        while let Some(tok) = self.toks.peek() {
            match tok.kind {
                '$' => stmts.push(AstStmt::VariableDecl(
                    self.parse_variable_declaration_without_namespace(None, None)?,
                )),
                '/' => match self.toks.peek_n(1) {
                    Some(Token { kind: '/', .. }) => {
                        stmts.push(self.parse_silent_comment()?);
                        self.whitespace();
                    }
                    Some(Token { kind: '*', .. }) => {
                        stmts.push(AstStmt::LoudComment(self.parse_loud_comment()?));
                        self.whitespace();
                    }
                    _ => {
                        if let Some(stmt) = statement(self)? {
                            stmts.push(stmt);
                        }
                    }
                },
                ';' => {
                    self.toks.next();
                    self.whitespace();
                }
                _ => {
                    if let Some(stmt) = statement(self)? {
                        stmts.push(stmt);
                    }
                }
            }
        }

        Ok(stmts)
    }

    pub(crate) fn parse_identifier_body(
        &mut self,
        buffer: &mut String,
        normalize: bool,
        unit: bool,
    ) -> SassResult<()> {
        while let Some(tok) = self.toks.peek() {
            if unit && tok.kind == '-' {
                // Disallow `-` followed by a dot or a digit digit in units.
                let second = match self.toks.peek_n(1) {
                    Some(v) => v,
                    None => break,
                };

                if second.kind == '.' || second.kind.is_ascii_digit() {
                    break;
                }

                self.toks.next();
                buffer.push('-');
            } else if normalize && tok.kind == '_' {
                buffer.push('-');
                self.toks.next();
            } else if is_name(tok.kind) {
                self.toks.next();
                buffer.push(tok.kind);
            } else if tok.kind == '\\' {
                buffer.push_str(&self.parse_escape(false)?);
            } else {
                break;
            }
        }

        Ok(())
    }

    fn consume_escaped_char(&mut self) -> SassResult<char> {
        self.expect_char('\\')?;

        match self.toks.peek() {
            None => return Ok('\u{FFFD}'),
            Some(Token {
                kind: '\n' | '\r',
                pos,
            }) => return Err(("Expected escape sequence.", pos).into()),
            Some(Token { kind, .. }) if kind.is_ascii_hexdigit() => {
                let mut value = 0;
                for _ in 0..6 {
                    let next = match self.toks.peek() {
                        Some(c) => c,
                        None => break,
                    };
                    if !next.kind.is_ascii_hexdigit() {
                        break;
                    }
                    self.toks.next();
                    value = (value << 4) + as_hex(next.kind);
                }

                if self.toks.peek().is_some()
                    && self.toks.peek().unwrap().kind.is_ascii_whitespace()
                {
                    self.toks.next();
                }

                if value == 0 || (0xD800..=0xDFFF).contains(&value) || value >= 0x0010_FFFF {
                    return Ok('\u{FFFD}');
                } else {
                    return Ok(char::from_u32(value).unwrap());
                }
            }
            Some(Token { kind, .. }) => {
                self.toks.next();
                return Ok(kind);
            }
        }
        //         scanner.expectChar($backslash);
        //   var first = scanner.peekChar();
        //   if (first == null) {
        //     return 0xFFFD;
        //   } else if (isNewline(first)) {
        //     scanner.error("Expected escape sequence.");
        //   } else if (isHex(first)) {
        //   } else {
        //     return scanner.readChar();
        //   }
    }

    // todo: return span
    pub fn __parse_identifier(
        &mut self,
        // default=false
        normalize: bool,
        // default=false
        unit: bool,
    ) -> SassResult<String> {
        // NOTE: this logic is largely duplicated in
        // StylesheetParser.interpolatedIdentifier. Most changes here should be
        // mirrored there.

        let mut text = String::new();

        if self.consume_char_if_exists('-') {
            text.push('-');

            if self.consume_char_if_exists('-') {
                text.push('-');
                self.parse_identifier_body(&mut text, normalize, unit)?;
                return Ok(text);
            }
        }

        match self.toks.peek() {
            Some(Token { kind: '_', .. }) if normalize => {
                self.toks.next();
                text.push('-');
            }
            Some(Token { kind, .. }) if is_name_start(kind) => {
                self.toks.next();
                text.push(kind);
            }
            Some(Token { kind: '\\', .. }) => {
                text.push_str(&self.parse_escape(true)?);
            }
            Some(..) | None => {
                return Err(("Expected identifier.", self.toks.current_span()).into())
            }
        }

        self.parse_identifier_body(&mut text, normalize, unit)?;

        Ok(text)
    }

    fn parse_variable_name(&mut self) -> SassResult<String> {
        self.expect_char('$')?;
        self.__parse_identifier(true, false)
    }

    fn parse_argument_declaration(&mut self) -> SassResult<ArgumentDeclaration> {
        self.expect_char('(')?;
        self.whitespace_or_comment();

        let mut arguments = Vec::new();
        let mut named = HashSet::new();

        let mut rest_argument: Option<Identifier> = None;

        while self.toks.next_char_is('$') {
            let name = Identifier::from(self.parse_variable_name()?);
            self.whitespace_or_comment();

            let mut default_value: Option<AstExpr> = None;

            if self.consume_char_if_exists(':') {
                self.whitespace_or_comment();
                default_value = Some(self.parse_expression_until_comma(false)?.node);
            } else if self.consume_char_if_exists('.') {
                self.expect_char('.')?;
                self.expect_char('.')?;
                self.whitespace_or_comment();
                rest_argument = Some(name);
                break;
            }

            arguments.push(Argument {
                name,
                default: default_value,
            });

            if !named.insert(name) {
                todo!("Duplicate argument.")
            }

            if !self.consume_char_if_exists(',') {
                break;
            }
            self.whitespace_or_comment();
        }
        self.expect_char(')')?;

        Ok(ArgumentDeclaration {
            args: arguments,
            rest: rest_argument,
        })
    }

    fn plain_at_rule_name(&mut self) -> SassResult<String> {
        self.expect_char('@')?;
        let name = self.__parse_identifier(false, false)?;
        self.whitespace_or_comment();
        Ok(name)
    }

    fn with_children(
        &mut self,
        child: fn(&mut Self) -> SassResult<AstStmt>,
    ) -> SassResult<Vec<AstStmt>> {
        let children = self.parse_children(child)?;
        self.whitespace();
        Ok(children)
    }

    fn parse_at_root_query(&mut self) -> SassResult<Interpolation> {
        todo!()
    }

    fn parse_at_root_rule(&mut self) -> SassResult<AstStmt> {
        Ok(AstStmt::AtRootRule(if self.toks.next_char_is('(') {
            let query = self.parse_at_root_query()?;
            self.whitespace_or_comment();
            let children = self.with_children(Self::__parse_stmt)?;

            AstAtRootRule {
                query: Some(query),
                children,
                span: self.span_before,
            }
        } else if self.looking_at_children() {
            let children = self.with_children(Self::__parse_stmt)?;
            AstAtRootRule {
                query: None,
                children,
                span: self.span_before,
            }
        } else {
            let child = self.parse_style_rule(None)?;
            AstAtRootRule {
                query: None,
                children: vec![child],
                span: self.span_before,
            }
        }))
    }

    fn parse_content_rule(&mut self, start: usize) -> SassResult<AstStmt> {
        if !self.flags.in_mixin() {
            return Err((
                "@content is only allowed within mixin declarations.",
                self.toks.span_from(start),
            )
                .into());
        }

        self.whitespace_or_comment();

        let args = if self.toks.next_char_is('(') {
            self.parse_argument_invocation(true, false)?
        } else {
            ArgumentInvocation::empty(self.toks.current_span())
        };

        self.expect_statement_separator(Some("@content rule"))?;

        self.flags.set(ContextFlags::FOUND_CONTENT_RULE, true);

        Ok(AstStmt::ContentRule(AstContentRule { args }))
    }

    fn parse_debug_rule(&mut self) -> SassResult<AstStmt> {
        let value = self.parse_expression(None, None, None)?;
        self.expect_statement_separator(Some("@debug rule"))?;

        Ok(AstStmt::Debug(AstDebugRule {
            value: value.node,
            span: value.span,
        }))
    }

    fn parse_each_rule(
        &mut self,
        child: fn(&mut Self) -> SassResult<AstStmt>,
    ) -> SassResult<AstStmt> {
        let was_in_control_directive = self.flags.in_control_flow();
        self.flags.set(ContextFlags::IN_CONTROL_FLOW, true);

        let mut variables = vec![Identifier::from(self.parse_variable_name()?)];
        self.whitespace_or_comment();
        while self.consume_char_if_exists(',') {
            self.whitespace_or_comment();
            variables.push(Identifier::from(self.parse_variable_name()?));
            self.whitespace_or_comment();
        }

        self.expect_identifier("in", false)?;
        self.whitespace_or_comment();

        let list = self.parse_expression(None, None, None)?.node;

        let body = self.with_children(child)?;

        self.flags
            .set(ContextFlags::IN_CONTROL_FLOW, was_in_control_directive);

        Ok(AstStmt::Each(AstEach {
            variables,
            list,
            body,
        }))
        //      var wasInControlDirective = _inControlDirective;
        // _inControlDirective = true;

        // var variables = [variableName()];
        // whitespace();
        // while (scanner.scanChar($comma)) {
        //   whitespace();
        //   variables.add(variableName());
        //   whitespace();
        // }

        // expectIdentifier("in");
        // whitespace();

        // var list = _expression();

        // return _withChildren(child, start, (children, span) {
        //   _inControlDirective = wasInControlDirective;
        //   return EachRule(variables, list, children, span);
        // });
        // todo!()
    }

    fn parse_disallowed_at_rule(&mut self, start: usize) -> SassResult<AstStmt> {
        self.almost_any_value(false)?;
        return Err((
            "This at-rule is not allowed here.",
            self.toks.span_from(start),
        )
            .into());
    }

    fn parse_error_rule(&mut self) -> SassResult<AstStmt> {
        let value = self.parse_expression(None, None, None)?;
        self.expect_statement_separator(Some("@error rule"))?;
        Ok(AstStmt::ErrorRule(AstErrorRule {
            value: value.node,
            span: value.span,
        }))
    }

    fn parse_extend_rule(&mut self, start: usize) -> SassResult<AstStmt> {
        if !self.flags.in_style_rule() && !self.flags.in_mixin() && !self.flags.in_content_block() {
            return Err((
                "@extend may only be used within style rules.",
                self.toks.span_from(start),
            )
                .into());
        }

        let value = self.almost_any_value(false)?;

        let is_optional = self.consume_char_if_exists('!');

        if is_optional {
            self.expect_identifier("optional", false)?;
        }

        self.expect_statement_separator(Some("@extend rule"))?;

        Ok(AstStmt::Extend(AstExtendRule {
            value,
            is_optional,
            span: self.toks.span_from(start),
        }))
    }

    fn parse_for_rule(
        &mut self,
        child: fn(&mut Self) -> SassResult<AstStmt>,
    ) -> SassResult<AstStmt> {
        let was_in_control_directive = self.flags.in_control_flow();
        self.flags.set(ContextFlags::IN_CONTROL_FLOW, true);

        let variable = Spanned {
            node: Identifier::from(self.parse_variable_name()?),
            span: self.span_before,
        };
        self.whitespace_or_comment();

        self.expect_identifier("from", false)?;
        self.whitespace_or_comment();

        // todo: we shouldn't require cell here
        let exclusive: Cell<Option<bool>> = Cell::new(None);

        let from = self.parse_expression(
            Some(&|parser| {
                if !parser.looking_at_identifier() {
                    return Ok(false);
                }
                Ok(if parser.scan_identifier("to", false)? {
                    exclusive.set(Some(true));
                    true
                } else if parser.scan_identifier("through", false)? {
                    exclusive.set(Some(false));
                    true
                } else {
                    false
                })
            }),
            None,
            None,
        )?;

        let is_exclusive = match exclusive.get() {
            Some(b) => b,
            None => todo!("Expected \"to\" or \"through\"."),
        };

        self.whitespace_or_comment();

        let to = self.parse_expression(None, None, None)?;

        let body = self.with_children(child)?;

        self.flags
            .set(ContextFlags::IN_CONTROL_FLOW, was_in_control_directive);

        Ok(AstStmt::For(AstFor {
            variable,
            from,
            to,
            is_exclusive,
            body,
        }))

        //     var wasInControlDirective = _inControlDirective;
        // _inControlDirective = true;
        // var variable = variableName();
        // whitespace();

        // expectIdentifier("from");
        // whitespace();

        // bool? exclusive;
        // var from = _expression(until: () {
        //   if (!lookingAtIdentifier()) return false;
        //   if (scanIdentifier("to")) {
        //     exclusive = true;
        //     return true;
        //   } else if (scanIdentifier("through")) {
        //     exclusive = false;
        //     return true;
        //   } else {
        //     return false;
        //   }
        // });
        // if (exclusive == null) scanner.error('Expected "to" or "through".');

        // whitespace();
        // var to = _expression();

        // return _withChildren(child, start, (children, span) {
        //   _inControlDirective = wasInControlDirective;
        //   return ForRule(variable, from, to, children, span,
        //       exclusive: exclusive!); // dart-lang/sdk#45348
        // });
        // todo!()
    }

    fn parse_function_rule(&mut self, start: usize) -> SassResult<AstStmt> {
        let name_start = self.toks.cursor();
        let name = self.__parse_identifier(true, false)?;
        let name_span = self.toks.span_from(name_start);
        self.whitespace_or_comment();
        let arguments = self.parse_argument_declaration()?;

        if self.flags.in_mixin() || self.flags.in_content_block() {
            return Err((
                "Mixins may not contain function declarations.",
                self.toks.span_from(start),
            )
                .into());
        } else if self.flags.in_control_flow() {
            return Err((
                "Functions may not be declared in control directives.",
                self.toks.span_from(start),
            )
                .into());
        }

        if RESERVED_IDENTIFIERS.contains(&unvendor(&name)) {
            return Err(("Invalid function name.", self.toks.span_from(start)).into());
        }

        self.whitespace_or_comment();

        let children = self.with_children(Self::function_child)?;

        Ok(AstStmt::FunctionDecl(AstFunctionDecl {
            name: Spanned {
                node: Identifier::from(name),
                span: name_span,
            },
            arguments,
            children,
        }))
    }

    fn parse_variable_declaration_with_namespace(&mut self) -> SassResult<AstVariableDecl> {
        let start = self.toks.cursor();
        let namespace = self.__parse_identifier(false, false)?;
        let namespace_span = self.toks.span_from(start);
        self.expect_char('.')?;
        self.parse_variable_declaration_without_namespace(
            Some(Spanned {
                node: Identifier::from(namespace),
                span: namespace_span,
            }),
            Some(start),
        )
    }

    fn function_child(&mut self) -> SassResult<AstStmt> {
        let start = self.toks.cursor();
        if !self.toks.next_char_is('@') {
            match self.parse_variable_declaration_with_namespace() {
                Ok(decl) => return Ok(AstStmt::VariableDecl(decl)),
                Err(..) => {
                    self.toks.set_cursor(start);
                    let stmt = self.parse_declaration_or_style_rule()?;
                    let is_style_rule = matches!(stmt, AstStmt::RuleSet(..));
                    todo!(
                        "@function rules may not contain ${{statement is StyleRule ? \"style rules\" : \"declarations\"}}.",
                    )
                }
            }
        }

        return match self.plain_at_rule_name()?.as_str() {
            "debug" => self.parse_debug_rule(),
            "each" => self.parse_each_rule(Self::function_child),
            "else" => self.parse_disallowed_at_rule(start),
            "error" => self.parse_error_rule(),
            "for" => self.parse_for_rule(Self::function_child),
            "if" => self.parse_if_rule(Self::function_child),
            "return" => self.parse_return_rule(),
            "warn" => self.parse_warn_rule(),
            "while" => self.parse_while_rule(Self::function_child),
            _ => self.parse_disallowed_at_rule(start),
        };
    }

    pub(crate) fn parse_string(&mut self) -> SassResult<String> {
        // NOTE: this logic is largely duplicated in ScssParser._interpolatedString.
        // Most changes here should be mirrored there.

        let quote = match self.toks.next() {
            Some(Token {
                kind: q @ ('\'' | '"'),
                ..
            }) => q,
            Some(Token { pos, .. }) => return Err(("Expected string.", pos).into()),
            None => return Err(("Expected string.", self.toks.current_span()).into()),
        };

        let mut buffer = String::new();

        let mut found_matching_quote = false;

        while let Some(next) = self.toks.peek() {
            if next.kind == quote {
                self.toks.next();
                found_matching_quote = true;
                break;
            } else if next.kind == '\n' || next.kind == '\r' {
                break;
            } else if next.kind == '\\' {
                if matches!(
                    self.toks.peek_n(1),
                    Some(Token {
                        kind: '\n' | '\r',
                        ..
                    })
                ) {
                    self.toks.next();
                    self.toks.next();
                } else {
                    buffer.push(self.consume_escaped_char()?);
                }
            } else {
                self.toks.next();
                buffer.push(next.kind);
            }
        }

        if !found_matching_quote {
            todo!("Expected ${{String.fromCharCode(quote)}}.")
        }

        Ok(buffer)
    }

    fn scan_else(&mut self) -> SassResult<bool> {
        let start = self.toks.cursor();

        self.whitespace_or_comment();

        let before_at = self.toks.cursor();

        if self.consume_char_if_exists('@') {
            if self.scan_identifier("else", true)? {
                return Ok(true);
            }

            if self.scan_identifier("elseif", true)? {
                //     logger.warn(
                //         '@elseif is deprecated and will not be supported in future Sass '
                //         'versions.\n'
                //         '\n'
                //         'Recommendation: @else if',
                //         span: scanner.spanFrom(beforeAt),
                //         deprecation: true);
                //     scanner.position -= 2;
                //     return true;
                todo!()
            }
        }

        self.toks.set_cursor(start);

        Ok(false)
    }

    fn parse_if_rule(
        &mut self,
        child: fn(&mut Self) -> SassResult<AstStmt>,
    ) -> SassResult<AstStmt> {
        let was_in_control_directive = self.flags.in_control_flow();
        self.flags.set(ContextFlags::IN_CONTROL_FLOW, true);
        let condition = self.parse_expression(None, None, None)?.node;
        let body = self.parse_children(child)?;
        self.whitespace();

        let mut clauses = vec![AstIfClause { condition, body }];

        let mut last_clause: Option<Vec<AstStmt>> = None;

        while self.scan_else()? {
            self.whitespace_or_comment();
            if self.scan_identifier("if", false)? {
                self.whitespace_or_comment();
                let condition = self.parse_expression(None, None, None)?.node;
                let body = self.parse_children(child)?;
                clauses.push(AstIfClause { condition, body });
            } else {
                last_clause = Some(self.parse_children(child)?);
                break;
            }
        }

        self.flags
            .set(ContextFlags::IN_CONTROL_FLOW, was_in_control_directive);
        self.whitespace();

        Ok(AstStmt::If(AstIf {
            if_clauses: clauses,
            else_clause: last_clause,
        }))
    }

    fn try_import_modifiers(&mut self) -> SassResult<Option<Interpolation>> {
        // Exit before allocating anything if we're not looking at any modifiers, as
        // is the most common case.
        if !self.looking_at_interpolated_identifier() && !self.toks.next_char_is('(') {
            return Ok(None);
        }

        // var start = scanner.state;
        // var buffer = InterpolationBuffer();
        // while (true) {
        //   if (_lookingAtInterpolatedIdentifier()) {
        //     if (!buffer.isEmpty) buffer.writeCharCode($space);

        //     var identifier = interpolatedIdentifier();
        //     buffer.addInterpolation(identifier);

        //     var name = identifier.asPlain?.toLowerCase();
        //     if (name != "and" && scanner.scanChar($lparen)) {
        //       if (name == "supports") {
        //         var query = _importSupportsQuery();
        //         if (query is! SupportsDeclaration) buffer.writeCharCode($lparen);
        //         buffer.add(SupportsExpression(query));
        //         if (query is! SupportsDeclaration) buffer.writeCharCode($rparen);
        //       } else {
        //         buffer.writeCharCode($lparen);
        //         buffer.addInterpolation(_interpolatedDeclarationValue(
        //             allowEmpty: true, allowSemicolon: true));
        //         buffer.writeCharCode($rparen);
        //       }

        //       scanner.expectChar($rparen);
        //       whitespace();
        //     } else {
        //       whitespace();
        //       if (scanner.scanChar($comma)) {
        //         buffer.write(", ");
        //         buffer.addInterpolation(_mediaQueryList());
        //         return buffer.interpolation(scanner.spanFrom(start));
        //       }
        //     }
        //   } else if (scanner.peekChar() == $lparen) {
        //     if (!buffer.isEmpty) buffer.writeCharCode($space);
        //     buffer.addInterpolation(_mediaQueryList());
        //     return buffer.interpolation(scanner.spanFrom(start));
        //   } else {
        //     return buffer.interpolation(scanner.spanFrom(start));
        //   }
        // }
        todo!()
    }

    fn try_url_contents(&mut self, name: Option<&str>) -> SassResult<Option<Interpolation>> {
        // NOTE: this logic is largely duplicated in Parser.tryUrl. Most changes
        // here should be mirrored there.

        let start = self.toks.cursor();
        if !self.consume_char_if_exists('(') {
            return Ok(None);
        }
        self.whitespace();

        // Match Ruby Sass's behavior: parse a raw URL() if possible, and if not
        // backtrack and re-parse as a function expression.
        let mut buffer = Interpolation::new();
        buffer.add_string(name.unwrap_or("url").to_owned());
        buffer.add_char('(');

        while let Some(next) = self.toks.peek() {
            match next.kind {
                '\\' => buffer.add_string(self.parse_escape(false)?),
                '!' | '%' | '&' | '*'..='~' | '\u{80}'..=char::MAX => {
                    self.toks.next();
                    buffer.add_char(next.kind);
                }
                '#' => {
                    if matches!(self.toks.peek_n(1), Some(Token { kind: '{', .. })) {
                        let interpolation = self.parse_single_interpolation()?;
                        buffer.add_interpolation(interpolation);
                    } else {
                        self.toks.next();
                        buffer.add_char(next.kind);
                    }
                }
                ')' => {
                    self.toks.next();
                    buffer.add_char(next.kind);
                    return Ok(Some(buffer));
                }
                ' ' | '\t' | '\n' | '\r' => {
                    self.whitespace();
                    if !self.toks.next_char_is(')') {
                        break;
                    }
                }
                _ => break,
            }
        }

        self.toks.set_cursor(start);

        Ok(None)
    }

    fn parse_dynamic_url(&mut self) -> SassResult<AstExpr> {
        let start = self.toks.cursor();
        self.expect_identifier("url", false)?;

        Ok(match self.try_url_contents(None)? {
            Some(contents) => AstExpr::String(
                StringExpr(contents, QuoteKind::None),
                self.toks.span_from(start),
            ),
            None => AstExpr::InterpolatedFunction(InterpolatedFunction {
                name: Interpolation::new_plain("url".to_owned()),
                arguments: Box::new(self.parse_argument_invocation(false, false)?),
                span: self.toks.span_from(start),
            }),
        })
    }

    fn parse_import_argument(&mut self) -> SassResult<AstImport> {
        if self.toks.next_char_is('u') || self.toks.next_char_is('U') {
            let url = self.parse_dynamic_url()?;
            self.whitespace_or_comment();
            let modifiers = self.try_import_modifiers()?;
            return Ok(AstImport::Plain(AstPlainCssImport {
                url: Interpolation::new_with_expr(url),
                modifiers,
                span: self.span_before,
            }));
        }

        let start = self.toks.cursor();
        let url = self.parse_string()?;
        let raw_url = self.toks.raw_text(start);
        self.whitespace_or_comment();
        let modifiers = self.try_import_modifiers()?;

        let span = self.toks.span_from(start);

        if is_plain_css_import(&url) || modifiers.is_some() {
            Ok(AstImport::Plain(AstPlainCssImport {
                url: Interpolation::new_plain(raw_url),
                modifiers,
                span,
            }))
        } else {
            // todo: try parseImportUrl
            Ok(AstImport::Sass(AstSassImport { url, span }))
        }
    }

    fn parse_import_rule(&mut self, start: usize) -> SassResult<AstStmt> {
        let mut imports = Vec::new();

        loop {
            self.whitespace_or_comment();
            let argument = self.parse_import_argument()?;

            // todo: _inControlDirective
            if (self.flags.in_control_flow() || self.flags.in_mixin()) && argument.is_dynamic() {
                self.parse_disallowed_at_rule(start)?;
            }

            imports.push(argument);
            self.whitespace_or_comment();

            if !self.consume_char_if_exists(',') {
                break;
            }
        }

        Ok(AstStmt::ImportRule(AstImportRule { imports }))
    }

    fn parse_public_identifier(&mut self) -> SassResult<String> {
        let start = self.toks.cursor();
        let ident = self.__parse_identifier(true, false)?;
        Self::assert_public(&ident, self.toks.span_from(start))?;

        Ok(ident)
    }

    fn parse_include_rule(&mut self) -> SassResult<AstStmt> {
        let mut namespace: Option<Spanned<Identifier>> = None;

        let name_start = self.toks.cursor();
        let mut name = self.__parse_identifier(false, false)?;

        if self.consume_char_if_exists('.') {
            let namespace_span = self.toks.span_from(name_start);
            namespace = Some(Spanned {
                node: Identifier::from(name),
                span: namespace_span,
            });
            name = self.parse_public_identifier()?;
        } else {
            name = name.replace('_', "-");
        }

        let name = Identifier::from(name);
        let name_span = self.toks.span_from(name_start);

        self.whitespace_or_comment();

        let args = if self.toks.next_char_is('(') {
            self.parse_argument_invocation(true, false)?
        } else {
            ArgumentInvocation::empty(self.toks.current_span())
        };

        self.whitespace_or_comment();

        let content_args = if self.scan_identifier("using", false)? {
            self.whitespace_or_comment();
            let args = self.parse_argument_declaration()?;
            self.whitespace_or_comment();
            Some(args)
        } else {
            None
        };

        let mut content_block: Option<AstContentBlock> = None;

        if content_args.is_some() || self.looking_at_children() {
            let content_args = content_args.unwrap_or_else(ArgumentDeclaration::empty);
            let was_in_content_block = self.flags.in_content_block();
            self.flags.set(ContextFlags::IN_CONTENT_BLOCK, true);
            let body = self.with_children(Self::__parse_stmt)?;
            content_block = Some(AstContentBlock {
                args: content_args,
                body,
            });
            self.flags
                .set(ContextFlags::IN_CONTENT_BLOCK, was_in_content_block);
        } else {
            self.expect_statement_separator(None)?;
        }

        Ok(AstStmt::Include(AstInclude {
            namespace,
            name: Spanned {
                node: name,
                span: name_span,
            },
            args,
            content: content_block,
        }))
    }

    fn parse_media_rule(&mut self) -> SassResult<AstStmt> {
        let query = self.parse_media_query_list()?;

        let body = self.with_children(Self::__parse_stmt)?;

        Ok(AstStmt::Media(AstMedia { query, body }))
    }

    fn parse_interpolated_string(&mut self) -> SassResult<Spanned<StringExpr>> {
        // NOTE: this logic is largely duplicated in ScssParser.interpolatedString.
        // Most changes here should be mirrored there.

        let quote = match self.toks.next() {
            Some(Token {
                kind: kind @ ('"' | '\''),
                ..
            }) => kind,
            Some(..) | None => todo!("Expected string."),
        };

        let mut buffer = Interpolation::new();

        let mut found_match = false;

        while let Some(next) = self.toks.peek() {
            match next.kind {
                c if c == quote => {
                    self.toks.next();
                    found_match = true;
                    break;
                }
                '\n' => break,
                '\\' => {
                    match self.toks.peek_n(1) {
                        // todo: if (second == $cr) scanner.scanChar($lf);
                        // we basically need to stop normalizing to gain parity
                        Some(Token { kind: '\n', .. }) => {
                            self.toks.next();
                            self.toks.next();
                        }
                        _ => buffer.add_char(self.consume_escaped_char()?),
                    }
                }
                '#' => {
                    if matches!(self.toks.peek_n(1), Some(Token { kind: '{', .. })) {
                        buffer.add_interpolation(self.parse_single_interpolation()?);
                    } else {
                        self.toks.next();
                        buffer.add_token(next);
                    }
                }
                _ => {
                    buffer.add_token(next);
                    self.toks.next();
                }
            }
        }

        if !found_match {
            todo!("Expected ${{String.fromCharCode(quote)}}.")
        }

        Ok(Spanned {
            node: StringExpr(buffer, QuoteKind::Quoted),
            span: self.span_before,
        })
    }

    fn parse_return_rule(&mut self) -> SassResult<AstStmt> {
        let value = self.parse_expression(None, None, None)?;
        self.expect_statement_separator(None)?;
        Ok(AstStmt::Return(AstReturn {
            val: value.node,
            span: value.span,
        }))
    }

    fn parse_mixin_rule(&mut self, start: usize) -> SassResult<AstStmt> {
        let name = Identifier::from(self.__parse_identifier(true, false)?);
        self.whitespace_or_comment();
        let args = if self.toks.next_char_is('(') {
            self.parse_argument_declaration()?
        } else {
            ArgumentDeclaration::empty()
        };

        if self.flags.in_mixin() || self.flags.in_content_block() {
            return Err((
                "Mixins may not contain mixin declarations.",
                self.toks.span_from(start),
            )
                .into());
        } else if self.flags.in_control_flow() {
            return Err((
                "Mixins may not be declared in control directives.",
                self.toks.span_from(start),
            )
                .into());
        }

        self.whitespace_or_comment();

        let old_found_content_rule = self.flags.found_content_rule();
        self.flags.set(ContextFlags::FOUND_CONTENT_RULE, false);
        self.flags.set(ContextFlags::IN_MIXIN, true);

        let body = self.with_children(Self::__parse_stmt)?;

        let has_content = self.flags.found_content_rule();

        self.flags
            .set(ContextFlags::FOUND_CONTENT_RULE, old_found_content_rule);
        self.flags.set(ContextFlags::IN_MIXIN, false);

        Ok(AstStmt::Mixin(AstMixin {
            name,
            args,
            body,
            has_content,
        }))
    }

    fn parse_moz_document_rule(&mut self, name: Interpolation) -> SassResult<AstStmt> {
        todo!()
    }

    fn unknown_at_rule(&mut self, name: Interpolation) -> SassResult<AstStmt> {
        let was_in_unknown_at_rule = self.flags.in_unknown_at_rule();
        self.flags.set(ContextFlags::IN_UNKNOWN_AT_RULE, true);

        let value: Option<Interpolation> =
            if !self.toks.next_char_is('!') && !self.at_end_of_statement() {
                Some(self.almost_any_value(false)?)
            } else {
                None
            };

        let children = if self.looking_at_children() {
            Some(self.with_children(Self::__parse_stmt)?)
        } else {
            self.expect_statement_separator(None)?;
            None
        };

        self.flags
            .set(ContextFlags::IN_UNKNOWN_AT_RULE, was_in_unknown_at_rule);

        Ok(AstStmt::UnknownAtRule(AstUnknownAtRule {
            name,
            value,
            children,
            span: self.span_before,
        }))
    }

    fn parse_supports_rule(&mut self) -> SassResult<AstStmt> {
        todo!()
    }

    fn parse_warn_rule(&mut self) -> SassResult<AstStmt> {
        let value = self.parse_expression(None, None, None)?;
        self.expect_statement_separator(Some("@warn rule"))?;
        Ok(AstStmt::Warn(AstWarn {
            value: value.node,
            span: value.span,
        }))
    }

    fn parse_while_rule(
        &mut self,
        child: fn(&mut Self) -> SassResult<AstStmt>,
    ) -> SassResult<AstStmt> {
        let was_in_control_directive = self.flags.in_control_flow();
        self.flags.set(ContextFlags::IN_CONTROL_FLOW, true);

        let condition = self.parse_expression(None, None, None)?.node;

        let body = self.with_children(child)?;

        self.flags
            .set(ContextFlags::IN_CONTROL_FLOW, was_in_control_directive);

        Ok(AstStmt::While(AstWhile { condition, body }))
    }
    fn parse_forward_rule(&mut self) -> SassResult<AstStmt> {
        todo!()
    }

    fn parse_url_string(&mut self) -> SassResult<String> {
        // todo: real uri parsing
        self.parse_string()
    }

    fn use_namespace(&mut self, url: &Path, start: usize) -> SassResult<Option<String>> {
        if self.scan_identifier("as", false)? {
            self.whitespace_or_comment();
            return Ok(if self.consume_char_if_exists('*') {
                None
            } else {
                Some(self.__parse_identifier(false, false)?)
            });
        }

        let base_name = url
            .file_name()
            .map(ToOwned::to_owned)
            .unwrap_or_else(OsString::new);
        let base_name = base_name.to_string_lossy();
        let dot = base_name.find('.');

        let start = if base_name.starts_with('_') { 1 } else { 0 };
        let end = dot.unwrap_or(base_name.len());
        let namespace = if url.to_string_lossy().starts_with("sass:") {
            return Ok(Some(url.to_string_lossy().into_owned()));
        } else {
            &base_name[start..end]
        };

        let mut toks = Lexer::new(
            namespace
                .chars()
                .map(|x| Token::new(self.span_before, x))
                .collect(),
        );

        let identifier = Parser {
            toks: &mut toks,
            map: self.map,
            path: self.path,
            is_plain_css: self.is_plain_css,
            span_before: self.span_before,
            flags: self.flags,
            options: self.options,
        }
        .__parse_identifier(false, false);

        match (identifier, toks.peek().is_none()) {
            (Ok(i), true) => Ok(Some(i)),
            _ => {
                Err((
                    format!("The default namespace \"{namespace}\" is not a valid Sass identifier.\n\nRecommendation: add an \"as\" clause to define an explicit namespace."),
                    self.toks.span_from(start)
                ).into())
            }
        }
    }

    fn parse_configuration(
        &mut self,
        // default=false
        allow_guarded: bool,
    ) -> SassResult<Option<Vec<ConfiguredVariable>>> {
        if !self.scan_identifier("with", false)? {
            return Ok(None);
        }

        let mut variable_names = HashSet::new();
        let mut configuration = Vec::new();
        self.whitespace_or_comment();
        self.expect_char('(')?;

        loop {
            self.whitespace_or_comment();
            let var_start = self.toks.cursor();
            let name = Identifier::from(self.parse_variable_name()?);
            let name_span = self.toks.span_from(var_start);
            self.whitespace_or_comment();
            self.expect_char(':')?;
            self.whitespace_or_comment();
            let expr = self.parse_expression_until_comma(false)?;

            let mut is_guarded = false;
            let flag_start = self.toks.cursor();
            if allow_guarded && self.consume_char_if_exists('!') {
                let flag = self.__parse_identifier(false, false)?;
                if flag == "default" {
                    is_guarded = true;
                    self.whitespace_or_comment();
                } else {
                    return Err(("Invalid flag name.", self.toks.span_from(flag_start)).into());
                }
            }

            let span = self.toks.span_from(var_start);
            if variable_names.contains(&name) {
                return Err(("The same variable may only be configured once.", span).into());
            }

            variable_names.insert(name);
            configuration.push(ConfiguredVariable {
                name: Spanned {
                    node: name,
                    span: name_span,
                },
                expr,
                is_guarded,
            });

            if !self.consume_char_if_exists(',') {
                break;
            }
            self.whitespace_or_comment();
            if !self.looking_at_expression() {
                break;
            }
        }

        self.expect_char(')')?;

        Ok(Some(configuration))
    }

    fn parse_use_rule(&mut self, start: usize) -> SassResult<AstStmt> {
        let url = self.parse_url_string()?;
        self.whitespace_or_comment();

        let path = PathBuf::from(url.clone());

        let namespace = self.use_namespace(path.as_ref(), start)?;
        self.whitespace_or_comment();
        let configuration = self.parse_configuration(false)?;

        self.expect_statement_separator(Some("@use rule"))?;

        let span = self.toks.span_from(start);

        if !self.flags.is_use_allowed() {
            return Err((
                "@use rules must be written before any other rules.",
                self.toks.span_from(start),
            )
                .into());
        }

        self.expect_statement_separator(Some("@use rule"))?;

        Ok(AstStmt::Use(AstUseRule {
            url: path,
            namespace,
            configuration: configuration.unwrap_or_default(),
            span,
        }))
    }

    fn parse_at_rule(
        &mut self,
        child: fn(&mut Self) -> SassResult<AstStmt>,
    ) -> SassResult<AstStmt> {
        // NOTE: this logic is largely duplicated in CssParser.atRule. Most changes
        // here should be mirrored there.

        let start = self.toks.cursor();

        self.expect_char('@')?;
        let name = self.parse_interpolated_identifier()?;
        self.whitespace_or_comment();

        // We want to set [_isUseAllowed] to `false` *unless* we're parsing
        // `@charset`, `@forward`, or `@use`. To avoid double-comparing the rule
        // name, we always set it to `false` and then set it back to its previous
        // value if we're parsing an allowed rule.
        let was_use_allowed = self.flags.is_use_allowed();
        self.flags.set(ContextFlags::IS_USE_ALLOWED, false);

        match name.as_plain() {
            Some("at-root") => self.parse_at_root_rule(),
            Some("content") => self.parse_content_rule(start),
            Some("debug") => self.parse_debug_rule(),
            Some("each") => self.parse_each_rule(child),
            Some("else") | Some("return") => self.parse_disallowed_at_rule(start),
            Some("error") => self.parse_error_rule(),
            Some("extend") => self.parse_extend_rule(start),
            Some("for") => self.parse_for_rule(child),
            Some("forward") => {
                self.flags
                    .set(ContextFlags::IS_USE_ALLOWED, was_use_allowed);
                // if (!root) {
                //     _disallowedAtRule();
                // }
                self.parse_forward_rule()
            }
            Some("function") => self.parse_function_rule(start),
            Some("if") => self.parse_if_rule(child),
            Some("import") => self.parse_import_rule(start),
            Some("include") => self.parse_include_rule(),
            Some("media") => self.parse_media_rule(),
            Some("mixin") => self.parse_mixin_rule(start),
            Some("-moz-document") => self.parse_moz_document_rule(name),
            Some("supports") => self.parse_supports_rule(),
            Some("use") => {
                self.flags
                    .set(ContextFlags::IS_USE_ALLOWED, was_use_allowed);
                // if (!root) {
                //     _disallowedAtRule();
                // }
                self.parse_use_rule(start)
            }
            Some("warn") => self.parse_warn_rule(),
            Some("while") => self.parse_while_rule(child),
            Some(..) | None => self.unknown_at_rule(name),
        }
    }

    fn __parse_stmt(&mut self) -> SassResult<AstStmt> {
        match self.toks.peek() {
            Some(Token { kind: '@', .. }) => self.parse_at_rule(Self::__parse_stmt),
            // todo: indented stuff
            Some(Token { kind: '+', .. }) => self.parse_style_rule(None),
            Some(Token { kind: '=', .. }) => todo!(),
            Some(Token { kind: '}', .. }) => todo!(),
            _ => {
                if self.flags.in_style_rule()
                    || self.flags.in_unknown_at_rule()
                    || self.flags.in_mixin()
                    || self.flags.in_content_block()
                {
                    self.parse_declaration_or_style_rule()
                } else {
                    self.parse_variable_declaration_or_style_rule()
                }
            }
        }
    }

    fn parse_declaration_or_style_rule(&mut self) -> SassResult<AstStmt> {
        if self.flags.in_plain_css()
            && self.flags.in_style_rule()
            && !self.flags.in_unknown_at_rule()
        {
            return self.parse_property_or_variable_declaration(true);
        }

        match self.parse_declaration_or_buffer()? {
            DeclarationOrBuffer::Stmt(s) => Ok(s),
            DeclarationOrBuffer::Buffer(existing_buffer) => {
                self.parse_style_rule(Some(existing_buffer))
            }
        }
    }

    fn parse_property_or_variable_declaration(
        &mut self,
        // default=true
        parse_custom_properties: bool,
    ) -> SassResult<AstStmt> {
        let start = self.toks.cursor();

        let name = if matches!(
            self.toks.peek(),
            Some(Token {
                kind: ':' | '*' | '.',
                ..
            })
        ) || (matches!(self.toks.peek(), Some(Token { kind: '#', .. }))
            && !matches!(self.toks.peek_n(1), Some(Token { kind: '{', .. })))
        {
            // Allow the "*prop: val", ":prop: val", "#prop: val", and ".prop: val"
            // hacks.
            let mut name_buffer = Interpolation::new();
            name_buffer.add_token(self.toks.next().unwrap());
            name_buffer.add_string(self.raw_text(Self::whitespace_or_comment));
            name_buffer.add_interpolation(self.parse_interpolated_identifier()?);
            name_buffer
        } else if !self.is_plain_css {
            match self.parse_variable_declaration_or_interpolation()? {
                VariableDeclOrInterpolation::Interpolation(interpolation) => interpolation,
                VariableDeclOrInterpolation::VariableDecl(decl) => {
                    return Ok(AstStmt::VariableDecl(decl))
                }
            }
        } else {
            self.parse_interpolated_identifier()?
        };

        self.whitespace_or_comment();
        self.expect_char(':')?;

        if parse_custom_properties && name.initial_plain().starts_with("--") {
            let interpolation = self.parse_interpolated_declaration_value(false, false, true)?;
            let value_span = self.toks.span_from(start);
            let value = AstExpr::String(StringExpr(interpolation, QuoteKind::None), value_span)
                .span(value_span);
            self.expect_statement_separator(Some("custom property"))?;
            return Ok(AstStmt::Style(AstStyle {
                name,
                value: Some(value),
                body: Vec::new(),
                span: value_span,
            }));
        }

        self.whitespace_or_comment();

        if self.looking_at_children() {
            if self.is_plain_css {
                return Err((
                    "Nested declarations aren't allowed in plain CSS.",
                    self.toks.current_span(),
                )
                    .into());
            }

            let children = self.with_children(Self::parse_declaration_child)?;

            assert!(
                !name.initial_plain().starts_with("--"),
                "todo: Declarations whose names begin with \"--\" may not be nested"
            );

            return Ok(AstStmt::Style(AstStyle {
                name,
                value: None,
                body: children,
                span: self.toks.span_from(start),
            }));
        }

        let value = self.parse_expression(None, None, None)?;
        if self.looking_at_children() {
            if self.is_plain_css {
                return Err((
                    "Nested declarations aren't allowed in plain CSS.",
                    self.toks.current_span(),
                )
                    .into());
            }

            let children = self.with_children(Self::parse_declaration_child)?;

            assert!(
                !(name.initial_plain().starts_with("--")
                    && !matches!(value.node, AstExpr::String(..))),
                "todo: Declarations whose names begin with \"--\" may not be nested"
            );

            Ok(AstStmt::Style(AstStyle {
                name,
                value: Some(value),
                body: children,
                span: self.toks.span_from(start),
            }))
        } else {
            self.expect_statement_separator(None)?;
            Ok(AstStmt::Style(AstStyle {
                name,
                value: Some(value),
                body: Vec::new(),
                span: self.toks.span_from(start),
            }))
        }
    }

    fn parse_single_interpolation(&mut self) -> SassResult<Interpolation> {
        self.expect_char('#')?;
        self.expect_char('{')?;
        self.whitespace_or_comment();
        let contents = self.parse_expression(None, None, None)?;
        self.expect_char('}')?;

        if self.flags.in_plain_css() {
            return Err(("Interpolation isn't allowed in plain CSS.", contents.span).into());
        }

        let mut interpolation = Interpolation::new();
        interpolation
            .contents
            .push(InterpolationPart::Expr(contents.node));

        Ok(interpolation)
    }

    fn parse_interpolated_identifier_body(&mut self, buffer: &mut Interpolation) -> SassResult<()> {
        while let Some(next) = self.toks.peek() {
            match next.kind {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-' | '\u{80}'..=std::char::MAX => {
                    buffer.add_token(next);
                    self.toks.next();
                }
                '\\' => {
                    buffer.add_string(self.parse_escape(false)?);
                }
                '#' if matches!(self.toks.peek_n(1), Some(Token { kind: '{', .. })) => {
                    buffer.add_interpolation(self.parse_single_interpolation()?);
                }
                _ => break,
            }
        }

        Ok(())
    }

    fn parse_interpolated_identifier(&mut self) -> SassResult<Interpolation> {
        let mut buffer = Interpolation::new();

        if self.consume_char_if_exists('-') {
            buffer.add_char('-');

            if self.consume_char_if_exists('-') {
                buffer.add_char('-');
                self.parse_interpolated_identifier_body(&mut buffer)?;
                return Ok(buffer);
            }
        }

        match self.toks.peek() {
            Some(tok) if is_name_start(tok.kind) => {
                buffer.add_token(tok);
                self.toks.next();
            }
            Some(Token { kind: '\\', .. }) => {
                buffer.add_string(self.parse_escape(true)?);
            }
            Some(Token { kind: '#', .. })
                if matches!(self.toks.peek_n(1), Some(Token { kind: '{', .. })) =>
            {
                buffer.add_interpolation(self.parse_single_interpolation()?);
            }
            Some(..) | None => {
                return Err(("Expected identifier.", self.toks.current_span()).into())
            }
        }

        self.parse_interpolated_identifier_body(&mut buffer)?;

        Ok(buffer)
    }

    fn fallible_raw_text<T>(
        &mut self,
        func: impl Fn(&mut Self) -> SassResult<T>,
    ) -> SassResult<String> {
        let start = self.toks.cursor();
        func(self)?;
        Ok(self.toks.raw_text(start))
    }

    pub(crate) fn raw_text<T>(&mut self, func: impl Fn(&mut Self) -> T) -> String {
        let start = self.toks.cursor();
        func(self);
        self.toks.raw_text(start)
    }

    fn looking_at_interpolated_identifier(&mut self) -> bool {
        let first = match self.toks.peek() {
            Some(Token { kind: '\\', .. }) => return true,
            Some(Token { kind: '#', .. }) => {
                return matches!(self.toks.peek_n(1), Some(Token { kind: '{', .. }))
            }
            Some(Token { kind, .. }) if is_name_start(kind) => return true,
            Some(tok) => tok,
            None => return false,
        };

        if first.kind != '-' {
            return false;
        }

        match self.toks.peek_n(1) {
            Some(Token { kind: '#', .. }) => {
                matches!(self.toks.peek_n(2), Some(Token { kind: '{', .. }))
            }
            Some(Token {
                kind: '\\' | '-', ..
            }) => true,
            Some(Token { kind, .. }) => is_name_start(kind),
            None => false,
        }
    }

    fn skip_loud_comment(&mut self) -> SassResult<()> {
        self.expect_char('/')?;
        self.expect_char('*')?;

        while let Some(next) = self.toks.next() {
            if next.kind != '*' {
                continue;
            }

            while self.consume_char_if_exists('*') {}

            if self.consume_char_if_exists('/') {
                break;
            }
        }

        Ok(())
    }

    fn parse_loud_comment(&mut self) -> SassResult<AstLoudComment> {
        let start = self.toks.cursor();
        self.expect_char('/')?;
        self.expect_char('*')?;

        let mut buffer = Interpolation::new_plain("/*".to_owned());

        while let Some(tok) = self.toks.peek() {
            match tok.kind {
                '#' => {
                    if matches!(self.toks.peek_n(1), Some(Token { kind: '{', .. })) {
                        buffer.add_interpolation(self.parse_single_interpolation()?);
                    } else {
                        self.toks.next();
                        buffer.add_token(tok);
                    }
                }
                '*' => {
                    self.toks.next();
                    buffer.add_token(tok);

                    if self.consume_char_if_exists('/') {
                        buffer.add_char('/');

                        return Ok(AstLoudComment {
                            text: buffer,
                            span: self.toks.span_from(start),
                        });
                    }
                }
                '\r' => {
                    self.toks.next();
                    // todo: does \r even exist at this point? (removed by lexer)
                    if !self.toks.next_char_is('\n') {
                        buffer.add_char('\n');
                    }
                }
                _ => {
                    buffer.add_token(tok);
                    self.toks.next();
                }
            }
        }

        return Err(("expected more input.", self.toks.current_span()).into());
    }

    fn expect_statement_separator(&mut self, _name: Option<&str>) -> SassResult<()> {
        self.whitespace();
        match self.toks.peek() {
            Some(Token {
                kind: ';' | '}', ..
            })
            | None => Ok(()),
            _ => {
                self.expect_char(';')?;
                Ok(())
            }
        }
    }

    fn parse_interpolated_declaration_value(
        &mut self,
        // default=false
        allow_semicolon: bool,
        // default=false
        allow_empty: bool,
        // default=true
        allow_colon: bool,
    ) -> SassResult<Interpolation> {
        let mut buffer = Interpolation::new();

        let mut brackets = Vec::new();
        let mut wrote_newline = false;

        while let Some(tok) = self.toks.peek() {
            match tok.kind {
                '\\' => {
                    buffer.add_string(self.parse_escape(true)?);
                    wrote_newline = false;
                }
                '"' | '\'' => {
                    buffer.add_interpolation(
                        self.parse_interpolated_string()?
                            .node
                            .as_interpolation(self.span_before, false),
                    );
                    wrote_newline = false;
                }
                '/' => {
                    if matches!(self.toks.peek_n(1), Some(Token { kind: '*', .. })) {
                        let comment = self.fallible_raw_text(Self::skip_loud_comment)?;
                        buffer.add_string(comment);
                    } else {
                        self.toks.next();
                        buffer.add_token(tok);
                    }

                    wrote_newline = false;
                }
                '#' => {
                    if matches!(self.toks.peek_n(1), Some(Token { kind: '{', .. })) {
                        // Add a full interpolated identifier to handle cases like
                        // "#{...}--1", since "--1" isn't a valid identifier on its own.
                        buffer.add_interpolation(self.parse_interpolated_identifier()?);
                    } else {
                        self.toks.next();
                        buffer.add_token(tok);
                    }

                    wrote_newline = false;
                }
                ' ' | '\t' => {
                    if wrote_newline
                        || !matches!(
                            self.toks.peek_n(1),
                            Some(Token {
                                kind: ' ' | '\r' | '\t' | '\n',
                                ..
                            })
                        )
                    {
                        self.toks.next();
                        buffer.add_token(tok);
                    } else {
                        self.toks.next();
                    }
                }
                '\n' | '\r' => {
                    if !matches!(
                        self.toks.peek_n_backwards(1),
                        Some(Token {
                            kind: '\r' | '\n',
                            ..
                        })
                    ) {
                        buffer.add_char('\n');
                    }
                    self.toks.next();
                    wrote_newline = true;
                }
                '(' | '{' | '[' => {
                    self.toks.next();
                    buffer.add_token(tok);
                    brackets.push(opposite_bracket(tok.kind));
                    wrote_newline = false;
                }
                ')' | '}' | ']' => {
                    if brackets.is_empty() {
                        break;
                    }
                    buffer.add_token(tok);
                    self.expect_char(brackets.pop().unwrap())?;
                    wrote_newline = false;
                }
                ';' => {
                    if !allow_semicolon && brackets.is_empty() {
                        break;
                    }
                    buffer.add_token(tok);
                    self.toks.next();
                    wrote_newline = false;
                }
                ':' => {
                    if !allow_colon && brackets.is_empty() {
                        break;
                    }
                    buffer.add_token(tok);
                    self.toks.next();
                    wrote_newline = false;
                }
                'u' | 'U' => {
                    let before_url = self.toks.cursor();

                    if !self.scan_identifier("url", false)? {
                        buffer.add_token(tok);
                        self.toks.next();
                        wrote_newline = false;
                        continue;
                    }

                    match self.try_url_contents(None)? {
                        Some(contents) => {
                            buffer.add_interpolation(contents);
                        }
                        None => {
                            self.toks.set_cursor(before_url);
                            buffer.add_token(tok);
                            self.toks.next();
                        }
                    }

                    wrote_newline = false;
                }
                _ => {
                    if self.looking_at_identifier() {
                        buffer.add_string(self.__parse_identifier(false, false)?);
                    } else {
                        buffer.add_token(tok);
                        self.toks.next();
                    }
                    wrote_newline = false;
                }
            }
        }

        if let Some(&last) = brackets.last() {
            self.expect_char(last)?;
        }

        if !allow_empty && buffer.contents.is_empty() {
            todo!("Expected token.");
        }

        Ok(buffer)
    }

    fn looking_at_children(&self) -> bool {
        matches!(self.toks.peek(), Some(Token { kind: '{', .. }))
    }

    fn parse_expression_until_comma(
        &mut self,
        // default=false
        single_equals: bool,
    ) -> SassResult<Spanned<AstExpr>> {
        ValueParser::parse_expression(
            self,
            Some(&|parser| Ok(matches!(parser.toks.peek(), Some(Token { kind: ',', .. })))),
            false,
            single_equals,
        )
    }

    fn parse_argument_invocation(
        &mut self,
        for_mixin: bool,
        allow_empty_second_arg: bool,
    ) -> SassResult<ArgumentInvocation> {
        let start = self.toks.cursor();

        self.expect_char('(')?;
        self.whitespace_or_comment();

        let mut positional = Vec::new();
        let mut named = BTreeMap::new();

        let mut rest: Option<AstExpr> = None;
        let mut keyword_rest: Option<AstExpr> = None;

        while self.looking_at_expression() {
            let expression = self.parse_expression_until_comma(!for_mixin)?;
            self.whitespace_or_comment();

            if expression.node.is_variable() && self.consume_char_if_exists(':') {
                let name = match expression.node {
                    AstExpr::Variable { name, .. } => name,
                    _ => unreachable!(),
                };

                self.whitespace_or_comment();
                if named.contains_key(&name.node) {
                    todo!("Duplicate argument.");
                }

                named.insert(
                    name.node,
                    self.parse_expression_until_comma(!for_mixin)?.node,
                );
            } else if self.consume_char_if_exists('.') {
                self.expect_char('.')?;
                self.expect_char('.')?;

                if rest.is_none() {
                    rest = Some(expression.node);
                } else {
                    keyword_rest = Some(expression.node);
                    self.whitespace_or_comment();
                    break;
                }
            } else if !named.is_empty() {
                todo!("Positional arguments must come before keyword arguments.");
            } else {
                positional.push(expression.node);
            }

            self.whitespace_or_comment();
            if !self.consume_char_if_exists(',') {
                break;
            }
            self.whitespace_or_comment();

            if allow_empty_second_arg
                && positional.len() == 1
                && named.is_empty()
                && rest.is_none()
                && matches!(self.toks.peek(), Some(Token { kind: ')', .. }))
            {
                positional.push(AstExpr::String(
                    StringExpr(Interpolation::new(), QuoteKind::None),
                    self.toks.current_span(),
                ));
                break;
            }
        }

        self.expect_char(')')?;

        Ok(ArgumentInvocation {
            positional,
            named,
            rest,
            keyword_rest,
            span: self.toks.span_from(start),
        })
    }

    fn parse_expression(
        &mut self,
        parse_until: Option<Predicate>,
        inside_bracketed_list: Option<bool>,
        single_equals: Option<bool>,
    ) -> SassResult<Spanned<AstExpr>> {
        ValueParser::parse_expression(
            self,
            parse_until,
            inside_bracketed_list.unwrap_or(false),
            single_equals.unwrap_or(false),
        )
    }

    fn at_end_of_statement(&self) -> bool {
        matches!(
            self.toks.peek(),
            Some(Token {
                kind: ';' | '}' | '{',
                ..
            }) | None
        )
    }

    fn parse_declaration_or_buffer(&mut self) -> SassResult<DeclarationOrBuffer> {
        let start = self.toks.cursor();
        let mut name_buffer = Interpolation::new();

        // Allow the "*prop: val", ":prop: val", "#prop: val", and ".prop: val"
        // hacks.
        let first = self.toks.peek();
        let mut starts_with_punctuation = false;

        if matches!(
            first,
            Some(Token {
                kind: ':' | '*' | '.',
                ..
            })
        ) || (matches!(first, Some(Token { kind: '#', .. }))
            && !matches!(self.toks.peek_n(1), Some(Token { kind: '{', .. })))
        {
            starts_with_punctuation = true;
            name_buffer.add_token(self.toks.next().unwrap());
            name_buffer.add_string(self.raw_text(Self::whitespace_or_comment));
        }

        if !self.looking_at_interpolated_identifier() {
            return Ok(DeclarationOrBuffer::Buffer(name_buffer));
        }

        let variable_or_interpolation = if starts_with_punctuation {
            VariableDeclOrInterpolation::Interpolation(self.parse_interpolated_identifier()?)
        } else {
            self.parse_variable_declaration_or_interpolation()?
        };

        match variable_or_interpolation {
            VariableDeclOrInterpolation::Interpolation(int) => name_buffer.add_interpolation(int),
            VariableDeclOrInterpolation::VariableDecl(v) => {
                return Ok(DeclarationOrBuffer::Stmt(AstStmt::VariableDecl(v)))
            }
        }

        self.flags.set(ContextFlags::IS_USE_ALLOWED, false);

        if self.next_matches("/*") {
            name_buffer.add_string(self.fallible_raw_text(Self::skip_loud_comment)?);
        }

        let mut mid_buffer = String::new();
        mid_buffer.push_str(&self.raw_text(Self::whitespace_or_comment));

        if !self.consume_char_if_exists(':') {
            if !mid_buffer.is_empty() {
                name_buffer.add_char(' ');
            }
            return Ok(DeclarationOrBuffer::Buffer(name_buffer));
        }
        mid_buffer.push(':');

        // Parse custom properties as declarations no matter what.
        if name_buffer.initial_plain().starts_with("--") {
            let value_start = self.toks.cursor();
            let value = self.parse_interpolated_declaration_value(false, false, true)?;
            let value_span = self.toks.span_from(value_start);
            self.expect_statement_separator(Some("custom property"))?;
            return Ok(DeclarationOrBuffer::Stmt(AstStmt::Style(AstStyle {
                name: name_buffer,
                value: Some(
                    AstExpr::String(StringExpr(value, QuoteKind::None), value_span)
                        .span(value_span),
                ),
                span: self.toks.span_from(start),
                body: Vec::new(),
            })));
        }

        if self.consume_char_if_exists(':') {
            name_buffer.add_string(mid_buffer);
            name_buffer.add_char(':');
            return Ok(DeclarationOrBuffer::Buffer(name_buffer));
        }

        let post_colon_whitespace = self.raw_text(Self::whitespace_or_comment);
        if self.looking_at_children() {
            let body = self.with_children(Self::parse_declaration_child)?;
            return Ok(DeclarationOrBuffer::Stmt(AstStmt::Style(AstStyle {
                name: name_buffer,
                value: None,
                span: self.toks.span_from(start),
                body,
            })));
        }

        mid_buffer.push_str(&post_colon_whitespace);
        let could_be_selector =
            post_colon_whitespace.is_empty() && self.looking_at_interpolated_identifier();

        let before_decl = self.toks.cursor();
        let value = loop {
            let value = self.parse_expression(None, None, None);

            if self.looking_at_children() {
                // Properties that are ambiguous with selectors can't have additional
                // properties nested beneath them, so we force an error. This will be
                // caught below and cause the text to be reparsed as a selector.
                if !could_be_selector {
                    break value?;
                }
            } else if self.at_end_of_statement() {
                // Force an exception if there isn't a valid end-of-property character
                // but don't consume that character. This will also cause the text to be
                // reparsed.
                break value?;
            }

            self.expect_statement_separator(None);

            if !could_be_selector {
                break value?;
            }

            self.toks.set_cursor(before_decl);
            let additional = self.almost_any_value(false)?;
            if self.toks.next_char_is(';') {
                break value?;
            }

            name_buffer.add_string(mid_buffer);
            name_buffer.add_interpolation(additional);
            return Ok(DeclarationOrBuffer::Buffer(name_buffer));
        };

        // = match self.parse_expression(None, None, None) {
        //     Ok(value) => {
        //         if self.looking_at_children() {
        //             // Properties that are ambiguous with selectors can't have additional
        //             // properties nested beneath them, so we force an error. This will be
        //             // caught below and cause the text to be reparsed as a selector.
        //             if could_be_selector {
        //                 self.expect_statement_separator(None).unwrap();
        //             } else if !self.at_end_of_statement() {
        //                 // Force an exception if there isn't a valid end-of-property character
        //                 // but don't consume that character. This will also cause the text to be
        //                 // reparsed.
        //                 // todo: unwrap here is invalid
        //                 self.expect_statement_separator(None).unwrap();
        //             }
        //         }
        //         value
        //     }
        //     Err(e) => {
        //         if !could_be_selector {
        //             return Err(e);
        //         }

        //         //   // If the value would be followed by a semicolon, it's definitely supposed
        //         //   // to be a property, not a selector.
        //         //   scanner.state = beforeDeclaration;
        //         //   var additional = almostAnyValue();
        //         //   if (!indented && scanner.peekChar() == $semicolon) rethrow;

        //         //   nameBuffer.write(midBuffer);
        //         //   nameBuffer.addInterpolation(additional);
        //         //   return nameBuffer;
        //         todo!()
        //     }
        // };

        if self.looking_at_children() {
            let body = self.with_children(Self::parse_declaration_child)?;
            Ok(DeclarationOrBuffer::Stmt(AstStmt::Style(AstStyle {
                name: name_buffer,
                value: Some(value),
                span: self.span_before,
                body,
            })))
        } else {
            self.expect_statement_separator(None)?;
            Ok(DeclarationOrBuffer::Stmt(AstStmt::Style(AstStyle {
                name: name_buffer,
                value: Some(value),
                span: self.span_before,
                body: Vec::new(),
            })))
        }
    }

    fn parse_declaration_child(&mut self) -> SassResult<AstStmt> {
        let start = self.toks.cursor();

        if self.toks.next_char_is('@') {
            self.parse_declaration_at_rule(start)
        } else {
            self.parse_property_or_variable_declaration(false)
        }
    }

    fn parse_plain_at_rule_name(&mut self) -> SassResult<String> {
        self.expect_char('@')?;
        let name = self.__parse_identifier(false, false)?;
        self.whitespace_or_comment();
        Ok(name)
    }

    fn parse_declaration_at_rule(&mut self, start: usize) -> SassResult<AstStmt> {
        let name = self.parse_plain_at_rule_name()?;

        match name.as_str() {
            "content" => self.parse_content_rule(start),
            "debug" => self.parse_debug_rule(),
            "each" => self.parse_each_rule(Self::parse_declaration_child),
            "else" => self.parse_disallowed_at_rule(start),
            "error" => self.parse_error_rule(),
            "for" => self.parse_for_rule(Self::parse_declaration_child),
            "if" => self.parse_if_rule(Self::parse_declaration_child),
            "include" => self.parse_include_rule(),
            "warn" => self.parse_warn_rule(),
            "while" => self.parse_while_rule(Self::parse_declaration_child),
            _ => self.parse_disallowed_at_rule(start),
        }
    }

    fn parse_variable_declaration_or_style_rule(&mut self) -> SassResult<AstStmt> {
        if self.flags.in_plain_css() {
            return self.parse_style_rule(None);
        }

        if !self.looking_at_identifier() {
            return self.parse_style_rule(None);
        }

        match self.parse_variable_declaration_or_interpolation()? {
            VariableDeclOrInterpolation::VariableDecl(var) => Ok(AstStmt::VariableDecl(var)),
            VariableDeclOrInterpolation::Interpolation(int) => self.parse_style_rule(Some(int)),
        }
    }

    fn parse_style_rule(&mut self, existing_buffer: Option<Interpolation>) -> SassResult<AstStmt> {
        self.flags.set(ContextFlags::IS_USE_ALLOWED, false);
        let mut interpolation = self.parse_style_rule_selector()?;

        if let Some(mut existing_buffer) = existing_buffer {
            existing_buffer.add_interpolation(interpolation);
            interpolation = existing_buffer;
        }

        if interpolation.contents.is_empty() {
            todo!("expected \"}}\".");
        }

        let was_in_style_rule = self.flags.in_style_rule();
        self.flags |= ContextFlags::IN_STYLE_RULE;

        let children = self.with_children(Self::__parse_stmt)?;

        self.flags
            .set(ContextFlags::IN_STYLE_RULE, was_in_style_rule);

        self.whitespace();

        Ok(AstStmt::RuleSet(AstRuleSet {
            selector: interpolation,
            body: children,
        }))
    }

    // fn parse_child(&mut self) -> SassResult<AstStmt> {
    //     self.__parse_stmt(false)
    // }

    // todo: should return silent comment struct
    fn parse_silent_comment(&mut self) -> SassResult<AstStmt> {
        let start = self.toks.cursor();
        self.expect_char('/')?;
        self.expect_char('/')?;

        let mut buffer = String::new();

        while let Some(tok) = self.toks.next() {
            if tok.kind == '\n' {
                self.whitespace();
                if self.next_matches("//") {
                    self.expect_char('/')?;
                    self.expect_char('/')?;
                    buffer.clear();
                    continue;
                }
                break;
            }

            buffer.push(tok.kind);
        }

        if self.flags.in_plain_css() {
            return Err((
                "Silent comments aren't allowed in plain CSS.",
                self.toks.span_from(start),
            )
                .into());
        }

        self.whitespace();

        Ok(AstStmt::SilentComment(AstSilentComment {
            text: buffer,
            span: self.span_before,
        }))
    }

    fn next_is_hex(&self) -> bool {
        match self.toks.peek() {
            Some(Token { kind, .. }) => kind.is_ascii_hexdigit(),
            None => false,
        }
    }

    fn parse_children(
        &mut self,
        child: fn(&mut Self) -> SassResult<AstStmt>,
    ) -> SassResult<Vec<AstStmt>> {
        self.expect_char('{')?;
        self.whitespace();
        let mut children = Vec::new();

        let mut found_matching_brace = false;

        while let Some(tok) = self.toks.peek() {
            match tok.kind {
                '$' => children.push(AstStmt::VariableDecl(
                    self.parse_variable_declaration_without_namespace(None, None)?,
                )),
                '/' => match self.toks.peek_n(1) {
                    Some(Token { kind: '/', .. }) => {
                        children.push(self.parse_silent_comment()?);
                        self.whitespace();
                    }
                    Some(Token { kind: '*', .. }) => {
                        children.push(AstStmt::LoudComment(self.parse_loud_comment()?));
                        self.whitespace();
                    }
                    _ => children.push(child(self)?),
                },
                ';' => {
                    self.toks.next();
                    self.whitespace();
                }
                '}' => {
                    self.expect_char('}')?;
                    found_matching_brace = true;
                    break;
                }
                _ => children.push(child(self)?),
            }
        }

        if !found_matching_brace {
            todo!("expected \"}}\".");
        }

        Ok(children)
    }

    fn assert_public(ident: &str, span: Span) -> SassResult<()> {
        if !Parser::is_private(ident) {
            return Ok(());
        }

        Err((
            "Private members can't be accessed from outside their modules.",
            span,
        )
            .into())
    }

    fn is_private(ident: &str) -> bool {
        ident.starts_with('-') || ident.starts_with('_')
    }

    fn parse_variable_declaration_without_namespace(
        &mut self,
        namespace: Option<Spanned<Identifier>>,
        start: Option<usize>,
    ) -> SassResult<AstVariableDecl> {
        //     var precedingComment = lastSilentComment;
        // lastSilentComment = null;
        let start = start.unwrap_or(self.toks.cursor());

        let name = self.parse_variable_name()?;

        if namespace.is_some() {
            Self::assert_public(&name, self.toks.span_from(start))?;
        }

        if self.flags.in_plain_css() {
            todo!("Sass variables aren't allowed in plain CSS.")
        }

        self.whitespace_or_comment();
        self.expect_char(':')?;
        self.whitespace_or_comment();

        let value = self.parse_expression(None, None, None)?.node;

        let mut is_guarded = false;
        let mut is_global = false;

        while self.consume_char_if_exists('!') {
            let flag_start = self.toks.cursor();
            let flag = self.__parse_identifier(false, false)?;

            match flag.as_str() {
                "default" => is_guarded = true,
                "global" => {
                    if namespace.is_some() {
                        return Err((
                            "!global isn't allowed for variables in other modules.",
                            self.toks.span_from(flag_start),
                        )
                            .into());
                    }

                    is_global = true;
                }
                _ => return Err(("Invalid flag name.", self.toks.span_from(flag_start)).into()),
            }

            self.whitespace_or_comment();
        }

        self.expect_statement_separator(Some("variable declaration"))?;

        let declaration = AstVariableDecl {
            namespace,
            name: Identifier::from(name),
            value,
            is_guarded,
            is_global,
            span: self.span_before,
        };

        if is_global {
            // todo
            // _globalVariables.putIfAbsent(name, () => declaration)
        }

        Ok(declaration)
    }

    fn parse_style_rule_selector(&mut self) -> SassResult<Interpolation> {
        self.almost_any_value(false)
    }

    fn scan_comment(&mut self) -> SassResult<bool> {
        if !matches!(self.toks.peek(), Some(Token { kind: '/', .. })) {
            return Ok(false);
        }

        Ok(match self.toks.peek_n(1) {
            Some(Token { kind: '/', .. }) => {
                self.parse_silent_comment()?;
                true
            }
            Some(Token { kind: '*', .. }) => {
                self.skip_loud_comment()?;
                true
            }
            _ => false,
        })
    }

    fn almost_any_value(
        &mut self,
        // default=false
        omit_comments: bool,
    ) -> SassResult<Interpolation> {
        let mut buffer = Interpolation::new();

        while let Some(tok) = self.toks.peek() {
            match tok.kind {
                '\\' => {
                    // Write a literal backslash because this text will be re-parsed.
                    buffer.add_token(tok);
                    self.toks.next();
                    // todo: is this breakable
                    buffer.add_token(self.toks.next().unwrap());
                }
                '"' | '\'' => {
                    let interpolation = self
                        .parse_interpolated_string()?
                        .node
                        .as_interpolation(self.span_before, false);
                    buffer.add_interpolation(interpolation);
                }
                '/' => {
                    let comment_start = self.toks.cursor();
                    if self.scan_comment()? {
                        if !omit_comments {
                            buffer.add_string(self.toks.raw_text(comment_start));
                        }
                    } else {
                        buffer.add_token(self.toks.next().unwrap());
                    }
                }
                '#' => {
                    if matches!(self.toks.peek_n(1), Some(Token { kind: '{', .. })) {
                        // Add a full interpolated identifier to handle cases like
                        // "#{...}--1", since "--1" isn't a valid identifier on its own.
                        buffer.add_interpolation(self.parse_interpolated_identifier()?);
                    } else {
                        self.toks.next();
                        buffer.add_token(tok);
                    }
                }
                '\r' | '\n' => buffer.add_token(self.toks.next().unwrap()),
                '!' | ';' | '{' | '}' => break,
                'u' | 'U' => {
                    let before_url = self.toks.cursor();
                    if !self.scan_identifier("url", false)? {
                        self.toks.next();
                        buffer.add_token(tok);
                        continue;
                    }

                    match self.try_url_contents(None)? {
                        Some(contents) => buffer.add_interpolation(contents),
                        None => {
                            self.toks.set_cursor(before_url);
                            self.toks.next();
                            buffer.add_token(tok);
                        }
                    }
                }
                _ => {
                    if self.looking_at_identifier() {
                        buffer.add_string(self.__parse_identifier(false, false)?);
                    } else {
                        buffer.add_token(self.toks.next().unwrap());
                    }
                }
            }
        }

        Ok(buffer)
    }

    fn parse_variable_declaration_or_interpolation(
        &mut self,
    ) -> SassResult<VariableDeclOrInterpolation> {
        if !self.looking_at_identifier() {
            return Ok(VariableDeclOrInterpolation::Interpolation(
                self.parse_interpolated_identifier()?,
            ));
        }

        let start = self.toks.cursor();

        let ident = self.__parse_identifier(false, false)?;
        if self.next_matches(".$") {
            let namespace_span = self.toks.span_from(start);
            self.expect_char('.')?;
            Ok(VariableDeclOrInterpolation::VariableDecl(
                self.parse_variable_declaration_without_namespace(
                    Some(Spanned {
                        node: Identifier::from(ident),
                        span: namespace_span,
                    }),
                    Some(start),
                )?,
            ))
        } else {
            let mut buffer = Interpolation::new_plain(ident);

            if self.looking_at_interpolated_identifier_body() {
                buffer.add_interpolation(self.parse_interpolated_identifier()?);
            }

            Ok(VariableDeclOrInterpolation::Interpolation(buffer))
        }
    }

    fn looking_at_interpolated_identifier_body(&mut self) -> bool {
        match self.toks.peek() {
            Some(Token { kind: '\\', .. }) => true,
            Some(Token { kind: '#', .. })
                if matches!(self.toks.peek_n(1), Some(Token { kind: '{', .. })) =>
            {
                true
            }
            Some(Token { kind, .. }) if is_name(kind) => true,
            Some(..) | None => false,
        }
    }

    fn next_matches(&mut self, s: &str) -> bool {
        for (idx, c) in s.chars().enumerate() {
            match self.toks.peek_n(idx) {
                Some(Token { kind, .. }) if kind == c => {}
                _ => return false,
            }
        }

        true
    }

    // todo: don't return token once we switch to remove pos
    pub fn expect_char(&mut self, c: char) -> SassResult<Token> {
        match self.toks.peek() {
            Some(tok) if tok.kind == c => {
                self.span_before = tok.pos;
                self.toks.next();
                Ok(tok)
            }
            Some(Token { pos, .. }) => Err((format!("expected \"{}\".", c), pos).into()),
            None => Err((format!("expected \"{}\".", c), self.toks.current_span()).into()),
        }
    }

    pub fn consume_char_if_exists(&mut self, c: char) -> bool {
        if let Some(Token { kind, .. }) = self.toks.peek() {
            if kind == c {
                self.toks.next();
                return true;
            }
        }

        false
    }

    pub fn expect_identifier(&mut self, ident: &str, case_sensitive: bool) -> SassResult<()> {
        let start = self.toks.cursor();

        for c in ident.chars() {
            if !self.scan_ident_char(c, case_sensitive)? {
                return Err((
                    format!("Expected \"{}\".", ident),
                    self.toks.span_from(start),
                )
                    .into());
            }
        }

        if !self.looking_at_identifier_body() {
            return Ok(());
        }

        Err((
            format!("Expected \"{}\".", ident),
            self.toks.span_from(start),
        )
            .into())
    }

    // fn parse_stmt(&mut self) -> SassResult<Vec<Stmt>> {
    //     let mut stmts = Vec::new();
    //     while let Some(Token { kind, pos }) = self.toks.peek() {
    //         if self.flags.in_function() && !stmts.is_empty() {
    //             return Ok(stmts);
    //         }
    //         self.span_before = pos;
    //         match kind {
    //             '@' => {
    //                 self.toks.next();
    //                 let kind_string = self.parse_identifier()?;
    //                 self.span_before = kind_string.span;
    //                 match AtRuleKind::try_from(&kind_string)? {
    //                     AtRuleKind::Import => stmts.append(&mut self.import()?),
    //                     AtRuleKind::Mixin => self.parse_mixin()?,
    //                     AtRuleKind::Content => stmts.append(&mut self.parse_content_rule()?),
    //                     AtRuleKind::Include => stmts.append(&mut self.parse_include()?),
    //                     AtRuleKind::Function => self.parse_function()?,
    //                     AtRuleKind::Return => {
    //                         if self.flags.in_function() {
    //                             return Ok(vec![Stmt::Return(self.parse_return()?)]);
    //                         }

    //                         return Err(
    //                             ("This at-rule is not allowed here.", kind_string.span).into()
    //                         );
    //                     }
    //                     AtRuleKind::AtRoot => {
    //                         if self.flags.in_function() {
    //                             return Err((
    //                                 "This at-rule is not allowed here.",
    //                                 kind_string.span,
    //                             )
    //                                 .into());
    //                         }

    //                         if self.at_root {
    //                             stmts.append(&mut self.parse_at_root()?);
    //                         } else {
    //                             let body = self.parse_at_root()?;
    //                             stmts.push(Stmt::AtRoot { body });
    //                         }
    //                     }
    //                     AtRuleKind::Error => {
    //                         let Spanned {
    //                             node: message,
    //                             span,
    //                         } = self.parse_value(false, &|_| false)?;

    //                         return Err((
    //                             message.inspect(span)?.to_string(),
    //                             span.merge(kind_string.span),
    //                         )
    //                             .into());
    //                     }
    //                     AtRuleKind::Warn => {
    //                         let Spanned {
    //                             node: message,
    //                             span,
    //                         } = self.parse_value(false, &|_| false)?;
    //                         span.merge(kind_string.span);

    //                         self.consume_char_if_exists(';');

    //                         self.warn(&Spanned {
    //                             node: message.to_css_string(span, false)?,
    //                             span,
    //                         });
    //                     }
    //                     AtRuleKind::Debug => {
    //                         let Spanned {
    //                             node: message,
    //                             span,
    //                         } = self.parse_value(false, &|_| false)?;
    //                         span.merge(kind_string.span);

    //                         self.consume_char_if_exists(';');

    //                         self.debug(&Spanned {
    //                             node: message.inspect(span)?,
    //                             span,
    //                         });
    //                     }
    //                     AtRuleKind::If => stmts.append(&mut self.parse_if()?),
    //                     AtRuleKind::Each => stmts.append(&mut self.parse_each()?),
    //                     AtRuleKind::For => stmts.append(&mut self.parse_for()?),
    //                     AtRuleKind::While => stmts.append(&mut self.parse_while()?),
    //                     AtRuleKind::Charset => {
    //                         if self.flags.in_function() {
    //                             return Err((
    //                                 "This at-rule is not allowed here.",
    //                                 kind_string.span,
    //                             )
    //                                 .into());
    //                         }

    //                         let val = self.parse_value(false, &|_| false)?;

    //                         self.consume_char_if_exists(';');

    //                         if !val.node.is_quoted_string() {
    //                             return Err(("Expected string.", val.span).into());
    //                         }

    //                         continue;
    //                     }
    //                     AtRuleKind::Media => stmts.push(self.parse_media()?),
    //                     AtRuleKind::Unknown(_) => {
    //                         stmts.push(self.parse_unknown_at_rule(kind_string.node)?);
    //                     }
    //                     AtRuleKind::Use => {
    //                         return Err((
    //                             "@use rules must be written before any other rules.",
    //                             kind_string.span,
    //                         )
    //                             .into())
    //                     }
    //                     AtRuleKind::Forward => todo!("@forward not yet implemented"),
    //                     AtRuleKind::Extend => self.parse_extend()?,
    //                     AtRuleKind::Supports => stmts.push(self.parse_supports()?),
    //                     AtRuleKind::Keyframes => {
    //                         stmts.push(self.parse_keyframes(kind_string.node)?);
    //                     }
    //                 }
    //             }
    //             '$' => self.parse_variable_declaration()?,
    //             '\t' | '\n' | ' ' | ';' => {
    //                 self.toks.next();
    //                 continue;
    //             }
    //             '/' => {
    //                 self.toks.next();
    //                 let comment = self.parse_comment()?;
    //                 self.whitespace();
    //                 match comment.node {
    //                     Comment::Silent => continue,
    //                     Comment::Loud(s) => {
    //                         if !self.flags.in_function() {
    //                             stmts.push(Stmt::Comment(s));
    //                         }
    //                     }
    //                 }
    //             }
    //             '\u{0}'..='\u{8}' | '\u{b}'..='\u{1f}' => {
    //                 return Err(("expected selector.", pos).into())
    //             }
    //             '}' => {
    //                 self.toks.next();
    //                 break;
    //             }
    //             // dart-sass seems to special-case the error message here?
    //             '!' | '{' => return Err(("expected \"}\".", pos).into()),
    //             _ => {
    //                 if self.flags.in_function() {
    //                     return Err((
    //                     "Functions can only contain variable declarations and control directives.",
    //                     self.span_before
    //                 )
    //                     .into());
    //                 }
    //                 if self.flags.in_keyframes() {
    //                     match self.is_selector_or_style()? {
    //                         SelectorOrStyle::ModuleVariableRedeclaration(module) => {
    //                             self.parse_module_variable_redeclaration(module)?;
    //                         }
    //                         SelectorOrStyle::Style(property, value) => {
    //                             if let Some(value) = value {
    //                                 stmts.push(Stmt::Style(Style { property, value }));
    //                             } else {
    //                                 stmts.extend(
    //                                     self.parse_style_group(property)?
    //                                         .into_iter()
    //                                         .map(Stmt::Style),
    //                                 );
    //                             }
    //                         }
    //                         SelectorOrStyle::Selector(init) => {
    //                             let selector = self.parse_keyframes_selector(init)?;
    //                             self.scopes.enter_new_scope();

    //                             let body = self.parse_stmt()?;
    //                             self.scopes.exit_scope();
    //                             stmts.push(Stmt::KeyframesRuleSet(Box::new(KeyframesRuleSet {
    //                                 selector,
    //                                 body,
    //                             })));
    //                         }
    //                     }
    //                     continue;
    //                 }

    //                 match self.is_selector_or_style()? {
    //                     SelectorOrStyle::ModuleVariableRedeclaration(module) => {
    //                         self.parse_module_variable_redeclaration(module)?;
    //                     }
    //                     SelectorOrStyle::Style(property, value) => {
    //                         if let Some(value) = value {
    //                             stmts.push(Stmt::Style(Style { property, value }));
    //                         } else {
    //                             stmts.extend(
    //                                 self.parse_style_group(property)?
    //                                     .into_iter()
    //                                     .map(Stmt::Style),
    //                             );
    //                         }
    //                     }
    //                     SelectorOrStyle::Selector(init) => {
    //                         let at_root = self.at_root;
    //                         self.at_root = false;
    //                         let selector = self
    //                             .parse_selector(true, false, init)?
    //                             .0
    //                             .resolve_parent_selectors(
    //                                 &self.super_selectors.last().clone().into_selector(),
    //                                 !at_root || self.at_root_has_selector,
    //                             )?;
    //                         self.scopes.enter_new_scope();

    //                         let extended_selector = self.extender.add_selector(selector.0, None);

    //                         self.super_selectors.push(extended_selector.clone());

    //                         let body = self.parse_stmt()?;
    //                         self.scopes.exit_scope();
    //                         self.super_selectors.pop();
    //                         self.at_root = self.super_selectors.is_empty();
    //                         stmts.push(Stmt::RuleSet {
    //                             selector: extended_selector,
    //                             body,
    //                         });
    //                     }
    //                 }
    //             }
    //         }
    //     }
    //     Ok(stmts)
    // }

    // pub fn parse_selector(
    //     &mut self,
    //     allows_parent: bool,
    //     from_fn: bool,
    //     mut string: String,
    // ) -> SassResult<(Selector, bool)> {
    //     todo!()
    // let mut span = if let Some(tok) = self.toks.peek() {
    //     tok.pos()
    // } else {
    //     return Err(("expected \"{\".", self.span_before).into());
    // };

    // self.span_before = span;

    // let mut found_curly = false;

    // let mut optional = false;

    // // we resolve interpolation and strip comments
    // while let Some(Token { kind, pos }) = self.toks.next() {
    //     span = span.merge(pos);
    //     match kind {
    //         '#' => {
    //             if self.consume_char_if_exists('{') {
    //                 string.push_str(
    //                     &self
    //                         .parse_interpolation()?
    //                         .to_css_string(span, self.options.is_compressed())?,
    //                 );
    //             } else {
    //                 string.push('#');
    //             }
    //         }
    //         '/' => {
    //             if self.toks.peek().is_none() {
    //                 return Err(("Expected selector.", pos).into());
    //             }
    //             self.parse_comment()?;
    //             string.push(' ');
    //         }
    //         '{' => {
    //             if from_fn {
    //                 return Err(("Expected selector.", pos).into());
    //             }

    //             found_curly = true;
    //             break;
    //         }
    //         '\\' => {
    //             string.push('\\');
    //             if let Some(Token { kind, .. }) = self.toks.next() {
    //                 string.push(kind);
    //             }
    //         }
    //         '!' => {
    //             if from_fn {
    //                 self.expect_identifier("optional")?;
    //                 optional = true;
    //             } else {
    //                 return Err(("expected \"{\".", pos).into());
    //             }
    //         }
    //         c => string.push(c),
    //     }
    // }

    // if !found_curly && !from_fn {
    //     return Err(("expected \"{\".", span).into());
    // }

    // let sel_toks: Vec<Token> = string.chars().map(|x| Token::new(span, x)).collect();

    // let mut lexer = Lexer::new(sel_toks);

    // let selector = SelectorParser::new(
    //     &mut Parser {
    //         toks: &mut lexer,
    //         map: self.map,
    //         path: self.path,
    //         scopes: self.scopes,
    //         global_scope: self.global_scope,
    //         super_selectors: self.super_selectors,
    //         span_before: self.span_before,
    //         content: self.content,
    //         flags: self.flags,
    //         at_root: self.at_root,
    //         at_root_has_selector: self.at_root_has_selector,
    //         extender: self.extender,
    //         content_scopes: self.content_scopes,
    //         options: self.options,
    //         modules: self.modules,
    //         module_config: self.module_config,
    //     },
    //     allows_parent,
    //     true,
    //     span,
    // )
    // .parse()?;

    // Ok((Selector(selector), optional))
    // }

    // /// Eat and return the contents of a comment.
    // ///
    // /// This function assumes that the starting "/" has already been consumed
    // /// The entirety of the comment, including the ending "*/" for multiline comments,
    // /// is consumed. Note that the ending "*/" is not included in the output.
    // #[allow(clippy::eval_order_dependence)]
    // pub fn parse_comment(&mut self) -> SassResult<Spanned<Comment>> {
    //     let mut span = self.span_before;
    //     Ok(Spanned {
    //         node: match self.toks.next() {
    //             Some(Token { kind: '/', .. }) => {
    //                 while let Some(tok) = self.toks.peek() {
    //                     if tok.kind == '\n' {
    //                         break;
    //                     }
    //                     span = span.merge(tok.pos);
    //                     self.toks.next();
    //                 }

    //                 Comment::Silent
    //             }
    //             Some(Token { kind: '*', .. }) => {
    //                 let mut comment = String::new();
    //                 while let Some(tok) = self.toks.next() {
    //                     span = span.merge(tok.pos());
    //                     match (tok.kind, self.toks.peek()) {
    //                         ('*', Some(Token { kind: '/', .. })) => {
    //                             self.toks.next();
    //                             break;
    //                         }
    //                         ('#', Some(Token { kind: '{', .. })) => {
    //                             self.toks.next();
    //                             comment.push_str(
    //                                 &self
    //                                     .parse_interpolation()?
    //                                     .to_css_string(span, self.options.is_compressed())?,
    //                             );
    //                             continue;
    //                         }
    //                         (..) => comment.push(tok.kind),
    //                     }
    //                 }
    //                 Comment::Loud(comment)
    //             }
    //             Some(..) | None => return Err(("expected selector.", self.span_before).into()),
    //         },
    //         span,
    //     })
    // }

    // #[track_caller]
    // pub fn parse_interpolation(&mut self) -> SassResult<Spanned<Value>> {
    //     let val = self.parse_value(true, &|_| false)?;

    //     self.span_before = val.span;

    //     self.expect_char('}')?;

    //     Ok(val.map_node(Value::unquote))
    // }

    // pub fn parse_interpolation_as_string(&mut self) -> SassResult<Cow<'static, str>> {
    //     let interpolation = self.parse_interpolation()?;
    //     Ok(match interpolation.node {
    //         Value::String(v, ..) => Cow::owned(v),
    //         v => v.to_css_string(interpolation.span, self.options.is_compressed())?,
    //     })
    // }

    // todo: this should also consume silent comments
    pub fn whitespace(&mut self) -> bool {
        let mut found_whitespace = false;
        while let Some(tok) = self.toks.peek() {
            match tok.kind {
                ' ' | '\t' | '\n' => {
                    self.toks.next();
                    found_whitespace = true;
                }
                _ => return found_whitespace,
            }
        }
        found_whitespace
    }

    /// Eat tokens until a newline
    ///
    /// This exists largely to eat silent comments, "//"
    /// We only have to check for \n as the lexing step normalizes all newline characters
    ///
    /// The newline is consumed
    pub fn read_until_newline(&mut self) {
        for tok in &mut self.toks {
            if tok.kind == '\n' {
                break;
            }
        }
    }

    // todo: rewrite
    pub fn whitespace_or_comment(&mut self) -> bool {
        let mut found_whitespace = false;
        while let Some(tok) = self.toks.peek() {
            match tok.kind {
                ' ' | '\t' | '\n' => {
                    self.toks.next();
                    found_whitespace = true;
                }
                '/' => match self.toks.peek_forward(1) {
                    Some(Token { kind: '*', .. }) => {
                        found_whitespace = true;
                        self.toks.next();
                        self.toks.next();
                        #[allow(clippy::while_let_on_iterator)]
                        while let Some(tok) = self.toks.next() {
                            if tok.kind == '*' && self.consume_char_if_exists('/') {
                                break;
                            }
                        }
                    }
                    Some(Token { kind: '/', .. }) => {
                        found_whitespace = true;
                        self.read_until_newline();
                    }
                    _ => {
                        self.toks.reset_cursor();
                        return found_whitespace;
                    }
                },
                _ => return found_whitespace,
            }
        }
        found_whitespace
    }
}

impl<'a, 'b> Parser<'a, 'b> {
    // fn parse_unknown_at_rule(&mut self, name: String) -> SassResult<Stmt> {
    // if self.flags.in_function() {
    //     return Err(("This at-rule is not allowed here.", self.span_before).into());
    // }

    // let mut params = String::new();
    // self.whitespace_or_comment();

    // loop {
    //     match self.toks.peek() {
    //         Some(Token { kind: '{', .. }) => {
    //             self.toks.next();
    //             break;
    //         }
    //         Some(Token { kind: ';', .. }) | Some(Token { kind: '}', .. }) | None => {
    //             self.consume_char_if_exists(';');
    //             return Ok(Stmt::UnknownAtRule(Box::new(UnknownAtRule {
    //                 name,
    //                 super_selector: Selector::new(self.span_before),
    //                 has_body: false,
    //                 params: params.trim().to_owned(),
    //                 body: Vec::new(),
    //             })));
    //         }
    //         Some(Token { kind: '#', .. }) => {
    //             self.toks.next();

    //             if let Some(Token { kind: '{', pos }) = self.toks.peek() {
    //                 self.span_before = self.span_before.merge(pos);
    //                 self.toks.next();
    //                 params.push_str(&self.parse_interpolation_as_string()?);
    //             } else {
    //                 params.push('#');
    //             }
    //             continue;
    //         }
    //         Some(Token { kind: '\n', .. })
    //         | Some(Token { kind: ' ', .. })
    //         | Some(Token { kind: '\t', .. }) => {
    //             self.whitespace();
    //             params.push(' ');
    //             continue;
    //         }
    //         Some(Token { kind, .. }) => {
    //             self.toks.next();
    //             params.push(kind);
    //         }
    //     }
    // }

    // let raw_body = self.parse_stmt()?;
    // let mut rules = Vec::with_capacity(raw_body.len());
    // let mut body = Vec::new();

    // for stmt in raw_body {
    //     match stmt {
    //         Stmt::Style(..) => body.push(stmt),
    //         _ => rules.push(stmt),
    //     }
    // }

    // if !self.super_selectors.last().as_selector_list().is_empty() {
    //     body = vec![Stmt::RuleSet {
    //         selector: self.super_selectors.last().clone(),
    //         body,
    //     }];
    // }

    // body.append(&mut rules);

    // Ok(Stmt::UnknownAtRule(Box::new(UnknownAtRule {
    //     name,
    //     super_selector: Selector::new(self.span_before),
    //     params: params.trim().to_owned(),
    //     has_body: true,
    //     body,
    // })))
    // todo!()
    // }

    // fn parse_media(&mut self) -> SassResult<Stmt> {
    //     if self.flags.in_function() {
    //         return Err(("This at-rule is not allowed here.", self.span_before).into());
    //     }

    //     let query = self.parse_media_query_list()?;

    //     self.whitespace();

    //     self.expect_char('{')?;

    //     let raw_body = self.parse_stmt()?;

    //     let mut rules = Vec::with_capacity(raw_body.len());
    //     let mut body = Vec::new();

    //     for stmt in raw_body {
    //         match stmt {
    //             Stmt::Style(..) => body.push(stmt),
    //             _ => rules.push(stmt),
    //         }
    //     }

    //     if !self.super_selectors.last().as_selector_list().is_empty() {
    //         body = vec![Stmt::RuleSet {
    //             selector: self.super_selectors.last().clone(),
    //             body,
    //         }];
    //     }

    //     body.append(&mut rules);

    //     Ok(Stmt::Media(Box::new(MediaRule {
    //         super_selector: Selector::new(self.span_before),
    //         query,
    //         body,
    //     })))
    // }

    // fn parse_at_root(&mut self) -> SassResult<Vec<Stmt>> {
    //     self.whitespace();
    //     let mut at_root_has_selector = false;
    //     let at_rule_selector = if self.consume_char_if_exists('{') {
    //         self.super_selectors.last().clone()
    //     } else {
    //         at_root_has_selector = true;
    //         let selector = self
    //             .parse_selector(true, false, String::new())?
    //             .0
    //             .resolve_parent_selectors(
    //                 &self.super_selectors.last().clone().into_selector(),
    //                 false,
    //             )?;

    //         self.extender.add_selector(selector.0, None)
    //     };

    //     self.whitespace();

    //     let mut styles = Vec::new();
    //     #[allow(clippy::unnecessary_filter_map)]
    //     let raw_stmts = Parser {
    //         toks: self.toks,
    //         map: self.map,
    //         path: self.path,
    //         scopes: self.scopes,
    //         global_scope: self.global_scope,
    //         super_selectors: &mut NeverEmptyVec::new(at_rule_selector.clone()),
    //         span_before: self.span_before,
    //         content: self.content,
    //         flags: self.flags | ContextFlags::IN_AT_ROOT_RULE,
    //         at_root: true,
    //         at_root_has_selector,
    //         extender: self.extender,
    //         content_scopes: self.content_scopes,
    //         options: self.options,
    //         modules: self.modules,
    //         module_config: self.module_config,
    //     }
    //     .parse_stmt()?
    //     .into_iter()
    //     .filter_map(|s| match s {
    //         Stmt::Style(..) => {
    //             styles.push(s);
    //             None
    //         }
    //         _ => Some(Ok(s)),
    //     })
    //     .collect::<SassResult<Vec<Stmt>>>()?;

    //     let stmts = if at_root_has_selector {
    //         let mut body = styles;
    //         body.extend(raw_stmts);

    //         vec![Stmt::RuleSet {
    //             body,
    //             selector: at_rule_selector,
    //         }]
    //     } else {
    //         if !styles.is_empty() {
    //             return Err((
    //                 "Found style at the toplevel inside @at-root.",
    //                 self.span_before,
    //             )
    //                 .into());
    //         }

    //         raw_stmts
    //     };

    //     Ok(stmts)
    // }

    // fn parse_extend(&mut self) -> SassResult<()> {
    // if self.flags.in_function() {
    //     return Err(("This at-rule is not allowed here.", self.span_before).into());
    // }
    // // todo: track when inside ruleset or `@content`
    // // if !self.in_style_rule && !self.in_mixin && !self.in_content_block {
    // //     return Err(("@extend may only be used within style rules.", self.span_before).into());
    // // }
    // let (value, is_optional) = Parser {
    //     toks: &mut Lexer::new(read_until_semicolon_or_closing_curly_brace(self.toks)?),
    //     map: self.map,
    //     path: self.path,
    //     scopes: self.scopes,
    //     global_scope: self.global_scope,
    //     super_selectors: self.super_selectors,
    //     span_before: self.span_before,
    //     content: self.content,
    //     flags: self.flags,
    //     at_root: self.at_root,
    //     at_root_has_selector: self.at_root_has_selector,
    //     extender: self.extender,
    //     content_scopes: self.content_scopes,
    //     options: self.options,
    //     modules: self.modules,
    //     module_config: self.module_config,
    // }
    // .parse_selector(false, true, String::new())?;

    // // todo: this might be superfluous
    // self.whitespace();

    // self.consume_char_if_exists(';');

    // let extend_rule = ExtendRule::new(value.clone(), is_optional, self.span_before);

    // let super_selector = self.super_selectors.last();

    // for complex in value.0.components {
    //     if complex.components.len() != 1 || !complex.components.first().unwrap().is_compound() {
    //         // If the selector was a compound selector but not a simple
    //         // selector, emit a more explicit error.
    //         return Err(("complex selectors may not be extended.", self.span_before).into());
    //     }

    //     let compound = match complex.components.first() {
    //         Some(ComplexSelectorComponent::Compound(c)) => c,
    //         Some(..) | None => todo!(),
    //     };
    //     if compound.components.len() != 1 {
    //         return Err((
    //             format!(
    //                 "compound selectors may no longer be extended.\nConsider `@extend {}` instead.\nSee http://bit.ly/ExtendCompound for details.\n",
    //                 compound.components.iter().map(ToString::to_string).collect::<Vec<String>>().join(", ")
    //             )
    //         , self.span_before).into());
    //     }

    //     self.extender.add_extension(
    //         super_selector.clone().into_selector().0,
    //         compound.components.first().unwrap(),
    //         &extend_rule,
    //         &None,
    //         self.span_before,
    //     );
    // }

    // Ok(())
    // todo!()
    // }

    // fn parse_supports(&mut self) -> SassResult<Stmt> {
    //     if self.flags.in_function() {
    //         return Err(("This at-rule is not allowed here.", self.span_before).into());
    //     }

    //     let params = self.parse_media_args()?;

    //     if params.is_empty() {
    //         return Err(("Expected \"not\".", self.span_before).into());
    //     }

    //     let raw_body = self.parse_stmt()?;

    //     let mut rules = Vec::with_capacity(raw_body.len());
    //     let mut body = Vec::new();

    //     for stmt in raw_body {
    //         match stmt {
    //             Stmt::Style(..) => body.push(stmt),
    //             _ => rules.push(stmt),
    //         }
    //     }

    //     if !self.super_selectors.last().as_selector_list().is_empty() {
    //         body = vec![Stmt::RuleSet {
    //             selector: self.super_selectors.last().clone(),
    //             body,
    //         }];
    //     }

    //     body.append(&mut rules);

    //     Ok(Stmt::Supports(Box::new(SupportsRule {
    //         params: params.trim().to_owned(),
    //         body,
    //     })))
    // }

    // todo: we should use a specialized struct to represent these
    // fn parse_media_args(&mut self) -> SassResult<String> {
    //     let mut params = String::new();
    //     self.whitespace();
    //     while let Some(tok) = self.toks.next() {
    //         match tok.kind {
    //             '{' => break,
    //             '#' => {
    //                 if let Some(Token { kind: '{', pos }) = self.toks.peek() {
    //                     self.toks.next();
    //                     self.span_before = pos;
    //                     let interpolation = self.parse_interpolation()?;
    //                     params.push_str(
    //                         &interpolation
    //                             .node
    //                             .to_css_string(interpolation.span, self.options.is_compressed())?,
    //                     );
    //                     continue;
    //                 }

    //                 params.push(tok.kind);
    //             }
    //             '\n' | ' ' | '\t' => {
    //                 self.whitespace();
    //                 params.push(' ');
    //                 continue;
    //             }
    //             _ => {}
    //         }
    //         params.push(tok.kind);
    //     }
    //     Ok(params)
    // }
}

// impl<'a, 'b> Parser<'a, 'b> {
//     fn debug(&self, message: &Spanned<Cow<'a, str>>) {
//         if self.options.quiet {
//             return;
//         }
//         let loc = self.map.look_up_span(message.span);
//         eprintln!(
//             "{}:{} DEBUG: {}",
//             loc.file.name(),
//             loc.begin.line + 1,
//             message.node
//         );
//     }

//     fn warn(&self, message: &Spanned<Cow<'a, str>>) {
//         if self.options.quiet {
//             return;
//         }
//         let loc = self.map.look_up_span(message.span);
//         eprintln!(
//             "Warning: {}\n    {} {}:{}  root stylesheet",
//             message.node,
//             loc.file.name(),
//             loc.begin.line + 1,
//             loc.begin.column + 1
//         );
//     }
// }
