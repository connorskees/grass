use std::{
    convert::TryFrom,
    ffi::{OsStr, OsString},
    fs, mem,
    path::Path,
    vec::IntoIter,
};

use codemap::{CodeMap, Span, Spanned};
use num_traits::cast::ToPrimitive;
use peekmore::{PeekMore, PeekMoreIterator};

use crate::{
    args::{CallArgs, FuncArgs},
    atrule::{AtRuleKind, Function, Mixin},
    common::{Brackets, Identifier, ListSeparator},
    error::SassResult,
    lexer::Lexer,
    scope::Scope,
    selector::{Selector, SelectorParser},
    style::Style,
    unit::Unit,
    utils::{
        is_name, is_name_start, peek_ident_no_interpolation, read_until_closing_curly_brace,
        read_until_closing_paren, read_until_closing_quote, read_until_newline,
        read_until_open_curly_brace, read_until_semicolon_or_closing_curly_brace,
    },
    value::{Number, Value},
    {Cow, Token},
};

use common::{Branch, NeverEmptyVec, SelectorOrStyle};

mod args;
pub mod common;
mod ident;
mod value;

pub(crate) enum Comment {
    Silent,
    Loud(String),
}

#[derive(Debug)]
struct VariableValue {
    value: Spanned<Value>,
    global: bool,
    default: bool,
}

impl VariableValue {
    pub const fn new(value: Spanned<Value>, global: bool, default: bool) -> Self {
        Self {
            value,
            global,
            default,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Stmt {
    RuleSet {
        super_selector: Selector,
        selector: Selector,
        body: Vec<Self>,
    },
    Style(Box<Style>),
    Media {
        super_selector: Selector,
        params: String,
        body: Vec<Stmt>,
    },
    UnknownAtRule {
        name: String,
        super_selector: Selector,
        params: String,
        body: Vec<Stmt>,
    },
    AtRoot {
        body: Vec<Stmt>,
    },
    Comment(String),
    Return(Value),
}

/// We could use a generic for the toks, but it makes the API
/// much simpler to work with if it isn't generic. The performance
/// hit (if there is one) is not important for now.
// todo: refactor `in_mixin`, in_function`, and `at_root` into state machine enum
#[allow(clippy::struct_excessive_bools)]
pub(crate) struct Parser<'a> {
    pub toks: &'a mut PeekMoreIterator<IntoIter<Token>>,
    pub map: &'a mut CodeMap,
    pub path: &'a Path,
    pub global_scope: &'a mut Scope,
    pub scopes: &'a mut NeverEmptyVec<Scope>,
    pub super_selectors: &'a mut NeverEmptyVec<Selector>,
    pub span_before: Span,
    pub content: Option<Vec<Stmt>>,
    pub in_mixin: bool,
    pub in_function: bool,
    pub in_control_flow: bool,
    /// Whether this parser is at the root of the document
    /// E.g. not inside a style, mixin, or function
    pub at_root: bool,
    /// If this parser is inside an `@at-rule` block, this is whether or
    /// not the `@at-rule` block has a super selector
    pub at_root_has_selector: bool,
}

impl<'a> Parser<'a> {
    pub fn parse(&mut self) -> SassResult<Vec<Stmt>> {
        let mut stmts = Vec::new();
        while self.toks.peek().is_some() {
            stmts.append(&mut self.parse_stmt()?);
            if self.in_function && !stmts.is_empty() {
                return Ok(stmts);
            }
            self.at_root = true;
        }
        Ok(stmts)
    }

    fn parse_stmt(&mut self) -> SassResult<Vec<Stmt>> {
        let mut stmts = Vec::new();
        while let Some(Token { kind, pos }) = self.toks.peek() {
            if self.in_function && !stmts.is_empty() {
                return Ok(stmts);
            }
            match kind {
                '@' => {
                    self.toks.next();
                    let kind_string = self.parse_identifier()?;
                    match AtRuleKind::try_from(&kind_string)? {
                        AtRuleKind::Import => stmts.append(&mut self.import()?),
                        AtRuleKind::Mixin => self.parse_mixin()?,
                        AtRuleKind::Content => {
                            if self.in_mixin {
                                if let Some(content) = &self.content {
                                    stmts.append(&mut content.clone());
                                }
                            } else {
                                return Err((
                                    "@content is only allowed within mixin declarations.",
                                    kind_string.span,
                                )
                                    .into());
                            }
                        }
                        AtRuleKind::Include => stmts.append(&mut self.parse_include()?),
                        AtRuleKind::Function => self.parse_function()?,
                        AtRuleKind::Return => {
                            if self.in_function {
                                return Ok(vec![Stmt::Return(self.parse_return()?)]);
                            } else {
                                return Err((
                                    "This at-rule is not allowed here.",
                                    kind_string.span,
                                )
                                    .into());
                            }
                        }
                        AtRuleKind::AtRoot => {
                            if self.at_root {
                                stmts.append(&mut self.parse_at_root()?);
                            } else {
                                stmts.push(Stmt::AtRoot {
                                    body: self.parse_at_root()?,
                                });
                            }
                        }
                        AtRuleKind::Error => {
                            let toks = read_until_semicolon_or_closing_curly_brace(self.toks)?;
                            let Spanned {
                                node: message,
                                span,
                            } = self.parse_value_from_vec(toks)?;

                            return Err((
                                message.inspect(span)?.to_string(),
                                span.merge(kind_string.span),
                            )
                                .into());
                        }
                        AtRuleKind::Warn => {
                            let toks = read_until_semicolon_or_closing_curly_brace(self.toks)?;
                            let Spanned {
                                node: message,
                                span,
                            } = self.parse_value_from_vec(toks)?;
                            span.merge(kind_string.span);
                            if let Some(Token { kind: ';', .. }) = self.toks.peek() {
                                kind_string.span.merge(self.toks.next().unwrap().pos());
                            }
                            self.warn(&Spanned {
                                node: message.to_css_string(span)?,
                                span,
                            })
                        }
                        AtRuleKind::Debug => {
                            let toks = read_until_semicolon_or_closing_curly_brace(self.toks)?;
                            let Spanned {
                                node: message,
                                span,
                            } = self.parse_value_from_vec(toks)?;
                            span.merge(kind_string.span);
                            if let Some(Token { kind: ';', .. }) = self.toks.peek() {
                                kind_string.span.merge(self.toks.next().unwrap().pos());
                            }
                            self.debug(&Spanned {
                                node: message.inspect(span)?,
                                span,
                            })
                        }
                        AtRuleKind::If => stmts.append(&mut self.parse_if()?),
                        AtRuleKind::Each => stmts.append(&mut self.parse_each()?),
                        AtRuleKind::For => stmts.append(&mut self.parse_for()?),
                        AtRuleKind::While => stmts.append(&mut self.parse_while()?),
                        AtRuleKind::Charset => {
                            read_until_semicolon_or_closing_curly_brace(self.toks)?;
                            if let Some(Token { kind: ';', .. }) = self.toks.peek() {
                                self.toks.next();
                            }
                            continue;
                        }
                        AtRuleKind::Media => stmts.push(self.parse_media()?),
                        AtRuleKind::Unknown(_) => {
                            stmts.push(self.parse_unknown_at_rule(kind_string.node)?)
                        }
                        AtRuleKind::Use => todo!("@use not yet implemented"),
                        AtRuleKind::Forward => todo!("@forward not yet implemented"),
                        AtRuleKind::Extend => self.parse_extend()?,
                        AtRuleKind::Supports => stmts.push(self.parse_supports()?),
                        AtRuleKind::Keyframes => stmts.push(self.parse_keyframes()?),
                    }
                }
                '$' => self.parse_variable_declaration()?,
                '\t' | '\n' | ' ' | ';' => {
                    self.toks.next();
                    continue;
                }
                '/' => {
                    self.toks.next();
                    let comment = self.parse_comment()?;
                    match comment.node {
                        Comment::Silent => continue,
                        Comment::Loud(s) => stmts.push(Stmt::Comment(s)),
                    }
                }
                '\u{0}'..='\u{8}' | '\u{b}'..='\u{1f}' => {
                    return Err(("expected selector.", self.toks.next().unwrap().pos).into())
                }
                '}' => {
                    self.toks.next();
                    break;
                }
                // dart-sass seems to special-case the error message here?
                '!' | '{' => return Err(("expected \"}\".", *pos).into()),
                _ => match self.is_selector_or_style()? {
                    SelectorOrStyle::Style(property, value) => {
                        let styles = if let Some(value) = value {
                            vec![Style {
                                property,
                                value: *value,
                            }]
                        } else {
                            self.parse_style_group(property)?
                        };
                        stmts.extend(styles.into_iter().map(Box::new).map(Stmt::Style));
                    }
                    SelectorOrStyle::Selector(init) => {
                        let at_root = self.at_root;
                        self.at_root = false;
                        let super_selector = self.super_selectors.last().clone().clone();
                        let selector =
                            self.parse_selector(!self.super_selectors.is_empty(), false, init)?;
                        self.scopes.push(self.scopes.last().clone());
                        self.super_selectors.push(selector.resolve_parent_selectors(
                            &super_selector,
                            !at_root || self.at_root_has_selector,
                        ));
                        let body = self.parse_stmt()?;
                        self.scopes.pop();
                        self.super_selectors.pop();
                        self.at_root = self.super_selectors.is_empty();
                        stmts.push(Stmt::RuleSet {
                            selector,
                            body,
                            super_selector,
                        });
                    }
                },
            }
        }
        Ok(stmts)
    }

    /// Determines whether the parser is looking at a style or a selector
    ///
    /// When parsing the children of a style rule, property declarations,
    /// namespaced variable declarations, and nested style rules can all begin
    /// with bare identifiers. In order to know which statement type to produce,
    /// we need to disambiguate them. We use the following criteria:
    ///
    /// * If the entity starts with an identifier followed by a period and a
    ///   dollar sign, it's a variable declaration. This is the simplest case,
    ///   because `.$` is used in and only in variable declarations.
    ///
    /// * If the entity doesn't start with an identifier followed by a colon,
    ///   it's a selector. There are some additional mostly-unimportant cases
    ///   here to support various declaration hacks.
    ///
    /// * If the colon is followed by another colon, it's a selector.
    ///
    /// * Otherwise, if the colon is followed by anything other than
    ///   interpolation or a character that's valid as the beginning of an
    ///   identifier, it's a declaration.
    ///
    /// * If the colon is followed by interpolation or a valid identifier, try
    ///   parsing it as a declaration value. If this fails, backtrack and parse
    ///   it as a selector.
    ///
    /// * If the declaration value is valid but is followed by "{", backtrack and
    ///   parse it as a selector anyway. This ensures that ".foo:bar {" is always
    ///   parsed as a selector and never as a property with nested properties
    ///   beneath it.
    // todo: potentially we read the property to a string already since properties
    // are more common than selectors? this seems to be annihilating our performance
    fn parse_style_value_when_no_space_after_semicolon(&mut self) -> Option<Vec<Token>> {
        let mut toks = Vec::new();
        while let Some(tok) = self.toks.peek() {
            match tok.kind {
                ';' | '}' => {
                    self.toks.reset_view();
                    break;
                }
                '{' => {
                    self.toks.reset_view();
                    return None;
                }
                '(' => {
                    toks.push(*tok);
                    self.toks.peek_forward(1);
                    let mut scope = 0;
                    while let Some(tok) = self.toks.peek() {
                        match tok.kind {
                            ')' => {
                                if scope == 0 {
                                    toks.push(*tok);
                                    self.toks.peek_forward(1);
                                    break;
                                } else {
                                    scope -= 1;
                                    toks.push(*tok);
                                    self.toks.peek_forward(1);
                                }
                            }
                            '(' => {
                                toks.push(*tok);
                                self.toks.peek_forward(1);
                                scope += 1;
                            }
                            _ => {
                                toks.push(*tok);
                                self.toks.peek_forward(1);
                            }
                        }
                    }
                }
                _ => {
                    toks.push(*tok);
                    self.toks.peek_forward(1);
                }
            }
        }
        Some(toks)
    }

    fn is_selector_or_style(&mut self) -> SassResult<SelectorOrStyle> {
        if let Some(first_char) = self.toks.peek() {
            if first_char.kind == '#' {
                if !matches!(self.toks.peek_forward(1), Some(Token { kind: '{', .. })) {
                    self.toks.reset_view();
                    return Ok(SelectorOrStyle::Selector(String::new()));
                }
                self.toks.reset_view();
            } else if !is_name_start(first_char.kind) && first_char.kind != '-' {
                return Ok(SelectorOrStyle::Selector(String::new()));
            }
        }

        let mut property = self.parse_identifier()?.node;
        let whitespace_after_property = self.whitespace();

        if let Some(Token { kind: ':', .. }) = self.toks.peek() {
            self.toks.next();
            if let Some(Token { kind, .. }) = self.toks.peek() {
                return Ok(match kind {
                    ':' => {
                        if whitespace_after_property {
                            property.push(' ');
                        }
                        property.push(':');
                        SelectorOrStyle::Selector(property)
                    }
                    c if is_name(*c) => {
                        if let Some(toks) = self.parse_style_value_when_no_space_after_semicolon() {
                            let len = toks.len();
                            if let Ok(val) = self.parse_value_from_vec(toks) {
                                self.toks.take(len).for_each(drop);
                                return Ok(SelectorOrStyle::Style(property, Some(Box::new(val))));
                            }
                        }

                        if whitespace_after_property {
                            property.push(' ');
                        }
                        property.push(':');
                        return Ok(SelectorOrStyle::Selector(property));
                    }
                    _ => SelectorOrStyle::Style(property, None),
                });
            }
        } else {
            if whitespace_after_property {
                property.push(' ');
            }
            return Ok(SelectorOrStyle::Selector(property));
        }
        Err(("expected \"{\".", self.span_before).into())
    }

    fn parse_property(&mut self, mut super_property: String) -> SassResult<String> {
        let property = self.parse_identifier()?;
        self.whitespace_or_comment();
        if let Some(Token { kind: ':', .. }) = self.toks.peek() {
            self.toks.next();
            self.whitespace_or_comment();
        } else {
            return Err(("Expected \":\".", property.span).into());
        }

        if super_property.is_empty() {
            Ok(property.node)
        } else {
            super_property.reserve(1 + property.node.len());
            super_property.push('-');
            super_property.push_str(&property.node);
            Ok(super_property)
        }
    }

    fn parse_style_value(&mut self) -> SassResult<Spanned<Value>> {
        self.parse_value()
    }

    fn parse_style_group(&mut self, super_property: String) -> SassResult<Vec<Style>> {
        let mut styles = Vec::new();
        self.whitespace();
        while let Some(tok) = self.toks.peek().cloned() {
            match tok.kind {
                '{' => {
                    self.toks.next();
                    self.whitespace();
                    loop {
                        let property = self.parse_property(super_property.clone())?;
                        if let Some(tok) = self.toks.peek() {
                            if tok.kind == '{' {
                                styles.append(&mut self.parse_style_group(property)?);
                                self.whitespace();
                                if let Some(tok) = self.toks.peek() {
                                    if tok.kind == '}' {
                                        self.toks.next();
                                        self.whitespace();
                                        return Ok(styles);
                                    } else {
                                        continue;
                                    }
                                }
                                continue;
                            }
                        }
                        let value = self.parse_style_value()?;
                        match self.toks.peek() {
                            Some(Token { kind: '}', .. }) => {
                                styles.push(Style { property, value });
                            }
                            Some(Token { kind: ';', .. }) => {
                                self.toks.next();
                                self.whitespace();
                                styles.push(Style { property, value });
                            }
                            Some(Token { kind: '{', .. }) => {
                                styles.push(Style {
                                    property: property.clone(),
                                    value,
                                });
                                styles.append(&mut self.parse_style_group(property)?);
                            }
                            Some(..) | None => {
                                self.whitespace();
                                styles.push(Style { property, value });
                            }
                        }
                        if let Some(tok) = self.toks.peek() {
                            match tok.kind {
                                '}' => {
                                    self.toks.next();
                                    self.whitespace();
                                    return Ok(styles);
                                }
                                _ => continue,
                            }
                        }
                    }
                }
                _ => {
                    let value = self.parse_style_value()?;
                    let t = self
                        .toks
                        .peek()
                        .ok_or(("expected more input.", value.span))?;
                    match t.kind {
                        ';' => {
                            self.toks.next();
                            self.whitespace();
                        }
                        '{' => {
                            let mut v = vec![Style {
                                property: super_property.clone(),
                                value,
                            }];
                            v.append(&mut self.parse_style_group(super_property)?);
                            return Ok(v);
                        }
                        _ => {}
                    }
                    return Ok(vec![Style {
                        property: super_property,
                        value,
                    }]);
                }
            }
        }
        Ok(styles)
    }

    pub fn parse_selector(
        &mut self,
        allows_parent: bool,
        from_fn: bool,
        mut string: String,
    ) -> SassResult<Selector> {
        let mut span = if let Some(tok) = self.toks.peek() {
            tok.pos()
        } else {
            return Err(("expected \"{\".", self.span_before).into());
        };

        let mut found_curly = false;

        while let Some(tok) = self.toks.next() {
            span = span.merge(tok.pos());
            match tok.kind {
                '#' => {
                    if let Some(Token { kind: '{', .. }) = self.toks.peek().cloned() {
                        self.toks.next();
                        string.push_str(&self.parse_interpolation()?.to_css_string(span)?);
                    } else {
                        string.push('#');
                    }
                }
                ',' => {
                    while let Some(c) = string.pop() {
                        if c == ' ' || c == ',' {
                            continue;
                        }
                        string.push(c);
                        string.push(',');
                        break;
                    }
                }
                '/' => {
                    if self.toks.peek().is_none() {
                        return Err(("Expected selector.", tok.pos()).into());
                    }
                    self.parse_comment()?;
                    self.whitespace();
                    string.push(' ');
                }
                '{' => {
                    found_curly = true;
                    break;
                }
                c => string.push(c),
            }
        }

        if !found_curly && !from_fn {
            return Err(("expected \"{\".", span).into());
        }

        while let Some(c) = string.pop() {
            if c == ' ' || c == ',' || c == '\t' {
                continue;
            }
            string.push(c);
            break;
        }

        let sel_toks: Vec<Token> = string.chars().map(|x| Token::new(span, x)).collect();

        let mut iter = sel_toks.into_iter().peekmore();

        Ok(Selector(
            SelectorParser::new(
                &mut Parser {
                    toks: &mut iter,
                    map: self.map,
                    path: self.path,
                    scopes: self.scopes,
                    global_scope: self.global_scope,
                    super_selectors: self.super_selectors,
                    span_before: self.span_before,
                    content: self.content.clone(),
                    in_mixin: self.in_mixin,
                    in_function: self.in_function,
                    in_control_flow: self.in_control_flow,
                    at_root: self.at_root,
                    at_root_has_selector: self.at_root_has_selector,
                },
                allows_parent,
                true,
                span,
            )
            .parse()?,
        ))
    }

    fn parse_variable_declaration(&mut self) -> SassResult<()> {
        assert!(matches!(self.toks.next(), Some(Token { kind: '$', .. })));
        let ident: Identifier = self.parse_identifier_no_interpolation(false)?.node.into();
        self.whitespace();
        if !matches!(self.toks.next(), Some(Token { kind: ':', .. })) {
            return Err(("expected \":\".", self.span_before).into());
        }
        let value = self.parse_variable_value()?;

        if value.global && !value.default {
            self.global_scope
                .insert_var(ident.clone(), value.value.clone())?;
        }

        if value.default {
            if self.at_root && !self.in_control_flow {
                if !self.global_scope.var_exists_no_global(&ident) {
                    self.global_scope.insert_var(ident, value.value)?;
                }
            } else {
                if value.global && !self.global_scope.var_exists_no_global(&ident) {
                    self.global_scope
                        .insert_var(ident.clone(), value.value.clone())?;
                }
                if !self.scopes.last().var_exists_no_global(&ident) {
                    self.scopes.last_mut().insert_var(ident, value.value)?;
                }
            }
        } else if self.at_root {
            if self.in_control_flow {
                if self.global_scope.var_exists_no_global(&ident) {
                    self.global_scope.insert_var(ident, value.value)?;
                } else {
                    self.scopes.last_mut().insert_var(ident, value.value)?;
                }
            } else {
                self.global_scope.insert_var(ident, value.value)?;
            }
        } else {
            let len = self.scopes.len();
            for (_, scope) in self
                .scopes
                .iter_mut()
                .enumerate()
                .filter(|(i, _)| *i != len)
            {
                if scope.var_exists_no_global(&ident) {
                    scope.insert_var(ident.clone(), value.value.clone())?;
                }
            }
            self.scopes.last_mut().insert_var(ident, value.value)?;
        }
        Ok(())
    }

    fn parse_variable_value(&mut self) -> SassResult<VariableValue> {
        let mut default = false;
        let mut global = false;

        let mut val_toks = Vec::new();
        let mut nesting = 0;
        while let Some(tok) = self.toks.peek() {
            match tok.kind {
                ';' => {
                    self.toks.next();
                    break;
                }
                '\\' => {
                    val_toks.push(self.toks.next().unwrap());
                    if self.toks.peek().is_some() {
                        val_toks.push(self.toks.next().unwrap());
                    }
                }
                '"' | '\'' => {
                    let quote = self.toks.next().unwrap();
                    val_toks.push(quote);
                    val_toks.extend(read_until_closing_quote(self.toks, quote.kind)?);
                }
                '#' => {
                    val_toks.push(self.toks.next().unwrap());
                    match self.toks.peek() {
                        Some(Token { kind: '{', .. }) => nesting += 1,
                        Some(Token { kind: ';', .. }) => break,
                        Some(Token { kind: '}', .. }) => {
                            if nesting == 0 {
                                break;
                            } else {
                                nesting -= 1;
                            }
                        }
                        Some(..) | None => {}
                    }
                    val_toks.push(self.toks.next().unwrap());
                }
                '{' => break,
                '}' => {
                    if nesting == 0 {
                        break;
                    } else {
                        nesting -= 1;
                        val_toks.push(self.toks.next().unwrap());
                    }
                }
                '/' => {
                    let next = self.toks.next().unwrap();
                    match self.toks.peek() {
                        Some(Token { kind: '/', .. }) => read_until_newline(self.toks),
                        Some(..) | None => val_toks.push(next),
                    };
                    continue;
                }
                '(' => {
                    val_toks.push(self.toks.next().unwrap());
                    val_toks.extend(read_until_closing_paren(self.toks)?);
                }
                '!' => {
                    let pos = tok.pos();
                    if self.toks.peek_forward(1).is_none() {
                        return Err(("Expected identifier.", pos).into());
                    }
                    // todo: it should not be possible to declare the same flag more than once
                    let mut ident = peek_ident_no_interpolation(self.toks, false, pos)?;
                    ident.node.make_ascii_lowercase();
                    match ident.node.as_str() {
                        "global" => {
                            self.toks.take(7).for_each(drop);
                            global = true;
                        }
                        "default" => {
                            self.toks.take(8).for_each(drop);
                            default = true;
                        }
                        "important" => {
                            self.toks.reset_view();
                            val_toks.push(self.toks.next().unwrap());
                            continue;
                        }
                        _ => {
                            return Err(("Invalid flag name.", ident.span).into());
                        }
                    }
                }
                _ => val_toks.push(self.toks.next().unwrap()),
            }
        }
        let val = self.parse_value_from_vec(val_toks)?;
        Ok(VariableValue::new(val, global, default))
    }

    /// Eat and return the contents of a comment.
    ///
    /// This function assumes that the starting "/" has already been consumed
    /// The entirety of the comment, including the ending "*/" for multiline comments,
    /// is consumed. Note that the ending "*/" is not included in the output.
    #[allow(clippy::eval_order_dependence)]
    pub fn parse_comment(&mut self) -> SassResult<Spanned<Comment>> {
        let mut span = self.span_before;
        Ok(Spanned {
            node: match self.toks.next() {
                Some(Token { kind: '/', .. }) => {
                    while let Some(tok) = self.toks.next() {
                        if tok.kind == '\n' {
                            break;
                        }
                        span = span.merge(tok.pos);
                    }

                    Comment::Silent
                }
                Some(Token { kind: '*', .. }) => {
                    let mut comment = String::new();
                    while let Some(tok) = self.toks.next() {
                        span = span.merge(tok.pos());
                        match (tok.kind, self.toks.peek()) {
                            ('*', Some(Token { kind: '/', .. })) => {
                                self.toks.next();
                                break;
                            }
                            ('#', Some(Token { kind: '{', .. })) => {
                                self.toks.next();
                                comment.push_str(&self.parse_interpolation()?.to_css_string(span)?);
                                continue;
                            }
                            (..) => comment.push(tok.kind),
                        }
                    }
                    Comment::Loud(comment)
                }
                Some(..) | None => return Err(("expected selector.", self.span_before).into()),
            },
            span,
        })
    }

    pub fn parse_interpolation(&mut self) -> SassResult<Spanned<Value>> {
        let toks = read_until_closing_curly_brace(self.toks)?;
        let val = self.parse_value_from_vec(toks)?;
        match self.toks.next() {
            Some(Token { kind: '}', .. }) => {}
            Some(..) | None => return Err(("expected \"}\".", val.span).into()),
        }
        Ok(Spanned {
            node: val.node.eval(val.span)?.node.unquote(),
            span: val.span,
        })
    }

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

    fn read_until_newline(&mut self) {
        while let Some(tok) = self.toks.next() {
            if tok.kind == '\n' {
                break;
            }
        }
    }

    fn whitespace_or_comment(&mut self) -> bool {
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
                        while let Some(tok) = self.toks.next() {
                            if tok.kind == '*' {
                                if let Some(Token { kind: '/', .. }) = self.toks.next() {
                                    break;
                                }
                            }
                        }
                    }
                    Some(Token { kind: '/', .. }) => {
                        found_whitespace = true;
                        self.read_until_newline();
                    }
                    _ => {
                        self.toks.reset_view();
                        return found_whitespace;
                    }
                },
                _ => return found_whitespace,
            }
        }
        found_whitespace
    }
}

impl<'a> Parser<'a> {
    fn parse_mixin(&mut self) -> SassResult<()> {
        self.whitespace();
        let Spanned { node: name, span } = self.parse_identifier()?;
        self.whitespace();
        let args = match self.toks.next() {
            Some(Token { kind: '(', .. }) => self.parse_func_args()?,
            Some(Token { kind: '{', .. }) => FuncArgs::new(),
            Some(t) => return Err(("expected \"{\".", t.pos()).into()),
            None => return Err(("expected \"{\".", span).into()),
        };

        self.whitespace();

        let mut body = read_until_closing_curly_brace(self.toks)?;
        body.push(self.toks.next().unwrap());

        // todo: `@include` can only give content when `@content` is present within the body
        // if `@content` is *not* present and `@include` attempts to give a body, we throw an error
        // `Error: Mixin doesn't accept a content block.`
        //
        // this is blocked on figuring out just how to check for this. presumably we could have a check
        // not when parsing initially, but rather when `@include`ing to see if an `@content` was found.

        let mixin = Mixin::new(Scope::new(), args, body, false);

        if self.at_root {
            self.global_scope.insert_mixin(name, mixin);
        } else {
            self.scopes.last_mut().insert_mixin(name, mixin);
        }
        Ok(())
    }

    fn parse_include(&mut self) -> SassResult<Vec<Stmt>> {
        self.whitespace_or_comment();
        let name = self.parse_identifier()?;

        self.whitespace_or_comment();

        let mut has_content = false;

        let args = match self.toks.next() {
            Some(Token { kind: ';', .. }) => CallArgs::new(name.span),
            Some(Token { kind: '(', .. }) => {
                let tmp = self.parse_call_args()?;
                self.whitespace_or_comment();
                if let Some(tok) = self.toks.peek() {
                    match tok.kind {
                        ';' => {
                            self.toks.next();
                        }
                        '{' => {
                            self.toks.next();
                            has_content = true
                        }
                        _ => {}
                    }
                }
                tmp
            }
            Some(Token { kind: '{', .. }) => {
                has_content = true;
                CallArgs::new(name.span)
            }
            Some(Token { pos, .. }) => return Err(("expected \"{\".", pos).into()),
            None => return Err(("expected \"{\".", name.span).into()),
        };

        self.whitespace();

        let content = if has_content {
            Some(self.parse_content()?)
        } else {
            None
        };

        let mut mixin = self.scopes.last().get_mixin(name, self.global_scope)?;
        self.eval_mixin_args(&mut mixin, args)?;

        self.scopes.push(mixin.scope);

        let body = Parser {
            toks: &mut mixin.body,
            map: self.map,
            path: self.path,
            scopes: self.scopes,
            global_scope: self.global_scope,
            super_selectors: self.super_selectors,
            span_before: self.span_before,
            in_mixin: true,
            in_function: self.in_function,
            in_control_flow: self.in_control_flow,
            content,
            at_root: false,
            at_root_has_selector: self.at_root_has_selector,
        }
        .parse()?;

        Ok(body)
    }

    fn parse_content(&mut self) -> SassResult<Vec<Stmt>> {
        self.parse_stmt()
    }

    fn parse_function(&mut self) -> SassResult<()> {
        if self.in_mixin {
            return Err((
                "Mixins may not contain function declarations.",
                self.span_before,
            )
                .into());
        }

        self.whitespace_or_comment();
        let Spanned { node: name, span } = self.parse_identifier()?;
        self.whitespace_or_comment();
        let args = match self.toks.next() {
            Some(Token { kind: '(', .. }) => self.parse_func_args()?,
            Some(Token { pos, .. }) => return Err(("expected \"(\".", pos).into()),
            None => return Err(("expected \"(\".", span).into()),
        };

        self.whitespace();

        let mut body = read_until_closing_curly_brace(self.toks)?;
        body.push(self.toks.next().unwrap());
        self.whitespace();

        let function = Function::new(self.scopes.last().clone(), args, body, span);

        if self.at_root {
            self.global_scope.insert_fn(name, function);
        } else {
            self.scopes.last_mut().insert_fn(name, function);
        }
        Ok(())
    }

    fn parse_if(&mut self) -> SassResult<Vec<Stmt>> {
        self.whitespace_or_comment();
        let mut branches = Vec::new();
        let init_cond_toks = read_until_open_curly_brace(self.toks)?;
        if init_cond_toks.is_empty() {
            return Err(("Expected expression.", self.span_before).into());
        }
        let span_before = match self.toks.next() {
            Some(t) => t.pos,
            None => return Err(("Expected expression.", self.span_before).into()),
        };
        self.whitespace_or_comment();
        let mut init_toks = read_until_closing_curly_brace(self.toks)?;
        if let Some(tok) = self.toks.next() {
            init_toks.push(tok);
        } else {
            return Err(("expected \"}\".", span_before).into());
        }
        self.whitespace();

        branches.push(Branch::new(init_cond_toks, init_toks));

        let mut else_ = Vec::new();

        loop {
            if let Some(Token { kind: '@', pos }) = self.toks.peek().cloned() {
                self.toks.peek_forward(1);
                let ident = peek_ident_no_interpolation(self.toks, false, pos)?;
                if ident.as_str() != "else" {
                    self.toks.reset_view();
                    break;
                }
                self.toks.take(4).for_each(drop);
            } else {
                break;
            }
            self.whitespace();
            if let Some(tok) = self.toks.next() {
                self.whitespace();
                match tok.kind.to_ascii_lowercase() {
                    'i' if self.toks.next().unwrap().kind.to_ascii_lowercase() == 'f' => {
                        self.toks.next();
                        let cond = read_until_open_curly_brace(self.toks)?;
                        self.toks.next();
                        self.whitespace();
                        branches.push(Branch::new(
                            cond,
                            read_until_closing_curly_brace(self.toks)?,
                        ));
                        self.toks.next();
                        self.whitespace();
                    }
                    '{' => {
                        else_ = read_until_closing_curly_brace(self.toks)?;
                        self.toks.next();
                        break;
                    }
                    _ => {
                        return Err(("expected \"{\".", tok.pos()).into());
                    }
                }
            } else {
                break;
            }
        }
        self.whitespace();

        for branch in branches {
            self.span_before = branch.cond.first().unwrap().pos;
            let cond = self.parse_value_from_vec(branch.cond)?;
            if cond.node.is_true(cond.span)? {
                return Parser {
                    toks: &mut branch.toks.into_iter().peekmore(),
                    map: self.map,
                    path: self.path,
                    scopes: self.scopes,
                    global_scope: &mut self.global_scope,
                    super_selectors: self.super_selectors,
                    span_before: self.span_before,
                    content: self.content.clone(),
                    in_mixin: self.in_mixin,
                    in_function: self.in_function,
                    in_control_flow: true,
                    at_root: self.at_root,
                    at_root_has_selector: self.at_root_has_selector,
                }
                .parse();
            }
        }
        if else_.is_empty() {
            return Ok(Vec::new());
        }
        Parser {
            toks: &mut else_.into_iter().peekmore(),
            map: self.map,
            path: self.path,
            scopes: self.scopes,
            global_scope: &mut self.global_scope,
            super_selectors: self.super_selectors,
            span_before: self.span_before,
            content: self.content.clone(),
            in_mixin: self.in_mixin,
            in_function: self.in_function,
            in_control_flow: true,
            at_root: self.at_root,
            at_root_has_selector: self.at_root_has_selector,
        }
        .parse()
    }

    fn parse_for(&mut self) -> SassResult<Vec<Stmt>> {
        self.whitespace();
        let next = self
            .toks
            .next()
            .ok_or(("expected \"$\".", self.span_before))?;
        let var = match next.kind {
            '$' => self.parse_identifier_no_interpolation(false)?,
            _ => return Err(("expected \"$\".", self.span_before).into()),
        };
        self.whitespace();
        if self.toks.peek().is_none() {
            return Err(("Expected \"from\".", var.span).into());
        }
        self.span_before = self.toks.peek().unwrap().pos;
        if self.parse_identifier()?.node.to_ascii_lowercase() != "from" {
            return Err(("Expected \"from\".", var.span).into());
        }
        self.whitespace();
        let mut from_toks = Vec::new();
        let mut through = 0;
        while let Some(tok) = self.toks.peek().cloned() {
            match tok.kind {
                't' | 'T' | '\\' => {
                    let ident = peek_ident_no_interpolation(self.toks, false, tok.pos)?;
                    match ident.node.to_ascii_lowercase().as_str() {
                        "through" => {
                            through = 1;
                            // todo: it should take more if there were escapes
                            self.toks.take(7).for_each(drop);
                            break;
                        }
                        "to" => {
                            // todo: it should take more if there were escapes
                            self.toks.take(2).for_each(drop);
                            break;
                        }
                        _ => {
                            return Err(("Invalid flag name.", ident.span).into());
                        }
                    }
                }
                '{' => {
                    return Err(("Expected \"to\" or \"through\".", tok.pos()).into());
                }
                _ => from_toks.push(self.toks.next().unwrap()),
            }
        }
        self.whitespace();
        let from_val = self.parse_value_from_vec(from_toks)?;
        let from = match from_val.node.eval(from_val.span)?.node {
            Value::Dimension(n, _) => match n.to_integer().to_isize() {
                Some(v) => v,
                None => return Err((format!("{} is not a int.", n), from_val.span).into()),
            },
            v => {
                return Err((
                    format!("{} is not an integer.", v.inspect(from_val.span)?),
                    from_val.span,
                )
                    .into())
            }
        };

        let to_toks = read_until_open_curly_brace(self.toks)?;
        self.toks.next();
        let to_val = self.parse_value_from_vec(to_toks)?;
        let to = match to_val.node.eval(to_val.span)?.node {
            Value::Dimension(n, _) => match n.to_integer().to_isize() {
                Some(v) => v,
                None => return Err((format!("{} is not a int.", n), to_val.span).into()),
            },
            v => {
                return Err((
                    format!("{} is not an integer.", v.to_css_string(to_val.span)?),
                    to_val.span,
                )
                    .into())
            }
        };
        let body = read_until_closing_curly_brace(self.toks)?;
        self.toks.next();

        self.whitespace();

        let (mut x, mut y);
        // we can't use an inclusive range here
        #[allow(clippy::range_plus_one)]
        let iter: &mut dyn Iterator<Item = isize> = if from < to {
            x = from..(to + through);
            &mut x
        } else {
            y = ((to - through)..(from + 1)).skip(1).rev();
            &mut y
        };

        let mut stmts = Vec::new();

        self.scopes.push(self.scopes.last().clone());

        for i in iter {
            self.scopes.last_mut().insert_var(
                var.node.clone(),
                Spanned {
                    node: Value::Dimension(Number::from(i), Unit::None),
                    span: var.span,
                },
            )?;
            if self.in_function {
                let these_stmts = Parser {
                    toks: &mut body.clone().into_iter().peekmore(),
                    map: self.map,
                    path: self.path,
                    scopes: self.scopes,
                    global_scope: &mut self.global_scope,
                    super_selectors: self.super_selectors,
                    span_before: self.span_before,
                    content: self.content.clone(),
                    in_mixin: self.in_mixin,
                    in_function: self.in_function,
                    in_control_flow: true,
                    at_root: self.at_root,
                    at_root_has_selector: self.at_root_has_selector,
                }
                .parse()?;
                if !these_stmts.is_empty() {
                    return Ok(these_stmts);
                }
            } else {
                stmts.append(
                    &mut Parser {
                        toks: &mut body.clone().into_iter().peekmore(),
                        map: self.map,
                        path: self.path,
                        scopes: self.scopes,
                        global_scope: &mut self.global_scope,
                        super_selectors: self.super_selectors,
                        span_before: self.span_before,
                        content: self.content.clone(),
                        in_mixin: self.in_mixin,
                        in_function: self.in_function,
                        in_control_flow: true,
                        at_root: self.at_root,
                        at_root_has_selector: self.at_root_has_selector,
                    }
                    .parse()?,
                );
            }
        }

        self.scopes.pop();

        Ok(stmts)
    }

    fn parse_while(&mut self) -> SassResult<Vec<Stmt>> {
        self.whitespace();
        let cond = read_until_open_curly_brace(self.toks)?;

        if cond.is_empty() {
            return Err(("Expected expression.", self.span_before).into());
        }

        self.toks.next();

        let mut body = read_until_closing_curly_brace(self.toks)?;

        body.push(self.toks.next().unwrap());

        self.whitespace();

        let mut stmts = Vec::new();
        let mut val = self.parse_value_from_vec(cond.clone())?;
        self.scopes.push(self.scopes.last().clone());
        while val.node.is_true(val.span)? {
            if self.in_function {
                let these_stmts = Parser {
                    toks: &mut body.clone().into_iter().peekmore(),
                    map: self.map,
                    path: self.path,
                    scopes: self.scopes,
                    global_scope: self.global_scope,
                    super_selectors: self.super_selectors,
                    span_before: self.span_before,
                    content: self.content.clone(),
                    in_mixin: self.in_mixin,
                    in_function: self.in_function,
                    in_control_flow: true,
                    at_root: self.at_root,
                    at_root_has_selector: self.at_root_has_selector,
                }
                .parse()?;
                if !these_stmts.is_empty() {
                    return Ok(these_stmts);
                }
            } else {
                stmts.append(
                    &mut Parser {
                        toks: &mut body.clone().into_iter().peekmore(),
                        map: self.map,
                        path: self.path,
                        scopes: self.scopes,
                        global_scope: &mut self.global_scope,
                        super_selectors: self.super_selectors,
                        span_before: self.span_before,
                        content: self.content.clone(),
                        in_mixin: self.in_mixin,
                        in_function: self.in_function,
                        in_control_flow: true,
                        at_root: self.at_root,
                        at_root_has_selector: self.at_root_has_selector,
                    }
                    .parse()?,
                );
            }
            val = self.parse_value_from_vec(cond.clone())?;
        }
        self.scopes.pop();

        Ok(stmts)
    }

    fn parse_each(&mut self) -> SassResult<Vec<Stmt>> {
        self.whitespace();
        let mut vars = Vec::new();

        loop {
            let next = self
                .toks
                .next()
                .ok_or(("expected \"$\".", self.span_before))?;

            match next.kind {
                '$' => vars.push(self.parse_identifier()?),
                _ => return Err(("expected \"$\".", next.pos()).into()),
            }
            self.whitespace();
            if self
                .toks
                .peek()
                .ok_or(("expected \"$\".", vars[vars.len() - 1].span))?
                .kind
                == ','
            {
                self.toks.next();
                self.whitespace();
            } else {
                break;
            }
        }
        let i = self.parse_identifier()?;
        if i.node.to_ascii_lowercase() != "in" {
            return Err(("Expected \"in\".", i.span).into());
        }
        self.whitespace();
        let iter_val_toks = read_until_open_curly_brace(self.toks)?;
        let iter_val = self.parse_value_from_vec(iter_val_toks)?;
        let iter = match iter_val.node.eval(iter_val.span)?.node {
            Value::List(v, ..) => v,
            Value::Map(m) => m
                .into_iter()
                .map(|(k, v)| Value::List(vec![k, v], ListSeparator::Space, Brackets::None))
                .collect(),
            v => vec![v],
        };
        self.toks.next();
        self.whitespace();
        let mut body = read_until_closing_curly_brace(self.toks)?;
        body.push(self.toks.next().unwrap());
        self.whitespace();

        let mut stmts = Vec::new();

        for row in iter {
            let this_iterator = match row {
                Value::List(v, ..) => v,
                Value::Map(m) => m
                    .into_iter()
                    .map(|(k, v)| Value::List(vec![k, v], ListSeparator::Space, Brackets::None))
                    .collect(),
                v => vec![v],
            };

            if vars.len() == 1 {
                if this_iterator.len() == 1 {
                    self.scopes.last_mut().insert_var(
                        &vars[0].node,
                        Spanned {
                            node: this_iterator[0].clone(),
                            span: vars[0].span,
                        },
                    )?;
                } else {
                    self.scopes.last_mut().insert_var(
                        &vars[0].node,
                        Spanned {
                            node: Value::List(this_iterator, ListSeparator::Space, Brackets::None),
                            span: vars[0].span,
                        },
                    )?;
                }
            } else {
                for (var, val) in vars.clone().into_iter().zip(
                    this_iterator
                        .into_iter()
                        .chain(std::iter::once(Value::Null).cycle()),
                ) {
                    self.scopes.last_mut().insert_var(
                        &var.node,
                        Spanned {
                            node: val,
                            span: var.span,
                        },
                    )?;
                }
            }

            if self.in_function {
                let these_stmts = Parser {
                    toks: &mut body.clone().into_iter().peekmore(),
                    map: self.map,
                    path: self.path,
                    scopes: self.scopes,
                    global_scope: &mut self.global_scope,
                    super_selectors: self.super_selectors,
                    span_before: self.span_before,
                    content: self.content.clone(),
                    in_mixin: self.in_mixin,
                    in_function: self.in_function,
                    in_control_flow: true,
                    at_root: self.at_root,
                    at_root_has_selector: self.at_root_has_selector,
                }
                .parse()?;
                if !these_stmts.is_empty() {
                    return Ok(these_stmts);
                }
            } else {
                stmts.append(
                    &mut Parser {
                        toks: &mut body.clone().into_iter().peekmore(),
                        map: self.map,
                        path: self.path,
                        scopes: self.scopes,
                        global_scope: &mut self.global_scope,
                        super_selectors: self.super_selectors,
                        span_before: self.span_before,
                        content: self.content.clone(),
                        in_mixin: self.in_mixin,
                        in_function: self.in_function,
                        in_control_flow: true,
                        at_root: self.at_root,
                        at_root_has_selector: self.at_root_has_selector,
                    }
                    .parse()?,
                );
            }
        }

        Ok(stmts)
    }

    fn parse_return(&mut self) -> SassResult<Value> {
        let toks = read_until_semicolon_or_closing_curly_brace(self.toks)?;
        let v = self.parse_value_from_vec(toks)?;
        if let Some(Token { kind: ';', .. }) = self.toks.peek() {
            self.toks.next();
        }
        Ok(v.node)
    }

    pub fn eval_function(&mut self, mut function: Function, args: CallArgs) -> SassResult<Value> {
        self.scopes.push(self.scopes.last().clone());
        self.eval_fn_args(&mut function, args)?;
        let mut return_value = Parser {
            toks: &mut function.body.into_iter().peekmore(),
            map: self.map,
            path: self.path,
            scopes: self.scopes,
            global_scope: self.global_scope,
            super_selectors: self.super_selectors,
            span_before: self.span_before,
            content: self.content.clone(),
            in_mixin: self.in_mixin,
            in_function: true,
            in_control_flow: self.in_control_flow,
            at_root: false,
            at_root_has_selector: self.at_root_has_selector,
        }
        .parse()?;
        self.scopes.pop();
        debug_assert!(return_value.len() <= 1);
        match return_value
            .pop()
            .ok_or(("Function finished without @return.", self.span_before))?
        {
            Stmt::Return(v) => Ok(v),
            _ => todo!("should be unreachable"),
        }
    }

    fn eval_fn_args(&mut self, function: &mut Function, mut args: CallArgs) -> SassResult<()> {
        for (idx, arg) in function.args.0.iter_mut().enumerate() {
            if arg.is_variadic {
                let span = args.span();
                let arg_list = Value::ArgList(self.variadic_args(args)?);
                self.scopes.last_mut().insert_var(
                    arg.name.clone(),
                    Spanned {
                        node: arg_list,
                        span,
                    },
                )?;
                break;
            }
            let val = match args.get(idx, arg.name.clone()) {
                Some(v) => self.parse_value_from_vec(v)?,
                None => match arg.default.as_mut() {
                    Some(v) => self.parse_value_from_vec(mem::take(v))?,
                    None => {
                        return Err(
                            (format!("Missing argument ${}.", &arg.name), args.span()).into()
                        )
                    }
                },
            };
            self.scopes
                .last_mut()
                .insert_var(mem::take(&mut arg.name), val)?;
        }
        Ok(())
    }

    fn eval_mixin_args(&mut self, mixin: &mut Mixin, mut args: CallArgs) -> SassResult<()> {
        let mut scope = self.scopes.last().clone();
        for (idx, arg) in mixin.args.0.iter_mut().enumerate() {
            if arg.is_variadic {
                let span = args.span();
                // todo: does this get the most recent scope?
                let arg_list = Value::ArgList(self.variadic_args(args)?);
                mixin.scope.insert_var(
                    arg.name.clone(),
                    Spanned {
                        node: arg_list,
                        span,
                    },
                )?;
                break;
            }
            let val = match args.get(idx, arg.name.clone()) {
                Some(v) => self.parse_value_from_vec(v)?,
                None => match arg.default.as_mut() {
                    Some(v) => self.parse_value_from_vec(mem::take(v))?,
                    None => {
                        return Err(
                            (format!("Missing argument ${}.", &arg.name), args.span()).into()
                        )
                    }
                },
            };
            scope.insert_var(arg.name.clone(), val.clone())?;
            mixin.scope.insert_var(mem::take(&mut arg.name), val)?;
        }
        Ok(())
    }

    fn parse_unknown_at_rule(&mut self, name: String) -> SassResult<Stmt> {
        let mut params = String::new();
        self.whitespace();
        if let Some(Token { kind: ';', .. }) | None = self.toks.peek() {
            self.toks.next();
            return Ok(Stmt::UnknownAtRule {
                name,
                super_selector: Selector::new(),
                params: String::new(),
                body: Vec::new(),
            });
        }
        while let Some(tok) = self.toks.next() {
            match tok.kind {
                '{' => break,
                '#' => {
                    if let Some(Token { kind: '{', pos }) = self.toks.peek() {
                        self.span_before = self.span_before.merge(*pos);
                        self.toks.next();
                        let interpolation = self.parse_interpolation()?;
                        params.push_str(&interpolation.node.to_css_string(interpolation.span)?);
                    } else {
                        params.push(tok.kind);
                    }
                    continue;
                }
                '\n' | ' ' | '\t' => {
                    self.whitespace();
                    params.push(' ');
                    continue;
                }
                _ => {}
            }
            params.push(tok.kind);
        }

        let raw_body = self.parse()?;
        let mut rules = Vec::with_capacity(raw_body.len());
        let mut body = Vec::new();

        for stmt in raw_body {
            match stmt {
                Stmt::Style(..) => body.push(stmt),
                _ => rules.push(stmt),
            }
        }

        if !self.super_selectors.last().is_empty() {
            body = vec![Stmt::RuleSet {
                selector: self.super_selectors.last().clone(),
                body,
                super_selector: Selector::new(),
            }];
        }

        body.append(&mut rules);

        Ok(Stmt::UnknownAtRule {
            name,
            super_selector: Selector::new(),
            params: params.trim().to_owned(),
            body,
        })
    }

    fn import(&mut self) -> SassResult<Vec<Stmt>> {
        self.whitespace();
        let mut file_name = String::new();
        let next = match self.toks.next() {
            Some(v) => v,
            None => todo!("expected input after @import"),
        };
        match next.kind {
            q @ '"' | q @ '\'' => {
                file_name.push_str(
                    &self
                        .parse_quoted_string(q)?
                        .node
                        .unquote()
                        .to_css_string(self.span_before)?,
                );
            }
            _ => return Err(("Expected string.", next.pos()).into()),
        }
        if let Some(t) = self.toks.peek() {
            if t.kind == ';' {
                self.toks.next();
            }
        }

        self.whitespace();

        let path: &Path = file_name.as_ref();

        let mut rules = Vec::new();
        let path_buf = if path.is_absolute() {
            // todo: test for absolute path imports
            path.into()
        } else {
            self.path
                .parent()
                .unwrap_or_else(|| Path::new(""))
                .join(path)
        };
        // todo: will panic if path ended in `..`
        let name = path_buf.file_name().unwrap();
        if path_buf.extension() == Some(OsStr::new(".css")) {
            // || name.starts_with("http://") || name.starts_with("https://") {
            todo!("css imports")
        }
        let mut p1 = path_buf.clone();
        p1.push(OsString::from("index.scss"));
        let mut p2 = path_buf.clone();
        p2.push(OsString::from("_index.scss"));
        let paths = [
            path_buf.with_file_name(name).with_extension("scss"),
            path_buf.with_file_name(format!("_{}.scss", name.to_str().unwrap())),
            path_buf,
            p1,
            p2,
        ];
        for name in &paths {
            if name.is_file() {
                let file = self.map.add_file(
                    name.to_string_lossy().into(),
                    String::from_utf8(fs::read(name)?)?,
                );
                let rules2 = Parser {
                    toks: &mut Lexer::new(&file)
                        .collect::<Vec<Token>>()
                        .into_iter()
                        .peekmore(),
                    map: &mut self.map,
                    path: name.as_ref(),
                    scopes: &mut self.scopes,
                    global_scope: &mut self.global_scope,
                    super_selectors: self.super_selectors,
                    span_before: file.span.subspan(0, 0),
                    content: self.content.clone(),
                    in_mixin: self.in_mixin,
                    in_function: self.in_function,
                    in_control_flow: self.in_control_flow,
                    at_root: self.at_root,
                    at_root_has_selector: self.at_root_has_selector,
                }
                .parse()?;

                rules.extend(rules2);
                break;
            }
        }

        Ok(rules)
    }

    fn parse_media(&mut self) -> SassResult<Stmt> {
        let mut params = String::new();
        self.whitespace();
        while let Some(tok) = self.toks.next() {
            match tok.kind {
                '{' => break,
                '#' => {
                    if let Some(Token { kind: '{', pos }) = self.toks.peek().cloned() {
                        self.toks.next();
                        self.span_before = pos;
                        let interpolation = self.parse_interpolation()?;
                        params.push_str(&interpolation.node.to_css_string(interpolation.span)?);
                        continue;
                    } else {
                        params.push(tok.kind);
                    }
                }
                '\n' | ' ' | '\t' => {
                    self.whitespace();
                    params.push(' ');
                    continue;
                }
                _ => {}
            }
            params.push(tok.kind);
        }

        if params.is_empty() {
            return Err(("Expected identifier.", self.span_before).into());
        }

        let raw_body = Parser {
            toks: self.toks,
            map: self.map,
            path: self.path,
            scopes: self.scopes,
            global_scope: self.global_scope,
            super_selectors: self.super_selectors,
            span_before: self.span_before,
            content: self.content.clone(),
            in_mixin: self.in_mixin,
            in_function: self.in_function,
            in_control_flow: self.in_control_flow,
            at_root: false,
            at_root_has_selector: self.at_root_has_selector,
        }
        .parse()?;

        let mut rules = Vec::with_capacity(raw_body.len());
        let mut body = Vec::new();

        for stmt in raw_body {
            match stmt {
                Stmt::Style(..) => body.push(stmt),
                _ => rules.push(stmt),
            }
        }

        if !self.super_selectors.last().is_empty() {
            body = vec![Stmt::RuleSet {
                selector: self.super_selectors.last().clone(),
                body,
                super_selector: Selector::new(),
            }];
        }

        body.append(&mut rules);

        Ok(Stmt::Media {
            super_selector: Selector::new(),
            params: params.trim().to_owned(),
            body,
        })
    }

    fn parse_at_root(&mut self) -> SassResult<Vec<Stmt>> {
        self.whitespace();
        let mut at_root_has_selector = false;
        let at_rule_selector = if matches!(self.toks.peek(), Some(Token { kind: '{', .. })) {
            self.toks.next();
            self.super_selectors.last().clone()
        } else {
            at_root_has_selector = true;
            self.parse_selector(true, false, String::new())?
        }
        .resolve_parent_selectors(self.super_selectors.last(), false);

        self.whitespace();

        let mut body = read_until_closing_curly_brace(self.toks)?;
        body.push(self.toks.next().unwrap());

        self.whitespace();

        let mut styles = Vec::new();
        #[allow(clippy::unnecessary_filter_map)]
        let raw_stmts = Parser {
            toks: &mut body.into_iter().peekmore(),
            map: self.map,
            path: self.path,
            scopes: self.scopes,
            global_scope: self.global_scope,
            super_selectors: &mut NeverEmptyVec::new(at_rule_selector.clone()),
            span_before: self.span_before,
            content: self.content.clone(),
            in_mixin: self.in_mixin,
            in_function: self.in_function,
            in_control_flow: self.in_control_flow,
            at_root: true,
            at_root_has_selector,
        }
        .parse()?
        .into_iter()
        .filter_map(|s| match s {
            Stmt::Style(..) => {
                styles.push(s);
                None
            }
            Stmt::RuleSet { selector, body, .. } if !at_root_has_selector => Some(Stmt::RuleSet {
                super_selector: Selector::new(),
                selector: selector.resolve_parent_selectors(&at_rule_selector, false),
                body,
            }),
            _ => Some(s),
        })
        .collect::<Vec<Stmt>>();
        let mut stmts = vec![Stmt::RuleSet {
            selector: at_rule_selector,
            body: styles,
            super_selector: Selector::new(),
        }];
        stmts.extend(raw_stmts);
        Ok(stmts)
    }

    fn parse_extend(&mut self) -> SassResult<()> {
        todo!("@extend not yet implemented")
    }

    fn parse_supports(&mut self) -> SassResult<Stmt> {
        todo!("@supports not yet implemented")
    }

    fn parse_keyframes(&mut self) -> SassResult<Stmt> {
        todo!("@keyframes not yet implemented")
    }
}

impl<'a> Parser<'a> {
    fn debug(&self, message: &Spanned<Cow<'a, str>>) {
        let loc = self.map.look_up_span(message.span);
        eprintln!(
            "{}:{} Debug: {}",
            loc.file.name(),
            loc.begin.line + 1,
            message.node
        );
    }

    fn warn(&self, message: &Spanned<Cow<'a, str>>) {
        let loc = self.map.look_up_span(message.span);
        eprintln!(
            "Warning: {}\n    {} {}:{}  root stylesheet",
            message.node,
            loc.file.name(),
            loc.begin.line + 1,
            loc.begin.column + 1
        );
    }
}
