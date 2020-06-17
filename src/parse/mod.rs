use std::{convert::TryFrom, path::Path, vec::IntoIter};

use codemap::{CodeMap, Span, Spanned};
use num_traits::cast::ToPrimitive;
use peekmore::{PeekMore, PeekMoreIterator};

use crate::{
    atrule::AtRuleKind,
    common::{Brackets, ListSeparator},
    error::SassResult,
    scope::Scope,
    selector::{Selector, SelectorParser},
    style::Style,
    unit::Unit,
    utils::{
        peek_ident_no_interpolation, read_until_closing_curly_brace, read_until_open_curly_brace,
        read_until_semicolon_or_closing_curly_brace,
    },
    value::{Number, Value},
    {Cow, Token},
};

use common::{Branch, NeverEmptyVec, SelectorOrStyle};

mod args;
pub mod common;
mod function;
mod ident;
mod import;
mod mixin;
mod style;
mod value;
mod variable;

pub(crate) enum Comment {
    Silent,
    Loud(String),
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
            self.span_before = *pos;
            match kind {
                '@' => {
                    self.toks.next();
                    let kind_string = self.parse_identifier()?;
                    self.span_before = kind_string.span;
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
                    return Err(("expected selector.", *pos).into())
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

        self.span_before = span;

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
