use codemap::Spanned;
use num_traits::cast::ToPrimitive;
use peekmore::PeekMore;

use crate::{
    common::Identifier,
    error::SassResult,
    parse::{ContextFlags, Parser, Stmt},
    unit::Unit,
    utils::{
        peek_ident_no_interpolation, read_until_closing_curly_brace, read_until_open_curly_brace,
    },
    value::{Number, Value},
    Token,
};

impl<'a> Parser<'a> {
    pub(super) fn parse_if(&mut self) -> SassResult<Vec<Stmt>> {
        self.whitespace_or_comment();

        let mut found_true = false;
        let mut body = Vec::new();

        let init_cond = self.parse_value(true, &|_| false)?.node;

        self.expect_char('{')?;

        if self.toks.peek().is_none() {
            return Err(("expected \"}\".", self.span_before).into());
        }

        self.whitespace_or_comment();

        if init_cond.is_true() {
            self.scopes.enter_new_scope();
            found_true = true;
            body = Parser {
                toks: self.toks,
                map: self.map,
                path: self.path,
                scopes: self.scopes,
                global_scope: self.global_scope,
                super_selectors: self.super_selectors,
                span_before: self.span_before,
                content: self.content,
                flags: self.flags | ContextFlags::IN_CONTROL_FLOW,
                at_root: self.at_root,
                at_root_has_selector: self.at_root_has_selector,
                extender: self.extender,
                content_scopes: self.content_scopes,
                options: self.options,
                modules: self.modules,
                module_config: self.module_config,
            }
            .parse_stmt()?;
            self.scopes.exit_scope();
        } else {
            self.throw_away_until_closing_curly_brace()?;
        }

        loop {
            self.whitespace_or_comment();
            if let Some(Token { kind: '@', pos }) = self.toks.peek().cloned() {
                self.toks.peek_forward(1);
                let ident = peek_ident_no_interpolation(self.toks, false, pos)?;
                if ident.as_str() != "else" {
                    self.toks.reset_cursor();
                    break;
                }
                self.toks.truncate_iterator_to_cursor();
            } else {
                break;
            }
            self.whitespace_or_comment();
            if let Some(tok) = self.toks.peek().cloned() {
                match tok.kind {
                    'i' if matches!(
                        self.toks.peek_forward(1),
                        Some(Token { kind: 'f', .. }) | Some(Token { kind: 'F', .. })
                    ) =>
                    {
                        self.toks.next();
                        self.toks.next();
                        let cond = if found_true {
                            self.throw_away_until_open_curly_brace()?;
                            false
                        } else {
                            let v = self.parse_value(true, &|_| false)?.node.is_true();
                            self.expect_char('{')?;
                            v
                        };
                        if cond {
                            found_true = true;
                            self.scopes.enter_new_scope();
                            body = Parser {
                                toks: self.toks,
                                map: self.map,
                                path: self.path,
                                scopes: self.scopes,
                                global_scope: self.global_scope,
                                super_selectors: self.super_selectors,
                                span_before: self.span_before,
                                content: self.content,
                                flags: self.flags | ContextFlags::IN_CONTROL_FLOW,
                                at_root: self.at_root,
                                at_root_has_selector: self.at_root_has_selector,
                                extender: self.extender,
                                content_scopes: self.content_scopes,
                                options: self.options,
                                modules: self.modules,
                                module_config: self.module_config,
                            }
                            .parse_stmt()?;
                            self.scopes.exit_scope();
                        } else {
                            self.throw_away_until_closing_curly_brace()?;
                        }
                        self.whitespace();
                    }
                    '{' => {
                        self.toks.next();
                        if found_true {
                            self.throw_away_until_closing_curly_brace()?;
                            break;
                        }

                        self.scopes.enter_new_scope();
                        let tmp = Parser {
                            toks: self.toks,
                            map: self.map,
                            path: self.path,
                            scopes: self.scopes,
                            global_scope: self.global_scope,
                            super_selectors: self.super_selectors,
                            span_before: self.span_before,
                            content: self.content,
                            flags: self.flags | ContextFlags::IN_CONTROL_FLOW,
                            at_root: self.at_root,
                            at_root_has_selector: self.at_root_has_selector,
                            extender: self.extender,
                            content_scopes: self.content_scopes,
                            options: self.options,
                            modules: self.modules,
                            module_config: self.module_config,
                        }
                        .parse_stmt();
                        self.scopes.exit_scope();
                        return tmp;
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

        Ok(body)
    }

    pub(super) fn parse_for(&mut self) -> SassResult<Vec<Stmt>> {
        self.whitespace_or_comment();
        self.expect_char('$')?;

        let var = self
            .parse_identifier_no_interpolation(false)?
            .map_node(|n| n.into());

        self.whitespace_or_comment();
        self.span_before = match self.toks.peek() {
            Some(tok) => tok.pos,
            None => return Err(("Expected \"from\".", var.span).into()),
        };
        if self.parse_identifier()?.node.to_ascii_lowercase() != "from" {
            return Err(("Expected \"from\".", var.span).into());
        }
        self.whitespace_or_comment();

        let from_val = self.parse_value(false, &|toks| match toks.peek() {
            Some(Token { kind: 't', pos })
            | Some(Token { kind: 'T', pos })
            | Some(Token { kind: '\\', pos }) => {
                let span = *pos;
                let mut ident = match peek_ident_no_interpolation(toks, false, span) {
                    Ok(s) => s,
                    Err(..) => return false,
                };
                ident.node.make_ascii_lowercase();
                let v = matches!(ident.node.to_ascii_lowercase().as_str(), "to" | "through");
                toks.reset_cursor();
                v
            }
            Some(..) | None => false,
        })?;

        let through = if self.scan_identifier("through")? {
            1
        } else if self.scan_identifier("to")? {
            0
        } else {
            return Err(("Expected \"to\" or \"through\".", self.span_before).into());
        };

        let from = match from_val.node {
            Value::Dimension(Some(n), ..) => match n.to_i32() {
                Some(std::i32::MAX) | Some(std::i32::MIN) | None => {
                    return Err((format!("{} is not an int.", n), from_val.span).into())
                }
                Some(v) => v,
            },
            Value::Dimension(None, ..) => return Err(("NaN is not an int.", from_val.span).into()),
            v => {
                return Err((
                    format!("{} is not a number.", v.inspect(from_val.span)?),
                    from_val.span,
                )
                    .into())
            }
        };

        let to_val = self.parse_value(true, &|_| false)?;
        let to = match to_val.node {
            Value::Dimension(Some(n), ..) => match n.to_i32() {
                Some(std::i32::MAX) | Some(std::i32::MIN) | None => {
                    return Err((format!("{} is not an int.", n), to_val.span).into())
                }
                Some(v) => v,
            },
            Value::Dimension(None, ..) => return Err(("NaN is not an int.", from_val.span).into()),
            v => {
                return Err((
                    format!("{} is not a number.", v.to_css_string(to_val.span)?),
                    to_val.span,
                )
                    .into())
            }
        };

        self.expect_char('{')?;

        let body = read_until_closing_curly_brace(self.toks)?;

        self.expect_char('}')?;

        let (mut x, mut y);
        // we can't use an inclusive range here
        #[allow(clippy::range_plus_one)]
        let iter: &mut dyn Iterator<Item = i32> = if from < to {
            x = from..(to + through);
            &mut x
        } else {
            y = ((to - through)..(from + 1)).skip(1).rev();
            &mut y
        };

        let mut stmts = Vec::new();

        self.scopes.enter_new_scope();

        for i in iter {
            self.scopes.insert_var_last(
                var.node,
                Value::Dimension(Some(Number::from(i)), Unit::None, true),
            );
            if self.flags.in_function() {
                let these_stmts = Parser {
                    toks: &mut body.clone().into_iter().peekmore(),
                    map: self.map,
                    path: self.path,
                    scopes: self.scopes,
                    global_scope: self.global_scope,
                    super_selectors: self.super_selectors,
                    span_before: self.span_before,
                    content: self.content,
                    flags: self.flags | ContextFlags::IN_CONTROL_FLOW,
                    at_root: self.at_root,
                    at_root_has_selector: self.at_root_has_selector,
                    extender: self.extender,
                    content_scopes: self.content_scopes,
                    options: self.options,
                    modules: self.modules,
                    module_config: self.module_config,
                }
                .parse_stmt()?;
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
                        global_scope: self.global_scope,
                        super_selectors: self.super_selectors,
                        span_before: self.span_before,
                        content: self.content,
                        flags: self.flags | ContextFlags::IN_CONTROL_FLOW,
                        at_root: self.at_root,
                        at_root_has_selector: self.at_root_has_selector,
                        extender: self.extender,
                        content_scopes: self.content_scopes,
                        options: self.options,
                        modules: self.modules,
                        module_config: self.module_config,
                    }
                    .parse_stmt()?,
                );
            }
        }

        self.scopes.exit_scope();

        Ok(stmts)
    }

    pub(super) fn parse_while(&mut self) -> SassResult<Vec<Stmt>> {
        // technically not necessary to eat whitespace here, but since we
        // operate on raw tokens rather than an AST, it potentially saves a lot of
        // time in re-parsing
        self.whitespace_or_comment();
        let cond = read_until_open_curly_brace(self.toks)?;

        if cond.is_empty() {
            return Err(("Expected expression.", self.span_before).into());
        }

        self.toks.next();

        let mut body = read_until_closing_curly_brace(self.toks)?;

        body.push(match self.toks.next() {
            Some(tok) => tok,
            None => return Err(("expected \"}\".", self.span_before).into()),
        });

        let mut stmts = Vec::new();
        let mut val = self.parse_value_from_vec(cond.clone(), true)?;
        self.scopes.enter_new_scope();
        while val.node.is_true() {
            if self.flags.in_function() {
                let these_stmts = Parser {
                    toks: &mut body.clone().into_iter().peekmore(),
                    map: self.map,
                    path: self.path,
                    scopes: self.scopes,
                    global_scope: self.global_scope,
                    super_selectors: self.super_selectors,
                    span_before: self.span_before,
                    content: self.content,
                    flags: self.flags | ContextFlags::IN_CONTROL_FLOW,
                    at_root: self.at_root,
                    at_root_has_selector: self.at_root_has_selector,
                    extender: self.extender,
                    content_scopes: self.content_scopes,
                    options: self.options,
                    modules: self.modules,
                    module_config: self.module_config,
                }
                .parse_stmt()?;
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
                        global_scope: self.global_scope,
                        super_selectors: self.super_selectors,
                        span_before: self.span_before,
                        content: self.content,
                        flags: self.flags | ContextFlags::IN_CONTROL_FLOW,
                        at_root: self.at_root,
                        at_root_has_selector: self.at_root_has_selector,
                        extender: self.extender,
                        content_scopes: self.content_scopes,
                        options: self.options,
                        modules: self.modules,
                        module_config: self.module_config,
                    }
                    .parse_stmt()?,
                );
            }
            val = self.parse_value_from_vec(cond.clone(), true)?;
        }
        self.scopes.exit_scope();

        Ok(stmts)
    }

    pub(super) fn parse_each(&mut self) -> SassResult<Vec<Stmt>> {
        let mut vars: Vec<Spanned<Identifier>> = Vec::new();

        self.whitespace_or_comment();
        loop {
            self.expect_char('$')?;

            vars.push(self.parse_identifier()?.map_node(|i| i.into()));

            self.whitespace_or_comment();
            if self
                .toks
                .peek()
                .ok_or(("expected \"$\".", vars[vars.len() - 1].span))?
                .kind
                == ','
            {
                self.toks.next();
                self.whitespace_or_comment();
            } else {
                break;
            }
        }
        let i = self.parse_identifier()?;
        if i.node.to_ascii_lowercase() != "in" {
            return Err(("Expected \"in\".", i.span).into());
        }
        self.whitespace_or_comment();
        let iter_val_toks = read_until_open_curly_brace(self.toks)?;
        let iter = self
            .parse_value_from_vec(iter_val_toks, true)?
            .node
            .as_list();
        self.toks.next();
        self.whitespace();
        let mut body = read_until_closing_curly_brace(self.toks)?;
        body.push(match self.toks.next() {
            Some(tok) => tok,
            None => return Err(("expected \"}\".", self.span_before).into()),
        });
        self.whitespace();

        let mut stmts = Vec::new();

        self.scopes.enter_new_scope();

        for row in iter {
            if vars.len() == 1 {
                self.scopes.insert_var_last(vars[0].node, row);
            } else {
                for (var, val) in vars.iter().zip(
                    row.as_list()
                        .into_iter()
                        .chain(std::iter::once(Value::Null).cycle()),
                ) {
                    self.scopes.insert_var_last(var.node, val);
                }
            }

            if self.flags.in_function() {
                let these_stmts = Parser {
                    toks: &mut body.clone().into_iter().peekmore(),
                    map: self.map,
                    path: self.path,
                    scopes: self.scopes,
                    global_scope: self.global_scope,
                    super_selectors: self.super_selectors,
                    span_before: self.span_before,
                    content: self.content,
                    flags: self.flags | ContextFlags::IN_CONTROL_FLOW,
                    at_root: self.at_root,
                    at_root_has_selector: self.at_root_has_selector,
                    extender: self.extender,
                    content_scopes: self.content_scopes,
                    options: self.options,
                    modules: self.modules,
                    module_config: self.module_config,
                }
                .parse_stmt()?;
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
                        global_scope: self.global_scope,
                        super_selectors: self.super_selectors,
                        span_before: self.span_before,
                        content: self.content,
                        flags: self.flags | ContextFlags::IN_CONTROL_FLOW,
                        at_root: self.at_root,
                        at_root_has_selector: self.at_root_has_selector,
                        extender: self.extender,
                        content_scopes: self.content_scopes,
                        options: self.options,
                        modules: self.modules,
                        module_config: self.module_config,
                    }
                    .parse_stmt()?,
                );
            }
        }

        self.scopes.exit_scope();

        Ok(stmts)
    }
}
