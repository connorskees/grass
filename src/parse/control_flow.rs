use codemap::Spanned;
use num_traits::cast::ToPrimitive;
use peekmore::PeekMore;

use crate::{
    common::Identifier,
    error::SassResult,
    parse::{ContextFlags, Parser, Stmt},
    unit::Unit,
    utils::{
        peek_ident_no_interpolation, read_until_closing_curly_brace, read_until_closing_quote,
        read_until_open_curly_brace,
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
                        } else {
                            return Parser {
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
                        }
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
        // todo: test for error here
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
        let mut from_toks = Vec::new();
        let mut through = 0;
        while let Some(tok) = self.toks.peek().cloned() {
            match tok.kind {
                't' | 'T' | '\\' => {
                    let ident = peek_ident_no_interpolation(self.toks, false, tok.pos)?;
                    match ident.node.to_ascii_lowercase().as_str() {
                        "through" => {
                            through = 1;
                            self.toks.truncate_iterator_to_cursor();
                            break;
                        }
                        "to" => {
                            self.toks.truncate_iterator_to_cursor();
                            break;
                        }
                        _ => {
                            from_toks.push(tok);
                            self.toks.next();
                            self.toks.reset_cursor();
                        }
                    }
                }
                '$' => {
                    from_toks.push(tok);
                    self.toks.next();
                    while let Some(tok) = self.toks.peek() {
                        if matches!(tok.kind, '0'..='9' | 'a'..='z' | 'A'..='Z' | '\\' | '-' | '_')
                        {
                            from_toks.push(self.toks.next().unwrap());
                        } else {
                            break;
                        }
                    }
                }
                '{' => {
                    return Err(("Expected \"to\" or \"through\".", tok.pos()).into());
                }
                '#' => {
                    from_toks.push(tok);
                    self.toks.next();
                    match self.toks.peek() {
                        Some(Token { kind: '{', .. }) => {
                            from_toks.push(self.toks.next().unwrap());
                            from_toks.append(&mut read_until_closing_curly_brace(self.toks)?);
                        }
                        Some(..) => {}
                        None => return Err(("expected \"{\".", self.span_before).into()),
                    }
                }
                q @ '\'' | q @ '"' => {
                    from_toks.push(tok);
                    self.toks.next();
                    from_toks.append(&mut read_until_closing_quote(self.toks, q)?);
                }
                _ => {
                    from_toks.push(tok);
                    self.toks.next();
                }
            }
        }
        self.whitespace_or_comment();
        let from_val = self.parse_value_from_vec(from_toks, true)?;
        let from = match from_val.node {
            Value::Dimension(Some(n), ..) => match n.to_integer().to_isize() {
                Some(v) => v,
                None => return Err((format!("{} is not a int.", n), from_val.span).into()),
            },
            Value::Dimension(None, ..) => todo!(),
            v => {
                return Err((
                    format!("{} is not an integer.", v.inspect(from_val.span)?),
                    from_val.span,
                )
                    .into())
            }
        };

        let to_val = self.parse_value(true, &|_| false)?;
        let to = match to_val.node {
            Value::Dimension(Some(n), ..) => match n.to_integer().to_isize() {
                Some(v) => v,
                None => return Err((format!("{} is not a int.", n), to_val.span).into()),
            },
            Value::Dimension(None, ..) => todo!(),
            v => {
                return Err((
                    format!("{} is not an integer.", v.to_css_string(to_val.span)?),
                    to_val.span,
                )
                    .into())
            }
        };

        self.expect_char('{')?;

        let body = read_until_closing_curly_brace(self.toks)?;
        self.toks.next();

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
