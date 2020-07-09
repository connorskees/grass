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

        let init_cond = self.parse_value()?.node;

        // consume the open curly brace
        let span_before = match self.toks.next() {
            Some(Token { kind: '{', pos }) => pos,
            Some(..) | None => return Err(("expected \"{\".", self.span_before).into()),
        };

        if self.toks.peek().is_none() {
            return Err(("expected \"}\".", span_before).into());
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
                            let v = self.parse_value()?.node.is_true();
                            match self.toks.next() {
                                Some(Token { kind: '{', .. }) => {}
                                Some(..) | None => {
                                    return Err(("expected \"{\".", self.span_before).into())
                                }
                            }
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
        self.whitespace();
        let next = self
            .toks
            .next()
            .ok_or(("expected \"$\".", self.span_before))?;
        let var: Spanned<Identifier> = match next.kind {
            '$' => self
                .parse_identifier_no_interpolation(false)?
                .map_node(|i| i.into()),
            _ => return Err(("expected \"$\".", self.span_before).into()),
        };
        self.whitespace();
        self.span_before = match self.toks.peek() {
            Some(tok) => tok.pos,
            None => return Err(("Expected \"from\".", var.span).into()),
        };
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
        self.whitespace();
        let from_val = self.parse_value_from_vec(from_toks)?;
        let from = match from_val.node {
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

        let to_val = self.parse_value()?;
        let to = match to_val.node {
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

        // consume the open curly brace
        match self.toks.next() {
            Some(Token { kind: '{', pos }) => pos,
            Some(..) | None => return Err(("expected \"{\".", to_val.span).into()),
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

        self.scopes.enter_new_scope();

        for i in iter {
            self.scopes.insert_var(
                var.node,
                Spanned {
                    node: Value::Dimension(Number::from(i), Unit::None),
                    span: var.span,
                },
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
                        global_scope: self.global_scope,
                        super_selectors: self.super_selectors,
                        span_before: self.span_before,
                        content: self.content,
                        flags: self.flags | ContextFlags::IN_CONTROL_FLOW,
                        at_root: self.at_root,
                        at_root_has_selector: self.at_root_has_selector,
                        extender: self.extender,
                        content_scopes: self.content_scopes,
                    }
                    .parse()?,
                );
            }
        }

        self.scopes.exit_scope();

        Ok(stmts)
    }

    pub(super) fn parse_while(&mut self) -> SassResult<Vec<Stmt>> {
        self.whitespace();
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

        self.whitespace();

        let mut stmts = Vec::new();
        let mut val = self.parse_value_from_vec(cond.clone())?;
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
                        global_scope: self.global_scope,
                        super_selectors: self.super_selectors,
                        span_before: self.span_before,
                        content: self.content,
                        flags: self.flags | ContextFlags::IN_CONTROL_FLOW,
                        at_root: self.at_root,
                        at_root_has_selector: self.at_root_has_selector,
                        extender: self.extender,
                        content_scopes: self.content_scopes,
                    }
                    .parse()?,
                );
            }
            val = self.parse_value_from_vec(cond.clone())?;
        }
        self.scopes.exit_scope();

        Ok(stmts)
    }

    pub(super) fn parse_each(&mut self) -> SassResult<Vec<Stmt>> {
        self.whitespace();
        let mut vars: Vec<Spanned<Identifier>> = Vec::new();

        loop {
            let next = self
                .toks
                .next()
                .ok_or(("expected \"$\".", self.span_before))?;

            match next.kind {
                '$' => vars.push(self.parse_identifier()?.map_node(|i| i.into())),
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
        let iter = self.parse_value_from_vec(iter_val_toks)?.node.as_list();
        self.toks.next();
        self.whitespace();
        let mut body = read_until_closing_curly_brace(self.toks)?;
        body.push(match self.toks.next() {
            Some(tok) => tok,
            None => return Err(("expected \"}\".", self.span_before).into()),
        });
        self.whitespace();

        let mut stmts = Vec::new();

        for row in iter {
            if vars.len() == 1 {
                self.scopes.insert_var(
                    vars[0].node,
                    Spanned {
                        node: row,
                        span: vars[0].span,
                    },
                );
            } else {
                for (var, val) in vars.iter().zip(
                    row.as_list()
                        .into_iter()
                        .chain(std::iter::once(Value::Null).cycle()),
                ) {
                    self.scopes.insert_var(
                        var.node,
                        Spanned {
                            node: val,
                            span: var.span,
                        },
                    );
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
                        global_scope: self.global_scope,
                        super_selectors: self.super_selectors,
                        span_before: self.span_before,
                        content: self.content,
                        flags: self.flags | ContextFlags::IN_CONTROL_FLOW,
                        at_root: self.at_root,
                        at_root_has_selector: self.at_root_has_selector,
                        extender: self.extender,
                        content_scopes: self.content_scopes,
                    }
                    .parse()?,
                );
            }
        }

        Ok(stmts)
    }
}
