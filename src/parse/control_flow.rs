use codemap::Spanned;
use num_traits::cast::ToPrimitive;

use crate::{
    common::Identifier,
    error::SassResult,
    lexer::Lexer,
    parse::{ContextFlags, Parser, Stmt},
    unit::Unit,
    utils::{read_until_closing_curly_brace, read_until_open_curly_brace},
    value::{Number, Value},
    Token,
};

impl<'a, 'b> Parser<'a, 'b> {
    fn subparser_with_in_control_flow_flag<'c>(&'c mut self) -> Parser<'c, 'b> {
        Parser {
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
    }

    fn with_toks<'d>(self, toks: &'a mut Lexer<'d>) -> Parser<'a, 'd> {
        Parser {
            toks,
            map: self.map,
            path: self.path,
            scopes: self.scopes,
            global_scope: self.global_scope,
            super_selectors: self.super_selectors,
            span_before: self.span_before,
            content: self.content,
            flags: self.flags,
            at_root: self.at_root,
            at_root_has_selector: self.at_root_has_selector,
            extender: self.extender,
            content_scopes: self.content_scopes,
            options: self.options,
            modules: self.modules,
            module_config: self.module_config,
        }
    }

    pub(super) fn parse_if(&mut self) -> SassResult<Vec<Stmt>> {
        self.whitespace_or_comment();

        let mut found_true = false;
        let mut body = Vec::new();

        let init_cond = self.parse_value(true, &|_| false)?.node;

        self.expect_char('{')?;

        if self.toks.peek().is_none() {
            return Err(("expected \"}\".", self.span_before).into());
        }

        if init_cond.is_true() {
            found_true = true;
            self.scopes.enter_new_scope();
            body = self.subparser_with_in_control_flow_flag().parse_stmt()?;
            self.scopes.exit_scope();
        } else {
            self.throw_away_until_closing_curly_brace()?;
        }

        loop {
            self.whitespace_or_comment();

            let start = self.toks.cursor();

            if !self.consume_char_if_exists('@') || !self.scan_identifier("else", false) {
                self.toks.set_cursor(start);
                break;
            }

            self.whitespace_or_comment();
            if let Some(tok) = self.toks.peek() {
                match tok.kind {
                    'i' | 'I' | '\\' => {
                        self.span_before = tok.pos;
                        let mut ident = self.parse_identifier_no_interpolation(false)?;

                        ident.node.make_ascii_lowercase();

                        if ident.node != "if" {
                            return Err(("expected \"{\".", ident.span).into());
                        }

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
                            body = self.subparser_with_in_control_flow_flag().parse_stmt()?;
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
                        let tmp = self.subparser_with_in_control_flow_flag().parse_stmt();
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

        let from_val = self.parse_value(false, &|parser| match parser.toks.peek() {
            Some(Token { kind: 't', .. })
            | Some(Token { kind: 'T', .. })
            | Some(Token { kind: '\\', .. }) => {
                let start = parser.toks.cursor();

                let mut ident = match parser.parse_identifier_no_interpolation(false) {
                    Ok(s) => s,
                    Err(..) => return false,
                };

                ident.node.make_ascii_lowercase();

                let v = matches!(ident.node.to_ascii_lowercase().as_str(), "to" | "through");

                parser.toks.set_cursor(start);

                v
            }
            Some(..) | None => false,
        })?;

        let through = if self.scan_identifier("through", true) {
            1
        } else if self.scan_identifier("to", true) {
            0
        } else {
            return Err(("Expected \"to\" or \"through\".", self.span_before).into());
        };

        let from = match from_val.node {
            Value::Dimension(Some(n), ..) => match n.to_i32() {
                Some(std::i32::MAX) | Some(std::i32::MIN) | None => {
                    return Err((format!("{} is not an int.", n.inspect()), from_val.span).into())
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
                    return Err((format!("{} is not an int.", n.inspect()), to_val.span).into())
                }
                Some(v) => v,
            },
            Value::Dimension(None, ..) => return Err(("NaN is not an int.", from_val.span).into()),
            v => {
                return Err((
                    format!(
                        "{} is not a number.",
                        v.to_css_string(to_val.span, self.options.is_compressed())?
                    ),
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
            let mut these_stmts = self.subparser_with_in_control_flow_flag()
                .with_toks(&mut Lexer::new_ref(&body))
                .parse_stmt()?;
            if self.flags.in_function() {
                if !these_stmts.is_empty() {
                    return Ok(these_stmts);
                }
            } else {
                stmts.append(&mut these_stmts);
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
        let mut val = self.parse_value_from_vec(&cond, true)?;
        self.scopes.enter_new_scope();
        while val.node.is_true() {
            let mut these_stmts = self.subparser_with_in_control_flow_flag()
                .with_toks(&mut Lexer::new_ref(&body))
                .parse_stmt()?;
            if self.flags.in_function() {
                if !these_stmts.is_empty() {
                    return Ok(these_stmts);
                }
            } else {
                stmts.append(&mut these_stmts);
            }
            val = self.parse_value_from_vec(&cond, true)?;
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
            .parse_value_from_vec(&iter_val_toks, true)?
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

            let mut these_stmts = self.subparser_with_in_control_flow_flag()
                .with_toks(&mut Lexer::new_ref(&body))
                .parse_stmt()?;
            if self.flags.in_function() {
                if !these_stmts.is_empty() {
                    return Ok(these_stmts);
                }
            } else {
                stmts.append(&mut these_stmts);
            }
        }

        self.scopes.exit_scope();

        Ok(stmts)
    }
}
