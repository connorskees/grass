use crate::{
    common::Identifier,
    error::SassResult,
    utils::{peek_ident_no_interpolation, read_until_closing_paren, read_until_closing_quote},
    Token,
};

use super::Parser;

#[derive(Debug)]
pub(crate) struct VariableValue {
    pub val_toks: Vec<Token>,
    pub global: bool,
    pub default: bool,
}

impl VariableValue {
    pub const fn new(val_toks: Vec<Token>, global: bool, default: bool) -> Self {
        Self {
            val_toks,
            global,
            default,
        }
    }
}

impl<'a> Parser<'a> {
    pub(super) fn parse_variable_declaration(&mut self) -> SassResult<()> {
        assert!(matches!(self.toks.next(), Some(Token { kind: '$', .. })));
        let ident: Identifier = self.parse_identifier_no_interpolation(false)?.node.into();
        self.whitespace_or_comment();

        self.expect_char(':')?;

        let VariableValue {
            val_toks,
            global,
            default,
        } = self.parse_variable_value()?;

        if default {
            let config_val = self.module_config.get(ident).filter(|v| !v.is_null());

            let value = if (self.at_root && !self.flags.in_control_flow()) || global {
                if self.global_scope.default_var_exists(ident) {
                    return Ok(());
                } else if let Some(value) = config_val {
                    value
                } else {
                    self.parse_value_from_vec(val_toks, true)?.node
                }
            } else if self.at_root && self.flags.in_control_flow() {
                if self.global_scope.default_var_exists(ident) {
                    return Ok(());
                }

                self.parse_value_from_vec(val_toks, true)?.node
            } else if !self.at_root {
                if self.scopes.default_var_exists(ident) {
                    return Ok(());
                }

                self.parse_value_from_vec(val_toks, true)?.node
            } else {
                self.parse_value_from_vec(val_toks, true)?.node
            };

            if self.at_root && !self.flags.in_control_flow() {
                self.global_scope.insert_var(ident, value);
                return Ok(());
            }

            if global {
                self.global_scope.insert_var(ident, value.clone());
            }

            self.scopes.insert_var(ident, value);

            return Ok(());
        }

        let value = self.parse_value_from_vec(val_toks, true)?.node;

        if global {
            self.global_scope.insert_var(ident, value.clone());
        }

        if self.at_root {
            if self.flags.in_control_flow() {
                if self.global_scope.var_exists(ident) {
                    self.global_scope.insert_var(ident, value);
                } else {
                    self.scopes.insert_var(ident, value);
                }
            } else {
                self.global_scope.insert_var(ident, value);
            }
        } else {
            self.scopes.insert_var(ident, value);
        }
        Ok(())
    }

    pub(super) fn parse_variable_value(&mut self) -> SassResult<VariableValue> {
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
                    if let Some(tok) = self.toks.next() {
                        val_toks.push(tok);
                    }
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
                        Some(Token { kind: '/', .. }) => self.read_until_newline(),
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
                    match self.toks.peek_forward(1) {
                        Some(Token { kind: '=', .. }) => {
                            self.toks.reset_cursor();
                            val_toks.push(self.toks.next().unwrap());
                            continue;
                        }
                        Some(..) => {}
                        None => return Err(("Expected identifier.", pos).into()),
                    }
                    // todo: it should not be possible to declare the same flag more than once
                    let mut ident = peek_ident_no_interpolation(self.toks, false, pos)?;
                    ident.node.make_ascii_lowercase();
                    match ident.node.as_str() {
                        "global" => {
                            self.toks.truncate_iterator_to_cursor();
                            global = true;
                        }
                        "default" => {
                            self.toks.truncate_iterator_to_cursor();
                            default = true;
                        }
                        "important" => {
                            self.toks.reset_cursor();
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

        Ok(VariableValue::new(val_toks, global, default))
    }
}
