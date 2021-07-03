use codemap::Spanned;

use crate::{common::Identifier, error::SassResult, value::Value, Token};

use super::Parser;

#[derive(Debug)]
pub(crate) struct VariableValue {
    pub var_value: SassResult<Spanned<Value>>,
    pub global: bool,
    pub default: bool,
}

impl VariableValue {
    pub const fn new(var_value: SassResult<Spanned<Value>>, global: bool, default: bool) -> Self {
        Self {
            var_value,
            global,
            default,
        }
    }
}

impl<'a> Parser<'a> {
    pub(super) fn parse_variable_declaration(&mut self) -> SassResult<()> {
        let next = self.toks.next();
        assert!(matches!(next, Some(Token { kind: '$', .. })));
        let ident: Identifier = self.parse_identifier_no_interpolation(false)?.node.into();
        self.whitespace_or_comment();

        self.expect_char(':')?;

        let VariableValue {
            var_value,
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
                    var_value?.node
                }
            } else if self.at_root && self.flags.in_control_flow() {
                if self.global_scope.default_var_exists(ident) {
                    return Ok(());
                }

                var_value?.node
            } else if self.at_root {
                var_value?.node
            } else {
                if self.scopes.default_var_exists(ident) {
                    return Ok(());
                }

                var_value?.node
            };

            if self.at_root && self.global_scope.var_exists(ident) {
                if !self.global_scope.default_var_exists(ident) {
                    self.global_scope.insert_var(ident, value.clone());
                }
            } else if self.at_root
                && !self.flags.in_control_flow()
                && !self.global_scope.default_var_exists(ident)
            {
                self.global_scope.insert_var(ident, value.clone());
            }

            if global {
                self.global_scope.insert_var(ident, value.clone());
            }

            if self.at_root && !self.flags.in_control_flow() {
                return Ok(());
            }

            self.scopes.insert_var(ident, value);

            return Ok(());
        }

        let value = var_value?.node;

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
        } else if !(self.flags.in_control_flow() && global) {
            self.scopes.insert_var(ident, value);
        }
        Ok(())
    }

    pub(super) fn parse_variable_value(&mut self) -> SassResult<VariableValue> {
        let mut default = false;
        let mut global = false;

        let value = self.parse_value(true, &|toks| {
            if matches!(toks.peek(), Some(Token { kind: '!', .. })) {
                let is_important = matches!(
                    toks.peek_next(),
                    Some(Token { kind: 'i', .. })
                        | Some(Token { kind: 'I', .. })
                        | Some(Token { kind: '=', .. })
                );
                toks.reset_cursor();
                !is_important
            } else {
                false
            }
        });

        // todo: it should not be possible to declare the same flag more than once
        while self.consume_char_if_exists('!') {
            let flag = self.parse_identifier_no_interpolation(false)?;

            match flag.node.as_str() {
                "global" => {
                    self.toks.truncate_iterator_to_cursor();
                    global = true;
                }
                "default" => {
                    self.toks.truncate_iterator_to_cursor();
                    default = true;
                }
                _ => {
                    return Err(("Invalid flag name.", flag.span).into());
                }
            }

            self.whitespace_or_comment();
        }

        match self.toks.peek() {
            Some(Token { kind: ';', .. }) => {
                self.toks.next();
            }
            Some(Token { kind: '}', .. }) => {}
            Some(..) | None => {
                value?;
                self.expect_char(';')?;
                unreachable!()
            }
        }

        Ok(VariableValue::new(value, global, default))
    }
}
