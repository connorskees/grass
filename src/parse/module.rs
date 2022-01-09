use std::convert::TryFrom;

use codemap::Spanned;

use crate::{
    atrule::AtRuleKind,
    builtin::modules::{
        declare_module_color, declare_module_list, declare_module_map, declare_module_math,
        declare_module_meta, declare_module_selector, declare_module_string, Module, ModuleConfig,
        Modules,
    },
    common::Identifier,
    error::SassResult,
    lexer::Lexer,
    parse::{common::Comment, Parser, Stmt, VariableValue},
    scope::Scope,
    Token,
};

impl<'a, 'b> Parser<'a, 'b> {
    fn parse_module_alias(&mut self) -> SassResult<Option<String>> {
        if !matches!(
            self.toks.peek(),
            Some(Token { kind: 'a', .. }) | Some(Token { kind: 'A', .. })
        ) {
            return Ok(None);
        }

        let mut ident = self.parse_identifier_no_interpolation(false)?;

        ident.node.make_ascii_lowercase();

        if ident.node != "as" {
            return Err(("expected \";\".", ident.span).into());
        }

        self.whitespace_or_comment();

        if self.consume_char_if_exists('*') {
            return Ok(Some('*'.to_string()));
        }

        let name = self.parse_identifier_no_interpolation(false)?;

        Ok(Some(name.node))
    }

    fn parse_module_config(&mut self) -> SassResult<ModuleConfig> {
        let mut config = ModuleConfig::default();

        if !matches!(
            self.toks.peek(),
            Some(Token { kind: 'w', .. }) | Some(Token { kind: 'W', .. })
        ) {
            return Ok(config);
        }

        let mut ident = self.parse_identifier_no_interpolation(false)?;

        ident.node.make_ascii_lowercase();
        if ident.node != "with" {
            return Err(("expected \";\".", ident.span).into());
        }

        self.whitespace_or_comment();

        self.span_before = ident.span;

        self.expect_char('(')?;

        loop {
            self.whitespace_or_comment();
            self.expect_char('$')?;

            let name = self.parse_identifier_no_interpolation(false)?;

            self.whitespace_or_comment();
            self.expect_char(':')?;
            self.whitespace_or_comment();

            let value = self.parse_value(false, &|parser| {
                matches!(
                    parser.toks.peek(),
                    Some(Token { kind: ',', .. }) | Some(Token { kind: ')', .. })
                )
            })?;

            config.insert(name.map_node(|n| n.into()), value)?;

            match self.toks.next() {
                Some(Token { kind: ',', .. }) => {
                    continue;
                }
                Some(Token { kind: ')', .. }) => {
                    break;
                }
                Some(..) | None => {
                    return Err(("expected \")\".", self.span_before).into());
                }
            }
        }

        Ok(config)
    }

    pub fn load_module(
        &mut self,
        name: &str,
        config: &mut ModuleConfig,
    ) -> SassResult<(Module, Vec<Stmt>)> {
        Ok(match name {
            "sass:color" => (declare_module_color(), Vec::new()),
            "sass:list" => (declare_module_list(), Vec::new()),
            "sass:map" => (declare_module_map(), Vec::new()),
            "sass:math" => (declare_module_math(), Vec::new()),
            "sass:meta" => (declare_module_meta(), Vec::new()),
            "sass:selector" => (declare_module_selector(), Vec::new()),
            "sass:string" => (declare_module_string(), Vec::new()),
            _ => {
                if let Some(import) = self.find_import(name.as_ref()) {
                    let mut global_scope = Scope::new();

                    let file = self.map.add_file(
                        name.to_owned(),
                        String::from_utf8(self.options.fs.read(&import)?)?,
                    );

                    let mut modules = Modules::default();

                    let stmts = Parser {
                        toks: &mut Lexer::new_from_file(&file),
                        map: self.map,
                        path: &import,
                        scopes: self.scopes,
                        global_scope: &mut global_scope,
                        super_selectors: self.super_selectors,
                        span_before: file.span.subspan(0, 0),
                        content: self.content,
                        flags: self.flags,
                        at_root: self.at_root,
                        at_root_has_selector: self.at_root_has_selector,
                        extender: self.extender,
                        content_scopes: self.content_scopes,
                        options: self.options,
                        modules: &mut modules,
                        module_config: config,
                    }
                    .parse()?;

                    if !config.is_empty() {
                        return Err((
                            "This variable was not declared with !default in the @used module.",
                            self.span_before,
                        )
                            .into());
                    }

                    (Module::new_from_scope(global_scope, modules, false), stmts)
                } else {
                    return Err(("Can't find stylesheet to import.", self.span_before).into());
                }
            }
        })
    }

    /// Returns any multiline comments that may have been found
    /// while loading modules
    pub(super) fn load_modules(&mut self) -> SassResult<Vec<Stmt>> {
        let mut comments = Vec::new();

        loop {
            self.whitespace();

            match self.toks.peek() {
                Some(Token { kind: '@', .. }) => {
                    let start = self.toks.cursor();

                    self.toks.next();

                    if let Some(Token { kind, .. }) = self.toks.peek() {
                        if !matches!(kind, 'u' | 'U' | '\\') {
                            self.toks.set_cursor(start);
                            break;
                        }
                    }

                    let ident = self.parse_identifier_no_interpolation(false)?;

                    if AtRuleKind::try_from(&ident)? != AtRuleKind::Use {
                        self.toks.set_cursor(start);
                        break;
                    }

                    self.whitespace_or_comment();

                    let quote = match self.toks.next() {
                        Some(Token { kind: q @ '"', .. }) | Some(Token { kind: q @ '\'', .. }) => q,
                        Some(..) | None => {
                            return Err(("Expected string.", self.span_before).into())
                        }
                    };

                    let Spanned { node: module, span } = self.parse_quoted_string(quote)?;
                    let module_name = module
                        .unquote()
                        .to_css_string(span, self.options.is_compressed())?;

                    self.whitespace_or_comment();

                    let module_alias = self.parse_module_alias()?;

                    self.whitespace_or_comment();

                    let mut config = self.parse_module_config()?;

                    self.whitespace_or_comment();
                    self.expect_char(';')?;

                    let (module, mut stmts) =
                        self.load_module(module_name.as_ref(), &mut config)?;

                    comments.append(&mut stmts);

                    // if the config isn't empty here, that means
                    // variables were passed to a builtin module
                    if !config.is_empty() {
                        return Err(("Built-in modules can't be configured.", span).into());
                    }

                    let module_name = match module_alias.as_deref() {
                        Some("*") => {
                            self.modules.merge(module.modules);
                            self.global_scope.merge_module_scope(module.scope);
                            continue;
                        }
                        Some(..) => module_alias.unwrap(),
                        None => match module_name.as_ref() {
                            "sass:color" => "color".to_owned(),
                            "sass:list" => "list".to_owned(),
                            "sass:map" => "map".to_owned(),
                            "sass:math" => "math".to_owned(),
                            "sass:meta" => "meta".to_owned(),
                            "sass:selector" => "selector".to_owned(),
                            "sass:string" => "string".to_owned(),
                            _ => module_name.into_owned(),
                        },
                    };

                    self.modules.insert(module_name.into(), module, span)?;
                }
                Some(Token { kind: '/', .. }) => {
                    self.toks.next();
                    match self.parse_comment()?.node {
                        Comment::Silent => continue,
                        Comment::Loud(s) => comments.push(Stmt::Comment(s)),
                    }
                }
                Some(Token { kind: '$', .. }) => self.parse_variable_declaration()?,
                Some(..) | None => break,
            }
        }

        self.toks.reset_cursor();

        Ok(comments)
    }

    pub(super) fn parse_module_variable_redeclaration(
        &mut self,
        module: Identifier,
    ) -> SassResult<()> {
        let variable = self
            .parse_identifier_no_interpolation(false)?
            .map_node(|n| n.into());

        self.whitespace_or_comment();
        self.expect_char(':')?;

        let VariableValue {
            var_value,
            global,
            default,
        } = self.parse_variable_value()?;

        if global {
            return Err((
                "!global isn't allowed for variables in other modules.",
                variable.span,
            )
                .into());
        }

        if default {
            return Ok(());
        }

        let value = var_value?;

        self.modules
            .get_mut(module, variable.span)?
            .update_var(variable, value.node)?;

        Ok(())
    }
}
