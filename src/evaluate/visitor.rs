use std::{
    borrow::{Borrow, Cow},
    cell::{Cell, Ref, RefCell},
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    ffi::OsStr,
    fmt, mem,
    path::{Path, PathBuf},
    sync::Arc,
};

use codemap::{Span, Spanned};
use indexmap::IndexSet;
use num_traits::ToPrimitive;

use crate::{
    ast::*,
    atrule::{
        keyframes::KeyframesRuleSet,
        media::{MediaQuery, MediaQueryMergeResult, MediaRule},
        mixin::Mixin,
        SupportsRule, UnknownAtRule,
    },
    builtin::{
        meta::IF_ARGUMENTS,
        modules::{
            declare_module_color, declare_module_list, declare_module_map, declare_module_math,
            declare_module_meta, declare_module_selector, declare_module_string, Module,
        },
        Builtin, GLOBAL_FUNCTIONS,
    },
    common::{unvendor, BinaryOp, Identifier, ListSeparator, QuoteKind, UnaryOp},
    error::{SassError, SassResult},
    interner::InternedString,
    lexer::Lexer,
    parse::{
        add, cmp, div, mul, rem, single_eq, sub, AtRootQueryParser, KeyframesSelectorParser,
        Parser, Stmt,
    },
    selector::{
        ComplexSelectorComponent, ExtendRule, ExtendedSelector, ExtensionStore, Selector,
        SelectorList, SelectorParser,
    },
    style::Style,
    token::Token,
    utils::{to_sentence, trim_ascii},
    value::{
        ArgList, CalculationArg, CalculationName, Number, SassCalculation, SassFunction, SassMap,
        SassNumber, UserDefinedFunction, Value,
    },
    ContextFlags,
};

use super::env::Environment;

#[derive(Debug, Clone)]
struct CssTree {
    // None is tombstone
    stmts: Vec<RefCell<Option<Stmt>>>,
    parent_to_child: BTreeMap<CssTreeIdx, Vec<CssTreeIdx>>,
    child_to_parent: BTreeMap<CssTreeIdx, CssTreeIdx>,
}

impl CssTree {
    const ROOT: CssTreeIdx = CssTreeIdx(0);

    pub fn new() -> Self {
        let mut tree = Self {
            stmts: Vec::new(),
            parent_to_child: BTreeMap::new(),
            child_to_parent: BTreeMap::new(),
        };

        tree.stmts.push(RefCell::new(None));

        tree
    }

    pub fn get(&self, idx: CssTreeIdx) -> Ref<Option<Stmt>> {
        self.stmts[idx.0].borrow()
    }

    pub fn finish(self) -> Vec<Stmt> {
        let mut idx = 1;

        while idx < self.stmts.len() - 1 {
            if self.stmts[idx].borrow().is_none() || !self.has_children(CssTreeIdx(idx)) {
                idx += 1;
                continue;
            }

            self.apply_children(CssTreeIdx(idx));

            idx += 1;
        }

        self.stmts
            .into_iter()
            .filter_map(RefCell::into_inner)
            .collect()
    }

    fn apply_children(&self, parent: CssTreeIdx) {
        for &child in &self.parent_to_child[&parent] {
            if self.has_children(child) {
                self.apply_children(child);
            }

            match self.stmts[child.0].borrow_mut().take() {
                Some(child) => self.add_child_to_parent(child, parent),
                None => continue,
            };
        }
    }

    fn has_children(&self, parent: CssTreeIdx) -> bool {
        self.parent_to_child.contains_key(&parent)
    }

    fn add_child_to_parent(&self, child: Stmt, parent_idx: CssTreeIdx) {
        let mut parent = self.stmts[parent_idx.0].borrow_mut().take();
        match &mut parent {
            Some(Stmt::RuleSet { body, .. }) => body.push(child),
            Some(
                Stmt::Style(..)
                | Stmt::Comment(..)
                // | Stmt::Return(..)
                | Stmt::Import(..)
                // | Stmt::AtRoot { .. },
            ) => unreachable!(),
            Some(Stmt::Media(media, ..)) => {
                media.body.push(child);
            }
            Some(Stmt::UnknownAtRule(at_rule)) => {
                at_rule.body.push(child);
            }
            Some(Stmt::Supports(supports)) => {
                supports.body.push(child);
            }
            Some(Stmt::Keyframes(keyframes)) => {
                keyframes.body.push(child);
            }
            Some(Stmt::KeyframesRuleSet(keyframes)) => {
                keyframes.body.push(child);
            }
            None => todo!(),
        }
        self.stmts[parent_idx.0]
            .borrow_mut()
            .replace(parent.unwrap());
    }

    fn add_child(&mut self, child: Stmt, parent_idx: CssTreeIdx) -> CssTreeIdx {
        let child_idx = self.add_stmt_inner(child);
        self.parent_to_child
            .entry(parent_idx)
            .or_default()
            .push(child_idx);
        self.child_to_parent.insert(child_idx, parent_idx);
        child_idx
    }

    pub fn add_stmt(&mut self, child: Stmt, parent: Option<CssTreeIdx>) -> CssTreeIdx {
        match parent {
            Some(parent) => self.add_child(child, parent),
            None => self.add_child(child, Self::ROOT),
        }
    }

    fn add_stmt_inner(&mut self, stmt: Stmt) -> CssTreeIdx {
        let idx = CssTreeIdx(self.stmts.len());
        self.stmts.push(RefCell::new(Some(stmt)));

        idx
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, PartialOrd, Ord)]
#[repr(transparent)]
struct CssTreeIdx(usize);

trait UserDefinedCallable {
    fn name(&self) -> Identifier;
    fn arguments(&self) -> &ArgumentDeclaration;
}

impl UserDefinedCallable for AstFunctionDecl {
    fn name(&self) -> Identifier {
        self.name.node
    }

    fn arguments(&self) -> &ArgumentDeclaration {
        &self.arguments
    }
}

impl UserDefinedCallable for AstMixin {
    fn name(&self) -> Identifier {
        self.name
    }

    fn arguments(&self) -> &ArgumentDeclaration {
        &self.args
    }
}

impl UserDefinedCallable for Arc<CallableContentBlock> {
    fn name(&self) -> Identifier {
        Identifier::from("@content")
    }

    fn arguments(&self) -> &ArgumentDeclaration {
        &self.content.args
    }
}

#[derive(Debug, Clone)]
pub(crate) struct CallableContentBlock {
    content: AstContentBlock,
    env: Environment,
    // scopes: Arc<RefCell<Scopes>>,
    // scope_idx: usize,
    // content_at_decl: Option<Arc<Self>>,
}

pub(crate) struct Visitor<'a> {
    pub declaration_name: Option<String>,
    pub flags: ContextFlags,
    pub parser: &'a mut Parser<'a, 'a>,
    pub env: Environment,
    pub style_rule_ignoring_at_root: Option<ExtendedSelector>,
    // avoid emitting duplicate warnings for the same span
    pub warnings_emitted: HashSet<Span>,
    pub media_queries: Option<Vec<MediaQuery>>,
    pub media_query_sources: Option<IndexSet<MediaQuery>>,
    pub extender: ExtensionStore,
    pub current_import_path: PathBuf,
    pub is_plain_css: bool,
    css_tree: CssTree,
    parent: Option<CssTreeIdx>,
    configuration: Arc<RefCell<Configuration>>,
}

impl<'a> Visitor<'a> {
    pub fn new(parser: &'a mut Parser<'a, 'a>) -> Self {
        let mut flags = ContextFlags::empty();
        flags.set(ContextFlags::IN_SEMI_GLOBAL_SCOPE, true);

        let extender = ExtensionStore::new(parser.span_before);

        let current_import_path = parser.path.to_path_buf();

        Self {
            declaration_name: None,
            parser,
            style_rule_ignoring_at_root: None,
            flags,
            warnings_emitted: HashSet::new(),
            media_queries: None,
            media_query_sources: None,
            env: Environment::new(),
            extender,
            css_tree: CssTree::new(),
            parent: None,
            current_import_path,
            configuration: Arc::new(RefCell::new(Configuration::empty())),
            is_plain_css: false,
        }
    }

    pub fn visit_stylesheet(&mut self, style_sheet: StyleSheet) -> SassResult<()> {
        let was_in_plain_css = self.is_plain_css;
        self.is_plain_css = style_sheet.is_plain_css;
        for stmt in style_sheet.body {
            let result = self.visit_stmt(stmt)?;
            assert!(result.is_none());
        }
        self.is_plain_css = was_in_plain_css;

        Ok(())
    }

    pub fn finish(self) -> SassResult<Vec<Stmt>> {
        Ok(self.css_tree.finish())
    }

    fn visit_return_rule(&mut self, ret: AstReturn) -> SassResult<Option<Value>> {
        let val = self.visit_expr(ret.val)?;

        Ok(Some(self.without_slash(val)))
    }

    // todo: we really don't have to return Option<Value> from all of these children
    //  - could save some time by not passing around size_of(Value) bytes
    pub fn visit_stmt(&mut self, stmt: AstStmt) -> SassResult<Option<Value>> {
        match stmt {
            AstStmt::RuleSet(ruleset) => self.visit_ruleset(ruleset),
            AstStmt::Style(style) => self.visit_style(style),
            AstStmt::SilentComment(..) => Ok(None),
            AstStmt::If(if_stmt) => self.visit_if_stmt(if_stmt),
            AstStmt::For(for_stmt) => self.visit_for_stmt(for_stmt),
            AstStmt::Return(ret) => self.visit_return_rule(ret),
            AstStmt::Each(each_stmt) => self.visit_each_stmt(each_stmt),
            AstStmt::Media(media_rule) => self.visit_media_rule(media_rule),
            AstStmt::Include(include_stmt) => self.visit_include_stmt(include_stmt),
            AstStmt::While(while_stmt) => self.visit_while_stmt(while_stmt),
            AstStmt::VariableDecl(decl) => self.visit_variable_decl(decl),
            AstStmt::LoudComment(comment) => self.visit_loud_comment(comment),
            AstStmt::ImportRule(import_rule) => self.visit_import_rule(import_rule),
            AstStmt::FunctionDecl(func) => {
                self.visit_function_decl(func);
                Ok(None)
            }
            AstStmt::Mixin(mixin) => {
                self.visit_mixin_decl(mixin);
                Ok(None)
            }
            AstStmt::ContentRule(content_rule) => self.visit_content_rule(content_rule),
            AstStmt::Warn(warn_rule) => {
                self.visit_warn_rule(warn_rule)?;
                Ok(None)
            }
            AstStmt::UnknownAtRule(unknown_at_rule) => self.visit_unknown_at_rule(unknown_at_rule),
            AstStmt::ErrorRule(error_rule) => Err(self.visit_error_rule(error_rule)?),
            AstStmt::Extend(extend_rule) => self.visit_extend_rule(extend_rule),
            AstStmt::AtRootRule(at_root_rule) => self.visit_at_root_rule(at_root_rule),
            AstStmt::Debug(debug_rule) => self.visit_debug_rule(debug_rule),
            AstStmt::Use(use_rule) => {
                self.visit_use_rule(use_rule)?;
                Ok(None)
            }
            AstStmt::Forward(forward_rule) => {
                self.visit_forward_rule(forward_rule)?;
                Ok(None)
            }
            AstStmt::Supports(supports_rule) => {
                self.visit_supports_rule(supports_rule)?;
                Ok(None)
            }
        }
    }

    fn visit_forward_rule(&mut self, forward_rule: AstForwardRule) -> SassResult<()> {
        let old_config = Arc::clone(&self.configuration);
        let adjusted_config =
            Configuration::through_forward(Arc::clone(&old_config), &forward_rule);

        if !forward_rule.configuration.is_empty() {
            let new_configuration =
                self.add_forward_configuration(Arc::clone(&adjusted_config), &forward_rule)?;

            self.load_module(
                forward_rule.url.as_path(),
                Some(Arc::clone(&new_configuration)),
                false,
                self.parser.span_before,
                |visitor, module| {
                    visitor.env.forward_module(module, forward_rule.clone())?;

                    Ok(())
                },
            )?;

            self.remove_used_configuration(
                adjusted_config,
                Arc::clone(&new_configuration),
                &forward_rule
                    .configuration
                    .iter()
                    .filter(|var| !var.is_guarded)
                    .map(|var| var.name.node)
                    .collect(),
            );

            // Remove all the variables that weren't configured by this particular
            // `@forward` before checking that the configuration is empty. Errors for
            // outer `with` clauses will be thrown once those clauses finish
            // executing.
            let configured_variables: HashSet<Identifier> = forward_rule
                .configuration
                .iter()
                .map(|var| var.name.node)
                .collect();

            for name in (*new_configuration).borrow().values.keys() {
                if !configured_variables.contains(&name) {
                    (*new_configuration).borrow_mut().remove(name);
                }
            }

            Self::assert_configuration_is_empty(new_configuration, false)?;
        } else {
            self.configuration = adjusted_config;
            let url = forward_rule.url.clone();
            self.load_module(
                url.as_path(),
                None,
                false,
                self.parser.span_before,
                move |visitor, module| {
                    visitor.env.forward_module(module, forward_rule.clone())?;

                    Ok(())
                },
            )?;
            self.configuration = old_config;
        }

        Ok(())
    }

    fn add_forward_configuration(
        &mut self,
        config: Arc<RefCell<Configuration>>,
        forward_rule: &AstForwardRule,
    ) -> SassResult<Arc<RefCell<Configuration>>> {
        //     var newValues = Map.of(configuration.values);
        // for (var variable in node.configuration) {
        //   if (variable.isGuarded) {
        //     var oldValue = configuration.remove(variable.name);
        //     if (oldValue != null && oldValue.value != sassNull) {
        //       newValues[variable.name] = oldValue;
        //       continue;
        //     }
        //   }

        //   var variableNodeWithSpan = _expressionNode(variable.expression);
        //   newValues[variable.name] = ConfiguredValue.explicit(
        //       _withoutSlash(
        //           await variable.expression.accept(this), variableNodeWithSpan),
        //       variable.span,
        //       variableNodeWithSpan);
        // }

        // if (configuration is ExplicitConfiguration || configuration.isEmpty) {
        //   return ExplicitConfiguration(newValues, node);
        // } else {
        //   return Configuration.implicit(newValues);
        // }
        todo!()
    }

    fn remove_used_configuration(
        &mut self,
        upstream: Arc<RefCell<Configuration>>,
        downstream: Arc<RefCell<Configuration>>,
        except: &HashSet<Identifier>,
    ) {
        //     for (var name in upstream.values.keys.toList()) {
        //   if (except.contains(name)) continue;
        //   if (!downstream.values.containsKey(name)) upstream.remove(name);
        // }
        todo!()
    }

    fn parenthesize_supports_condition(
        &mut self,
        condition: AstSupportsCondition,
        operator: Option<&str>,
    ) -> SassResult<String> {
        match &condition {
            AstSupportsCondition::Negation(..) => {
                Ok(format!("({})", self.visit_supports_condition(condition)?))
            }
            AstSupportsCondition::Operation {
                operator: operator2,
                ..
            } if operator2.is_none() || operator2.as_deref() != operator => {
                Ok(format!("({})", self.visit_supports_condition(condition)?))
            }
            _ => self.visit_supports_condition(condition),
        }
    }

    fn visit_supports_condition(&mut self, condition: AstSupportsCondition) -> SassResult<String> {
        match condition {
            AstSupportsCondition::Operation {
                left,
                operator,
                right,
            } => Ok(format!(
                "{} {} {}",
                self.parenthesize_supports_condition(*left, operator.as_deref())?,
                operator.as_ref().unwrap(),
                self.parenthesize_supports_condition(*right, operator.as_deref())?
            )),
            AstSupportsCondition::Negation(condition) => Ok(format!(
                "not {}",
                self.parenthesize_supports_condition(*condition, None)?
            )),
            AstSupportsCondition::Interpolation(expr) => {
                self.evaluate_to_css(expr, QuoteKind::None, self.parser.span_before)
            }
            AstSupportsCondition::Declaration { name, value } => {
                let old_in_supports_decl = self.flags.in_supports_declaration();
                self.flags.set(ContextFlags::IN_SUPPORTS_DECLARATION, true);

                let is_custom_property = match &name {
                    AstExpr::String(StringExpr(text, QuoteKind::None), ..) => {
                        text.initial_plain().starts_with("--")
                    }
                    _ => false,
                };

                let result = format!(
                    "({}:{}{})",
                    self.evaluate_to_css(name, QuoteKind::Quoted, self.parser.span_before)?,
                    if is_custom_property { "" } else { " " },
                    self.evaluate_to_css(value, QuoteKind::Quoted, self.parser.span_before)?,
                );

                self.flags
                    .set(ContextFlags::IN_SUPPORTS_DECLARATION, old_in_supports_decl);

                Ok(result)
            }
            AstSupportsCondition::Function { name, args } => Ok(format!(
                "{}({})",
                self.perform_interpolation(name, false)?,
                self.perform_interpolation(args, false)?
            )),
            AstSupportsCondition::Anything { contents } => Ok(format!(
                "({})",
                self.perform_interpolation(contents, false)?,
            )),
        }
    }

    fn visit_supports_rule(&mut self, supports_rule: AstSupportsRule) -> SassResult<()> {
        if self.declaration_name.is_some() {
            return Err((
                "Supports rules may not be used within nested declarations.",
                supports_rule.span,
            )
                .into());
        }

        let condition = self.visit_supports_condition(supports_rule.condition)?;

        let css_supports_rule = Stmt::Supports(Box::new(SupportsRule {
            params: condition,
            body: Vec::new(),
        }));

        let parent_idx = self.css_tree.add_stmt(css_supports_rule, self.parent);

        let children = supports_rule.children;

        self.with_parent::<SassResult<()>>(parent_idx, true, |visitor| {
            if !visitor.style_rule_exists() {
                for stmt in children {
                    let result = visitor.visit_stmt(stmt)?;
                    assert!(result.is_none());
                }
            } else {
                // If we're in a style rule, copy it into the supports rule so that
                // declarations immediately inside @supports have somewhere to go.
                //
                // For example, "a {@supports (a: b) {b: c}}" should produce "@supports
                // (a: b) {a {b: c}}".
                let selector = visitor.style_rule_ignoring_at_root.clone().unwrap();
                let ruleset = Stmt::RuleSet {
                    selector,
                    body: Vec::new(),
                };

                let parent_idx = visitor.css_tree.add_stmt(ruleset, visitor.parent);

                visitor.with_parent::<SassResult<()>>(parent_idx, false, |visitor| {
                    for stmt in children {
                        let result = visitor.visit_stmt(stmt)?;
                        assert!(result.is_none());
                    }

                    Ok(())
                })?;
            }

            Ok(())
        })?;

        Ok(())
    }

    fn execute(
        &mut self,
        stylesheet: StyleSheet,
        configuration: Option<Arc<RefCell<Configuration>>>,
        names_in_errors: bool,
    ) -> SassResult<Arc<RefCell<Module>>> {
        let env = Environment::new();
        let mut extension_store = ExtensionStore::new(self.parser.span_before);

        self.with_environment::<SassResult<()>>(env.new_closure(), |visitor| {
            // let old_importer = visitor._importer;
            // let old_stylesheet = visitor.__stylesheet;
            // let old_root = visitor.__root;
            let old_parent = visitor.parent;
            // let old_end_of_imports = visitor.__endOfImports;
            // let old_out_of_order_imports = visitor._outOfOrderImports;
            mem::swap(&mut visitor.extender, &mut extension_store);
            let old_style_rule = visitor.style_rule_ignoring_at_root.take();
            let old_media_queries = visitor.media_queries.take();
            let old_declaration_name = visitor.declaration_name.take();
            let old_in_unknown_at_rule = visitor.flags.in_unknown_at_rule();
            let old_at_root_excluding_style_rule = visitor.flags.at_root_excluding_style_rule();
            let old_in_keyframes = visitor.flags.in_keyframes();
            let old_configuration = if let Some(new_config) = configuration {
                Some(mem::replace(&mut visitor.configuration, new_config))
            } else {
                None
            };
            visitor.parent = None;
            visitor.flags.set(ContextFlags::IN_UNKNOWN_AT_RULE, false);
            visitor
                .flags
                .set(ContextFlags::AT_ROOT_EXCLUDING_STYLE_RULE, false);
            visitor.flags.set(ContextFlags::IN_KEYFRAMES, false);

            visitor.visit_stylesheet(stylesheet)?;

            // visitor.importer = old_importer;
            // visitor.stylesheet = old_stylesheet;
            // visitor.root = old_root;
            visitor.parent = old_parent;
            // visitor.end_of_imports = old_end_of_imports;
            // visitor.out_of_order_imports = old_out_of_order_imports;
            mem::swap(&mut visitor.extender, &mut extension_store);
            visitor.style_rule_ignoring_at_root = old_style_rule;
            visitor.media_queries = old_media_queries;
            visitor.declaration_name = old_declaration_name;
            visitor
                .flags
                .set(ContextFlags::IN_UNKNOWN_AT_RULE, old_in_unknown_at_rule);
            visitor.flags.set(
                ContextFlags::AT_ROOT_EXCLUDING_STYLE_RULE,
                old_at_root_excluding_style_rule,
            );
            visitor
                .flags
                .set(ContextFlags::IN_KEYFRAMES, old_in_keyframes);
            if let Some(old_config) = old_configuration {
                visitor.configuration = old_config;
            }

            Ok(())
        })?;

        let module = env.to_module(extension_store);

        // Ok(())
        Ok(module)
    }

    fn load_module(
        &mut self,
        url: &Path,
        configuration: Option<Arc<RefCell<Configuration>>>,
        names_in_errors: bool,
        span: Span,
        callback: impl Fn(&mut Self, Arc<RefCell<Module>>) -> SassResult<()>,
    ) -> SassResult<()> {
        let builtin = match url.to_string_lossy().as_ref() {
            "sass:color" => Some(declare_module_color()),
            "sass:list" => Some(declare_module_list()),
            "sass:map" => Some(declare_module_map()),
            "sass:math" => Some(declare_module_math()),
            "sass:meta" => Some(declare_module_meta()),
            "sass:selector" => Some(declare_module_selector()),
            "sass:string" => Some(declare_module_string()),
            _ => None,
        };

        if let Some(builtin) = builtin {
            // todo: lots of ugly unwraps here
            if configuration.is_some()
                && !(**configuration.as_ref().unwrap()).borrow().is_implicit()
            {
                let msg = if names_in_errors {
                    format!(
                        "Built-in module {} can't be configured.",
                        url.to_string_lossy()
                    )
                } else {
                    "Built-in modules can't be configured.".to_owned()
                };

                return Err((
                    msg,
                    (**configuration.as_ref().unwrap()).borrow().span.unwrap(),
                )
                    .into());
            }

            callback(self, Arc::new(RefCell::new(builtin)))?;
            return Ok(());
        }

        // todo: decide on naming convention for style_sheet vs stylesheet
        let stylesheet = self.load_style_sheet(url.to_string_lossy().as_ref(), false, span)?;

        let module = self.execute(stylesheet, configuration, names_in_errors)?;

        callback(self, module)?;

        Ok(())
    }

    fn visit_use_rule(&mut self, use_rule: AstUseRule) -> SassResult<()> {
        let mut configuration = Arc::new(RefCell::new(Configuration::empty()));

        if !use_rule.configuration.is_empty() {
            let mut values = BTreeMap::new();

            for var in use_rule.configuration {
                let value = self.visit_expr(var.expr.node)?;
                let value = self.without_slash(value);
                values.insert(
                    var.name.node,
                    ConfiguredValue::explicit(value, var.name.span.merge(var.expr.span)),
                );
            }

            configuration = Arc::new(RefCell::new(Configuration::explicit(values, use_rule.span)));
        }

        let span = use_rule.span;

        let namespace = use_rule
            .namespace
            .as_ref()
            .map(|s| Identifier::from(s.trim_start_matches("sass:")));

        self.load_module(
            &use_rule.url,
            Some(Arc::clone(&configuration)),
            false,
            span,
            |visitor, module| {
                visitor.env.add_module(namespace, module, span)?;

                Ok(())
            },
        )?;

        Self::assert_configuration_is_empty(configuration, false)?;

        Ok(())
    }

    fn assert_configuration_is_empty(
        config: Arc<RefCell<Configuration>>,
        name_in_error: bool,
    ) -> SassResult<()> {
        let config = (*config).borrow();
        // By definition, implicit configurations are allowed to only use a subset
        // of their values.
        if config.is_empty() || config.is_implicit() {
            return Ok(());
        }

        let Spanned { node: name, span } = config.first().unwrap();

        let msg = if name_in_error {
            format!("${name} was not declared with !default in the @used module.")
        } else {
            "This variable was not declared with !default in the @used module.".to_owned()
        };

        Err((msg, span).into())
    }

    fn visit_import_rule(&mut self, import_rule: AstImportRule) -> SassResult<Option<Value>> {
        for import in import_rule.imports {
            match import {
                AstImport::Sass(dynamic_import) => {
                    self.visit_dynamic_import_rule(dynamic_import)?;
                }
                AstImport::Plain(static_import) => self.visit_static_import_rule(static_import)?,
            }
        }

        Ok(None)
    }

    /// Searches the current directory of the file then searches in `load_paths` directories
    /// if the import has not yet been found.
    ///
    /// <https://sass-lang.com/documentation/at-rules/import#finding-the-file>
    /// <https://sass-lang.com/documentation/at-rules/import#load-paths>
    fn find_import(&self, path: &Path) -> Option<PathBuf> {
        let path_buf = if path.is_absolute() {
            // todo: test for absolute path imports
            path.into()
        } else {
            self.current_import_path
                .parent()
                .unwrap_or_else(|| Path::new(""))
                .join(path)
        };

        let name = path_buf.file_name().unwrap_or_else(|| OsStr::new(".."));

        macro_rules! try_path {
            ($name:expr) => {
                let name = $name;
                if self.parser.options.fs.is_file(&name) {
                    return Some(name);
                }
            };
        }

        try_path!(path_buf.with_file_name(name).with_extension("scss"));
        try_path!(path_buf
            .with_file_name(format!("_{}", name.to_str().unwrap()))
            .with_extension("scss"));
        try_path!(path_buf.clone());
        try_path!(path.with_file_name(name).with_extension("css"));
        try_path!(path_buf.join("index.scss"));
        try_path!(path_buf.join("_index.scss"));

        for path in &self.parser.options.load_paths {
            if self.parser.options.fs.is_dir(path) {
                try_path!(path.join(name).with_extension("scss"));
                try_path!(path.with_file_name(name).with_extension("css"));
                try_path!(path
                    .join(format!("_{}", name.to_str().unwrap()))
                    .with_extension("scss"));
                try_path!(path.join("index.scss"));
                try_path!(path.join("_index.scss"));
            } else {
                try_path!(path.to_path_buf());
                try_path!(path.with_file_name(name).with_extension("scss"));
                try_path!(path.with_file_name(name).with_extension("css"));
                try_path!(path
                    .with_file_name(format!("_{}", name.to_str().unwrap()))
                    .with_extension("scss"));
                try_path!(path.join("index.scss"));
                try_path!(path.join("_index.scss"));
            }
        }

        None
    }

    fn import_like_node(
        &mut self,
        url: &str,
        for_import: bool,
        span: Span,
    ) -> SassResult<StyleSheet> {
        if let Some(name) = self.find_import(url.as_ref()) {
            let file = self.parser.map.add_file(
                name.to_string_lossy().into(),
                String::from_utf8(self.parser.options.fs.read(&name)?)?,
            );

            let mut old_import_path = name.clone();
            mem::swap(&mut self.current_import_path, &mut old_import_path);

            let old_is_use_allowed = self.flags.is_use_allowed();
            self.flags.set(ContextFlags::IS_USE_ALLOWED, true);

            let style_sheet = Parser {
                toks: &mut Lexer::new_from_file(&file),
                map: self.parser.map,
                is_plain_css: name.extension() == Some(OsStr::new("css")),
                path: &name,
                span_before: file.span.subspan(0, 0),
                flags: self.flags,
                options: self.parser.options,
            }
            .__parse()?;

            self.flags
                .set(ContextFlags::IS_USE_ALLOWED, old_is_use_allowed);

            mem::swap(&mut self.current_import_path, &mut old_import_path);
            return Ok(style_sheet);
        }

        Err(("Can't find stylesheet to import.", span).into())
        // let path = self.find_import(url.as_ref());
        //      var result = _nodeImporter!.loadRelative(originalUrl, previous, forImport);

        // bool isDependency;
        // if (result != null) {
        //   isDependency = _inDependency;
        // } else {
        //   result = await _nodeImporter!.loadAsync(originalUrl, previous, forImport);
        //   if (result == null) return null;
        //   isDependency = true;
        // }

        // var contents = result.item1;
        // var url = result.item2;

        // return _LoadedStylesheet(
        //     Stylesheet.parse(contents,
        //         url.startsWith('file') ? Syntax.forPath(url) : Syntax.scss,
        //         url: url,
        //         logger: _quietDeps && isDependency ? Logger.quiet : _logger),
        //     isDependency: isDependency);
    }

    fn load_style_sheet(
        &mut self,
        url: &str,
        // default=false
        for_import: bool,
        span: Span,
    ) -> SassResult<StyleSheet> {
        // if let Some(result) = self.import_like_node(url, for_import)? {
        //     return Ok(result);
        // }
        self.import_like_node(url, for_import, span)
        //         var result = await _importLikeNode(
        //     url, baseUrl ?? _stylesheet.span.sourceUrl, forImport);
        // if (result != null) {
        //   result.stylesheet.span.sourceUrl.andThen(_loadedUrls.add);
        //   return result;
        // }

        //     try {
        //   assert(_importSpan == null);
        //   _importSpan = span;

        //   var importCache = _importCache;
        //   if (importCache != null) {
        //     baseUrl ??= _stylesheet.span.sourceUrl;
        //     var tuple = await importCache.canonicalize(Uri.parse(url),
        //         baseImporter: _importer, baseUrl: baseUrl, forImport: forImport);

        //     if (tuple != null) {
        //       var isDependency = _inDependency || tuple.item1 != _importer;
        //       var stylesheet = await importCache.importCanonical(
        //           tuple.item1, tuple.item2,
        //           originalUrl: tuple.item3, quiet: _quietDeps && isDependency);
        //       if (stylesheet != null) {
        //         _loadedUrls.add(tuple.item2);
        //         return _LoadedStylesheet(stylesheet,
        //             importer: tuple.item1, isDependency: isDependency);
        //       }
        //     }
        //   } else {
        //     var result = await _importLikeNode(
        //         url, baseUrl ?? _stylesheet.span.sourceUrl, forImport);
        //     if (result != null) {
        //       result.stylesheet.span.sourceUrl.andThen(_loadedUrls.add);
        //       return result;
        //     }
        //   }
    }

    // todo: import cache
    fn visit_dynamic_import_rule(&mut self, dynamic_import: AstSassImport) -> SassResult<()> {
        let stylesheet = self.load_style_sheet(&dynamic_import.url, true, dynamic_import.span)?;

        //     return _withStackFrame("@import", import, () async {
        //   var result =
        //       await _loadStylesheet(import.urlString, import.span, forImport: true);
        //   var stylesheet = result.stylesheet;

        //   var url = stylesheet.span.sourceUrl;
        //   if (url != null) {
        //     if (_activeModules.containsKey(url)) {
        //       throw _activeModules[url].andThen((previousLoad) =>
        //               _multiSpanException("This file is already being loaded.",
        //                   "new load", {previousLoad.span: "original load"})) ??
        //           _exception("This file is already being loaded.");
        //     }
        //     _activeModules[url] = import;
        //   }

        // If the imported stylesheet doesn't use any modules, we can inject its
        // CSS directly into the current stylesheet. If it does use modules, we
        // need to put its CSS into an intermediate [ModifiableCssStylesheet] so
        // that we can hermetically resolve `@extend`s before injecting it.
        if stylesheet.uses.is_empty() && stylesheet.forwards.is_empty() {
            self.visit_stylesheet(stylesheet)?;
            return Ok(());
        }
        //   if (stylesheet.uses.isEmpty && stylesheet.forwards.isEmpty) {
        //     var oldImporter = _importer;
        //     var oldStylesheet = _stylesheet;
        //     var oldInDependency = _inDependency;
        //     _importer = result.importer;
        //     _stylesheet = stylesheet;
        //     _inDependency = result.isDependency;
        //     await visitStylesheet(stylesheet);
        //     _importer = oldImporter;
        //     _stylesheet = oldStylesheet;
        //     _inDependency = oldInDependency;
        //     _activeModules.remove(url);
        //     return;
        //   }

        //   // If only built-in modules are loaded, we still need a separate
        //   // environment to ensure their namespaces aren't exposed in the outer
        //   // environment, but we don't need to worry about `@extend`s, so we can
        //   // add styles directly to the existing stylesheet instead of creating a
        //   // new one.
        //   var loadsUserDefinedModules =
        //       stylesheet.uses.any((rule) => rule.url.scheme != 'sass') ||
        //           stylesheet.forwards.any((rule) => rule.url.scheme != 'sass');

        //   late List<ModifiableCssNode> children;
        //   var environment = _environment.forImport();
        //   await _withEnvironment(environment, () async {
        //     var oldImporter = _importer;
        //     var oldStylesheet = _stylesheet;
        //     var oldRoot = _root;
        //     var oldParent = _parent;
        //     var oldEndOfImports = _endOfImports;
        //     var oldOutOfOrderImports = _outOfOrderImports;
        //     var oldConfiguration = _configuration;
        //     var oldInDependency = _inDependency;
        //     _importer = result.importer;
        //     _stylesheet = stylesheet;
        //     if (loadsUserDefinedModules) {
        //       _root = ModifiableCssStylesheet(stylesheet.span);
        //       _parent = _root;
        //       _endOfImports = 0;
        //       _outOfOrderImports = null;
        //     }
        //     _inDependency = result.isDependency;

        //     // This configuration is only used if it passes through a `@forward`
        //     // rule, so we avoid creating unnecessary ones for performance reasons.
        //     if (stylesheet.forwards.isNotEmpty) {
        //       _configuration = environment.toImplicitConfiguration();
        //     }

        //     await visitStylesheet(stylesheet);
        //     children = loadsUserDefinedModules ? _addOutOfOrderImports() : [];

        //     _importer = oldImporter;
        //     _stylesheet = oldStylesheet;
        //     if (loadsUserDefinedModules) {
        //       _root = oldRoot;
        //       _parent = oldParent;
        //       _endOfImports = oldEndOfImports;
        //       _outOfOrderImports = oldOutOfOrderImports;
        //     }
        //     _configuration = oldConfiguration;
        //     _inDependency = oldInDependency;
        //   });

        //   // Create a dummy module with empty CSS and no extensions to make forwarded
        //   // members available in the current import context and to combine all the
        //   // CSS from modules used by [stylesheet].
        //   var module = environment.toDummyModule();
        //   _environment.importForwards(module);
        //   if (loadsUserDefinedModules) {
        //     if (module.transitivelyContainsCss) {
        //       // If any transitively used module contains extensions, we need to
        //       // clone all modules' CSS. Otherwise, it's possible that they'll be
        //       // used or imported from another location that shouldn't have the same
        //       // extensions applied.
        //       await _combineCss(module,
        //               clone: module.transitivelyContainsExtensions)
        //           .accept(this);
        //     }

        //     var visitor = _ImportedCssVisitor(this);
        //     for (var child in children) {
        //       child.accept(visitor);
        //     }
        //   }

        //   _activeModules.remove(url);
        // });
        todo!()
    }

    fn visit_static_import_rule(&mut self, static_import: AstPlainCssImport) -> SassResult<()> {
        // NOTE: this logic is largely duplicated in [visitCssImport]. Most changes
        // here should be mirrored there.

        let import = self.interpolation_to_value(static_import.url, false, false)?;

        if static_import.modifiers.is_some() {
            todo!()
        }

        let node = Stmt::Import(import);

        // if self.parent != Some(CssTree::ROOT) {
        self.css_tree.add_stmt(node, self.parent);
        // } else {
        //     self.css_tree.add_child(node, Some(CssTree::ROOT))
        // }
        // } else if self.end_of_imports

        // var node = ModifiableCssImport(
        //     await _interpolationToValue(import.url), import.span,
        //     modifiers: await import.modifiers
        //         .andThen<Future<CssValue<String>>?>(_interpolationToValue));

        // if (_parent != _root) {
        //   _parent.addChild(node);
        // } else if (_endOfImports == _root.children.length) {
        //   _root.addChild(node);
        //   _endOfImports++;
        // } else {
        //   (_outOfOrderImports ??= []).add(node);
        // }
        // todo!()
        Ok(())
    }

    fn visit_debug_rule(&mut self, debug_rule: AstDebugRule) -> SassResult<Option<Value>> {
        if self.parser.options.quiet {
            return Ok(None);
        }

        let message = self.visit_expr(debug_rule.value)?;

        let loc = self.parser.map.look_up_span(debug_rule.span);
        eprintln!(
            "{}:{} DEBUG: {}",
            loc.file.name(),
            loc.begin.line + 1,
            message.inspect(debug_rule.span)?
        );

        Ok(None)
    }

    fn visit_content_rule(&mut self, content_rule: AstContentRule) -> SassResult<Option<Value>> {
        let span = content_rule.args.span;
        if let Some(content) = &self.env.content {
            self.run_user_defined_callable(
                MaybeEvaledArguments::Invocation(content_rule.args),
                Arc::clone(content),
                content.env.clone(),
                span,
                |content, visitor| {
                    for stmt in content.content.body.clone() {
                        let result = visitor.visit_stmt(stmt)?;
                        assert!(result.is_none());
                    }

                    Ok(())
                },
            )?;
        }

        Ok(None)
    }

    fn trim_included(&self, nodes: &[CssTreeIdx]) -> CssTreeIdx {
        if nodes.is_empty() {
            return CssTree::ROOT;
        }

        let mut parent = self.parent;

        let mut innermost_contiguous: Option<usize> = None;

        for i in 0..nodes.len() {
            while parent != nodes.get(i).copied() {
                innermost_contiguous = None;

                let grandparent = self.css_tree.child_to_parent.get(&parent.unwrap()).copied();
                if grandparent.is_none() {
                    todo!("Expected ${{nodes[i]}} to be an ancestor of $this.")
                }
                parent = grandparent;
            }
            innermost_contiguous = innermost_contiguous.or(Some(i));

            let grandparent = self.css_tree.child_to_parent.get(&parent.unwrap()).copied();
            if grandparent.is_none() {
                todo!("Expected ${{nodes[i]}} to be an ancestor of $this.")
            }
            parent = grandparent;
        }

        if parent != Some(CssTree::ROOT) {
            return CssTree::ROOT;
        }

        let root = nodes[innermost_contiguous.unwrap()];

        root
        // todo!()
        //       if (nodes.isEmpty) return _root;

        // var parent = _parent;
        // int? innermostContiguous;
        // for (var i = 0; i < nodes.length; i++) {
        //   while (parent != nodes[i]) {
        //     innermostContiguous = null;

        //     var grandparent = parent.parent;
        //     if (grandparent == null) {
        //       throw ArgumentError(
        //           "Expected ${nodes[i]} to be an ancestor of $this.");
        //     }

        //     parent = grandparent;
        //   }
        //   innermostContiguous ??= i;

        //   var grandparent = parent.parent;
        //   if (grandparent == null) {
        //     throw ArgumentError("Expected ${nodes[i]} to be an ancestor of $this.");
        //   }
        //   parent = grandparent;
        // }

        // if (parent != _root) return _root;
        // var root = nodes[innermostContiguous!];
        // nodes.removeRange(innermostContiguous, nodes.length);
        // return root;
    }

    fn visit_at_root_rule(&mut self, mut at_root_rule: AstAtRootRule) -> SassResult<Option<Value>> {
        let query = match at_root_rule.query.clone() {
            Some(val) => {
                let resolved = self.perform_interpolation(val, true)?;

                let mut query_toks = Lexer::new(
                    resolved
                        .chars()
                        .map(|x| Token::new(self.parser.span_before, x))
                        .collect(),
                );

                AtRootQueryParser::new(&mut Parser {
                    toks: &mut query_toks,
                    map: self.parser.map,
                    path: self.parser.path,
                    is_plain_css: false,
                    span_before: self.parser.span_before,
                    flags: self.parser.flags,
                    options: self.parser.options,
                })
                .parse()?
            }
            None => AtRootQuery::default(),
        };

        let mut current_parent_idx = self.parent;

        let mut included = Vec::new();

        while let Some(parent_idx) = current_parent_idx {
            let parent = self.css_tree.stmts[parent_idx.0].borrow();
            let grandparent_idx = match &*parent {
                Some(parent) => {
                    if !query.excludes(parent) {
                        included.push(parent_idx);
                    }
                    self.css_tree.child_to_parent.get(&parent_idx).copied()
                }
                None => break,
            };

            current_parent_idx = grandparent_idx;
        }

        let root = self.trim_included(&included);

        // If we didn't exclude any rules, we don't need to use the copies we might
        // have created.
        if Some(root) == self.parent {
            self.with_scope::<SassResult<()>>(false, true, |visitor| {
                for stmt in at_root_rule.children {
                    let result = visitor.visit_stmt(stmt)?;
                    assert!(result.is_none());
                }

                Ok(())
            })?;
            return Ok(None);
        }

        let mut inner_copy = self.css_tree.get(root).clone();
        if !included.is_empty() {
            // inner_copy = self.css_tree.get(included[0]);
            // let outer_copy = inner_copy;
            // for node in &included[1..] {
            //     // let copy =
            // }

            //   innerCopy = included.first.copyWithoutChildren();
            //   var outerCopy = innerCopy;
            //   for (var node in included.skip(1)) {
            //     var copy = node.copyWithoutChildren();
            //     copy.addChild(outerCopy);
            //     outerCopy = copy;
            //   }

            //   root.addChild(outerCopy);
        }

        let body = mem::take(&mut at_root_rule.children);

        self.with_scope_for_at_root::<SassResult<()>>(
            &at_root_rule,
            inner_copy,
            &query,
            &included,
            |visitor| {
                for stmt in body {
                    let result = visitor.visit_stmt(stmt)?;
                    assert!(result.is_none());
                }

                Ok(())
            },
        )?;

        Ok(None)
    }

    fn with_scope_for_at_root<T>(
        &mut self,
        at_root_rule: &AstAtRootRule,
        new_parent: Option<Stmt>,
        query: &AtRootQuery,
        included: &[CssTreeIdx],
        callback: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let new_parent_idx = new_parent.map(|p| self.css_tree.add_stmt(p, None));

        let old_parent = self.parent;
        self.parent = new_parent_idx;

        let old_at_root_excluding_style_rule = self.flags.at_root_excluding_style_rule();

        if query.excludes_style_rules() {
            self.flags
                .set(ContextFlags::AT_ROOT_EXCLUDING_STYLE_RULE, true);
        }

        if self.media_queries.is_some() && query.excludes_name("media") {
            // _withMediaQueries(null, null, () => innerScope(callback));
            todo!()
        }

        if self.flags.in_keyframes() && query.excludes_name("keyframes") {
            //     var wasInKeyframes = _inKeyframes;
            // _inKeyframes = false;
            // await innerScope(callback);
            // _inKeyframes = wasInKeyframes;
            todo!()
        }

        // if self.flags.in_unknown_at_rule() && !included.iter().any(|parent| parent is CssAtRule)

        let res = callback(self);

        self.parent = old_parent;

        self.flags.set(
            ContextFlags::AT_ROOT_EXCLUDING_STYLE_RULE,
            old_at_root_excluding_style_rule,
        );

        res
    }

    fn visit_function_decl(&mut self, fn_decl: AstFunctionDecl) {
        let name = fn_decl.name.node;
        // todo: independency

        let func = SassFunction::UserDefined(UserDefinedFunction {
            function: Box::new(fn_decl),
            name,
            env: self.env.new_closure(),
        });

        self.env.insert_fn(func);
    }

    pub fn parse_selector_from_string(&mut self, selector_text: &str) -> SassResult<SelectorList> {
        let mut sel_toks = Lexer::new(
            selector_text
                .chars()
                .map(|x| Token::new(self.parser.span_before, x))
                .collect(),
        );

        SelectorParser::new(
            &mut Parser {
                toks: &mut sel_toks,
                map: self.parser.map,
                path: self.parser.path,
                is_plain_css: self.is_plain_css,
                span_before: self.parser.span_before,
                flags: self.parser.flags,
                options: self.parser.options,
            },
            !self.is_plain_css,
            !self.is_plain_css,
            self.parser.span_before,
        )
        .parse()
    }

    fn visit_extend_rule(&mut self, extend_rule: AstExtendRule) -> SassResult<Option<Value>> {
        if self.style_rule_ignoring_at_root.is_none() || self.declaration_name.is_some() {
            return Err((
                "@extend may only be used within style rules.",
                extend_rule.span,
            )
                .into());
        }

        let super_selector = self.style_rule_ignoring_at_root.clone().unwrap();

        let target_text = self.interpolation_to_value(extend_rule.value, false, true)?;

        let list = self.parse_selector_from_string(&target_text)?;

        let extend_rule = ExtendRule {
            selector: Selector(list.clone()),
            is_optional: extend_rule.is_optional,
            span: extend_rule.span,
        };

        for complex in list.components {
            if complex.components.len() != 1 || !complex.components.first().unwrap().is_compound() {
                // If the selector was a compound selector but not a simple
                // selector, emit a more explicit error.
                return Err((
                    "complex selectors may not be extended.",
                    self.parser.span_before,
                )
                    .into());
            }

            let compound = match complex.components.first() {
                Some(ComplexSelectorComponent::Compound(c)) => c,
                Some(..) | None => todo!(),
            };
            if compound.components.len() != 1 {
                return Err((
                    format!(
                        "compound selectors may no longer be extended.\nConsider `@extend {}` instead.\nSee http://bit.ly/ExtendCompound for details.\n",
                        compound.components.iter().map(ToString::to_string).collect::<Vec<String>>().join(", ")
                    )
                , self.parser.span_before).into());
            }

            self.extender.add_extension(
                super_selector.clone().into_selector().0,
                compound.components.first().unwrap(),
                &extend_rule,
                &self.media_queries,
                self.parser.span_before,
            );
        }

        Ok(None)
    }

    fn visit_error_rule(&mut self, error_rule: AstErrorRule) -> SassResult<Box<SassError>> {
        let value = self
            .visit_expr(error_rule.value)?
            .inspect(error_rule.span)?
            .into_owned();

        Ok((value, error_rule.span).into())
    }

    fn merge_media_queries(
        queries1: &[MediaQuery],
        queries2: &[MediaQuery],
    ) -> Option<Vec<MediaQuery>> {
        let mut queries = Vec::new();

        for query1 in queries1 {
            for query2 in queries2 {
                match query1.merge(query2) {
                    MediaQueryMergeResult::Empty => continue,
                    MediaQueryMergeResult::Unrepresentable => return None,
                    MediaQueryMergeResult::Success(result) => queries.push(result),
                }
            }
        }

        Some(queries)
    }

    fn visit_media_queries(&mut self, queries: Interpolation) -> SassResult<Vec<CssMediaQuery>> {
        let resolved = self.perform_interpolation(queries, true)?;

        CssMediaQuery::parse_list(resolved, self.parser)
    }

    fn serialize_media_query(query: MediaQuery) -> String {
        let mut buffer = String::new();

        if let Some(modifier) = query.modifier {
            buffer.push_str(&modifier);
            buffer.push(' ');
        }

        if let Some(media_type) = query.media_type {
            buffer.push_str(&media_type);

            if !query.conditions.is_empty() {
                buffer.push_str(" and ");
            }
        }

        if query.conditions.len() == 1 && query.conditions.first().unwrap().starts_with("(not ") {
            buffer.push_str("not ");
            let condition = query.conditions.first().unwrap();
            buffer.push_str(&condition["(not ".len()..condition.len() - 1]);
        } else {
            let operator = if query.conjunction { " and " } else { " or " };
            buffer.push_str(&format!("{}", query.conditions.join(operator)))
        }

        buffer
    }

    //    if (query.modifier != null) {
    //   _buffer.write(query.modifier);
    //   _buffer.writeCharCode($space);
    // }

    // if (query.type != null) {
    //   _buffer.write(query.type);
    //   if (query.conditions.isNotEmpty) {
    //     _buffer.write(" and ");
    //   }
    // }

    // if (query.conditions.length == 1 &&
    //     query.conditions.first.startsWith("(not ")) {
    //   _buffer.write("not ");
    //   var condition = query.conditions.first;
    //   _buffer.write(condition.substring("(not ".length, condition.length - 1));
    // } else {
    //   var operator = query.conjunction ? "and" : "or";
    //   _writeBetween(query.conditions,
    //       _isCompressed ? "$operator " : " $operator ", _buffer.write);
    // }

    fn visit_media_rule(&mut self, media_rule: AstMedia) -> SassResult<Option<Value>> {
        // NOTE: this logic is largely duplicated in [visitCssMediaRule]. Most
        // changes here should be mirrored there.
        if self.declaration_name.is_some() {
            todo!("Media rules may not be used within nested declarations.")
        }

        let queries1 = self.visit_media_queries(media_rule.query)?;
        // todo: superfluous clone?
        let queries2 = self.media_queries.clone();
        let merged_queries = queries2
            .as_ref()
            .and_then(|queries2| Self::merge_media_queries(queries2, &queries1));

        // if let Some(merged_queries) = merged_queries {
        //     if merged_queries.is_empty() {
        //         return Ok(Vec::new());
        //     }
        // }

        let merged_sources = match &merged_queries {
            Some(merged_queries) if merged_queries.is_empty() => return Ok(None),
            Some(merged_queries) => {
                let mut set = IndexSet::new();
                set.extend(self.media_query_sources.clone().unwrap().into_iter());
                set.extend(self.media_queries.clone().unwrap().into_iter());
                set.extend(queries1.clone().into_iter());
                set
            }
            None => IndexSet::new(),
        };

        // dbg!(&merged_queries, &queries1);
        //     through: (node) =>
        //         node is CssStyleRule ||
        //         (mergedSources.isNotEmpty &&
        //             node is CssMediaRule &&
        //             node.queries.every(mergedSources.contains)),
        //     scopeWhen: node.hasDeclarations);

        let children = media_rule.body;

        let query = merged_queries.clone().unwrap_or_else(|| queries1.clone());

        let media_rule = Stmt::Media(
            Box::new(MediaRule {
                query: query
                    .into_iter()
                    .map(Self::serialize_media_query)
                    .collect::<Vec<String>>()
                    .join(", "),
                body: Vec::new(),
            }),
            self.style_rule_exists(),
        );

        let parent_idx = self.css_tree.add_stmt(media_rule, self.parent);

        self.with_parent::<SassResult<()>>(parent_idx, true, |visitor| {
            visitor.with_media_queries(
                Some(merged_queries.unwrap_or(queries1)),
                Some(merged_sources),
                |visitor| {
                    if !visitor.style_rule_exists() {
                        for stmt in children {
                            let result = visitor.visit_stmt(stmt)?;
                            assert!(result.is_none());
                        }
                    } else {
                        // If we're in a style rule, copy it into the media query so that
                        // declarations immediately inside @media have somewhere to go.
                        //
                        // For example, "a {@media screen {b: c}}" should produce
                        // "@media screen {a {b: c}}".
                        let selector = visitor.style_rule_ignoring_at_root.clone().unwrap();
                        let ruleset = Stmt::RuleSet {
                            selector,
                            body: Vec::new(),
                        };

                        let parent_idx = visitor.css_tree.add_stmt(ruleset, visitor.parent);

                        visitor.with_parent::<SassResult<()>>(parent_idx, false, |visitor| {
                            for stmt in children {
                                let result = visitor.visit_stmt(stmt)?;
                                assert!(result.is_none());
                            }

                            Ok(())
                        })?;
                    }

                    Ok(())
                },
            )
        })?;

        // if (_declarationName != null) {
        //   throw _exception(
        //       "Media rules may not be used within nested declarations.", node.span);
        // }

        // var queries = await _visitMediaQueries(node.query);
        // var mergedQueries = _mediaQueries
        //     .andThen((mediaQueries) => _mergeMediaQueries(mediaQueries, queries));
        // if (mergedQueries != null && mergedQueries.isEmpty) return null;

        // var mergedSources = mergedQueries == null
        //     ? const <CssMediaQuery>{}
        //     : {..._mediaQuerySources!, ..._mediaQueries!, ...queries};

        // await _withParent(
        //     ModifiableCssMediaRule(mergedQueries ?? queries, node.span), () async {
        //   await _withMediaQueries(mergedQueries ?? queries, mergedSources,
        //       () async {
        //     var styleRule = _styleRule;
        //     if (styleRule == null) {
        //       for (var child in node.children) {
        //         await child.accept(this);
        //       }
        //     } else {
        //     }
        //   });
        // },
        //     through: (node) =>
        //         node is CssStyleRule ||
        //         (mergedSources.isNotEmpty &&
        //             node is CssMediaRule &&
        //             node.queries.every(mergedSources.contains)),
        //     scopeWhen: node.hasDeclarations);

        // return null;
        Ok(None)
    }

    fn visit_unknown_at_rule(
        &mut self,
        unknown_at_rule: AstUnknownAtRule,
    ) -> SassResult<Option<Value>> {
        // NOTE: this logic is largely duplicated in [visitCssAtRule]. Most changes
        // here should be mirrored there.

        if self.declaration_name.is_some() {
            return Err((
                "At-rules may not be used within nested declarations.",
                self.parser.span_before,
            )
                .into());
        }

        let name = self.interpolation_to_value(unknown_at_rule.name, false, false)?;

        let value = unknown_at_rule
            .value
            .map(|v| self.interpolation_to_value(v, true, true))
            .transpose()?;

        if unknown_at_rule.children.is_none() {
            let stmt = Stmt::UnknownAtRule(Box::new(UnknownAtRule {
                name,
                params: value.unwrap_or_default(),
                body: Vec::new(),
                has_body: false,
            }));

            self.css_tree.add_stmt(stmt, self.parent);

            return Ok(None);
        }

        let was_in_keyframes = self.flags.in_keyframes();
        let was_in_unknown_at_rule = self.flags.in_unknown_at_rule();

        if unvendor(&name) == "keyframes" {
            self.flags.set(ContextFlags::IN_KEYFRAMES, true);
        } else {
            self.flags.set(ContextFlags::IN_UNKNOWN_AT_RULE, true);
        }

        let children = unknown_at_rule.children.unwrap();

        let stmt = Stmt::UnknownAtRule(Box::new(UnknownAtRule {
            name,
            params: value.unwrap_or_default(),
            body: Vec::new(),
            has_body: true,
        }));

        let parent_idx = self.css_tree.add_stmt(stmt, self.parent);

        self.with_parent::<SassResult<()>>(parent_idx, true, |visitor| {
            if !visitor.style_rule_exists() || visitor.flags.in_keyframes() {
                for stmt in children {
                    let result = visitor.visit_stmt(stmt)?;
                    assert!(result.is_none());
                }
            } else {
                // If we're in a style rule, copy it into the at-rule so that
                // declarations immediately inside it have somewhere to go.
                //
                // For example, "a {@foo {b: c}}" should produce "@foo {a {b: c}}".
                let selector = visitor.style_rule_ignoring_at_root.clone().unwrap();

                let style_rule = Stmt::RuleSet {
                    selector,
                    body: Vec::new(),
                };

                let parent_idx = visitor.css_tree.add_stmt(style_rule, visitor.parent);

                visitor.with_parent::<SassResult<()>>(parent_idx, false, |visitor| {
                    for stmt in children {
                        let result = visitor.visit_stmt(stmt)?;
                        assert!(result.is_none());
                    }

                    Ok(())
                })?;
            }

            Ok(())
        })?;

        self.flags.set(ContextFlags::IN_KEYFRAMES, was_in_keyframes);
        self.flags
            .set(ContextFlags::IN_UNKNOWN_AT_RULE, was_in_unknown_at_rule);

        Ok(None)
    }

    fn emit_warning(&mut self, message: Cow<str>, span: Span) {
        if self.parser.options.quiet {
            return;
        }
        let loc = self.parser.map.look_up_span(span);
        eprintln!(
            "Warning: {}\n    {} {}:{}  root stylesheet",
            message,
            loc.file.name(),
            loc.begin.line + 1,
            loc.begin.column + 1
        );
    }

    fn visit_warn_rule(&mut self, warn_rule: AstWarn) -> SassResult<()> {
        if self.warnings_emitted.insert(warn_rule.span) {
            let value = self.visit_expr(warn_rule.value)?;
            let message =
                value.to_css_string(warn_rule.span, self.parser.options.is_compressed())?;
            self.emit_warning(message, warn_rule.span);
        }

        Ok(())
        //         if (_quietDeps &&
        //     (_inDependency || (_currentCallable?.inDependency ?? false))) {
        //   return;
        // }

        // if (!_warningsEmitted.add(Tuple2(message, span))) return;
        // _logger.warn(message,
        //     span: span, trace: _stackTrace(span), deprecation: deprecation);
    }

    fn with_media_queries<T>(
        &mut self,
        queries: Option<Vec<MediaQuery>>,
        sources: Option<IndexSet<MediaQuery>>,
        callback: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let old_media_queries = self.media_queries.take();
        let old_media_query_sources = self.media_query_sources.take();
        self.media_queries = queries;
        self.media_query_sources = sources;
        let result = callback(self);
        self.media_queries = old_media_queries;
        self.media_query_sources = old_media_query_sources;
        result
    }

    fn with_environment<T>(
        &mut self,
        env: Environment,
        callback: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let mut old_env = env;
        mem::swap(&mut self.env, &mut old_env);
        let val = callback(self);
        mem::swap(&mut self.env, &mut old_env);
        val
    }

    fn with_parent<T>(
        &mut self,
        parent: CssTreeIdx,
        // default=true
        scope_when: bool,
        callback: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let old_parent = self.parent;
        self.parent = Some(parent);
        let result = self.with_scope(false, scope_when, callback);
        self.parent = old_parent;
        result
    }

    fn with_scope<T>(
        &mut self,
        // default=false
        semi_global: bool,
        // default=true
        when: bool,
        callback: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let semi_global = semi_global && self.flags.in_semi_global_scope();
        let was_in_semi_global_scope = self.flags.in_semi_global_scope();
        self.flags
            .set(ContextFlags::IN_SEMI_GLOBAL_SCOPE, semi_global);

        if !when {
            let v = callback(self);
            self.flags
                .set(ContextFlags::IN_SEMI_GLOBAL_SCOPE, was_in_semi_global_scope);

            return v;
        }

        self.env.scopes_mut().enter_new_scope();

        let v = callback(self);

        self.flags
            .set(ContextFlags::IN_SEMI_GLOBAL_SCOPE, was_in_semi_global_scope);
        self.env.scopes_mut().exit_scope();

        v
    }

    fn with_content<T>(
        &mut self,
        content: Option<Arc<CallableContentBlock>>,
        callback: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let old_content = self.env.content.take();
        self.env.content = content;
        let v = callback(self);
        self.env.content = old_content;
        v
    }

    fn visit_include_stmt(&mut self, include_stmt: AstInclude) -> SassResult<Option<Value>> {
        let mixin = self
            .env
            .get_mixin(include_stmt.name, include_stmt.namespace)?;

        match mixin {
            Mixin::Builtin(mixin) => {
                if include_stmt.content.is_some() {
                    todo!("Mixin doesn't accept a content block.")
                }

                //   await _runBuiltInCallable(node.arguments, mixin, nodeWithSpan);

                todo!()
            }
            Mixin::UserDefined(mixin, env) => {
                if include_stmt.content.is_some() && !mixin.has_content {
                    todo!("Mixin doesn't accept a content block.")
                }

                let AstInclude { args, content, .. } = include_stmt;

                let old_in_mixin = self.flags.in_mixin();
                self.flags.set(ContextFlags::IN_MIXIN, true);

                let callable_content = content.map(|c| {
                    Arc::new(CallableContentBlock {
                        content: c,
                        env: self.env.new_closure(),
                    })
                });

                self.run_user_defined_callable::<_, ()>(
                    MaybeEvaledArguments::Invocation(args),
                    mixin,
                    env,
                    include_stmt.name.span,
                    |mixin, visitor| {
                        visitor.with_content(callable_content, |visitor| {
                            for stmt in mixin.body {
                                let result = visitor.visit_stmt(stmt)?;
                                assert!(result.is_none());
                            }
                            Ok(())
                        })
                    },
                )?;

                self.flags.set(ContextFlags::IN_MIXIN, old_in_mixin);

                Ok(None)
            }
        }
    }

    fn visit_mixin_decl(&mut self, mixin: AstMixin) {
        self.env.insert_mixin(
            mixin.name,
            Mixin::UserDefined(mixin, self.env.new_closure()),
        );
    }

    fn visit_each_stmt(&mut self, each_stmt: AstEach) -> SassResult<Option<Value>> {
        let list = self.visit_expr(each_stmt.list)?.as_list();

        self.env.scopes_mut().enter_new_scope();

        let mut result = None;

        'outer: for val in list {
            if each_stmt.variables.len() == 1 {
                self.env
                    .scopes_mut()
                    .insert_var_last(each_stmt.variables[0], val);
            } else {
                for (&var, val) in each_stmt.variables.iter().zip(
                    val.as_list()
                        .into_iter()
                        .chain(std::iter::once(Value::Null).cycle()),
                ) {
                    self.env.scopes_mut().insert_var_last(var, val);
                }
            }

            for stmt in each_stmt.body.clone() {
                let val = self.visit_stmt(stmt)?;
                if val.is_some() {
                    result = val;
                    break 'outer;
                }
            }
        }

        self.env.scopes_mut().exit_scope();

        Ok(result)
        //     var list = await node.list.accept(this);
        // var nodeWithSpan = _expressionNode(node.list);
        // var setVariables = node.variables.length == 1
        //     ? (Value value) => _environment.setLocalVariable(node.variables.first,
        //         _withoutSlash(value, nodeWithSpan), nodeWithSpan)
        //     : (Value value) =>
        //         _setMultipleVariables(node.variables, value, nodeWithSpan);
        // return _environment.scope(() {
        //   return _handleReturn<Value>(list.asList, (element) {
        //     setVariables(element);
        //     return _handleReturn<Statement>(
        //         node.children, (child) => child.accept(this));
        //   });
        // }, semiGlobal: true);
        // todo!()
    }

    fn visit_for_stmt(&mut self, for_stmt: AstFor) -> SassResult<Option<Value>> {
        let from_number = self.visit_expr(for_stmt.from.node)?.assert_number()?;
        let to_number = self.visit_expr(for_stmt.to.node)?.assert_number()?;

        // todo: proper error here
        assert!(to_number.unit().comparable(&from_number.unit()));

        let from = from_number.num.to_i64().unwrap();
        let mut to = to_number
            .num()
            .convert(to_number.unit(), from_number.unit())
            .to_i64()
            .unwrap();

        let direction = if from > to { -1 } else { 1 };

        if !for_stmt.is_exclusive {
            to += direction;
        }

        if from == to {
            return Ok(None);
        }

        // todo: self.with_scopes
        self.env.scopes_mut().enter_new_scope();

        let mut result = None;

        let mut i = from;
        'outer: while i != to {
            self.env.scopes_mut().insert_var_last(
                for_stmt.variable.node,
                Value::Dimension {
                    num: Number::from(i),
                    unit: from_number.unit().clone(),
                    as_slash: None,
                },
            );

            for stmt in for_stmt.body.clone() {
                let val = self.visit_stmt(stmt)?;
                if val.is_some() {
                    result = val;
                    break 'outer;
                }
            }

            i += direction;
        }

        self.env.scopes_mut().exit_scope();

        Ok(result)
    }

    fn visit_while_stmt(&mut self, while_stmt: AstWhile) -> SassResult<Option<Value>> {
        self.with_scope::<SassResult<Option<Value>>>(
            true,
            while_stmt.has_declarations(),
            |visitor| {
                let mut result = None;

                'outer: while visitor.visit_expr(while_stmt.condition.clone())?.is_true() {
                    for stmt in while_stmt.body.clone() {
                        let val = visitor.visit_stmt(stmt)?;
                        if val.is_some() {
                            result = val;
                            break 'outer;
                        }
                    }
                }

                Ok(result)
            },
        )
    }

    fn visit_if_stmt(&mut self, if_stmt: AstIf) -> SassResult<Option<Value>> {
        let mut clause: Option<Vec<AstStmt>> = if_stmt.else_clause;
        for clause_to_check in if_stmt.if_clauses {
            if self.visit_expr(clause_to_check.condition)?.is_true() {
                clause = Some(clause_to_check.body);
                break;
            }
        }

        // todo: self.with_scope
        self.env.scopes_mut().enter_new_scope();

        let mut result = None;

        if let Some(stmts) = clause {
            for stmt in stmts {
                let val = self.visit_stmt(stmt)?;
                if val.is_some() {
                    result = val;
                    break;
                }
            }
        }

        self.env.scopes_mut().exit_scope();

        Ok(result)
    }

    fn visit_loud_comment(&mut self, comment: AstLoudComment) -> SassResult<Option<Value>> {
        if self.flags.in_function() {
            return Ok(None);
        }

        // todo:
        // // Comments are allowed to appear between CSS imports.
        // if (_parent == _root && _endOfImports == _root.children.length) {
        //   _endOfImports++;
        // }

        let comment = Stmt::Comment(
            self.perform_interpolation(comment.text, false)?,
            comment.span,
        );
        self.css_tree.add_stmt(comment, self.parent);

        Ok(None)
    }

    fn visit_variable_decl(&mut self, decl: AstVariableDecl) -> SassResult<Option<Value>> {
        let name = Spanned {
            node: decl.name,
            span: decl.span,
        };

        if decl.is_guarded {
            if decl.namespace.is_none() && self.env.at_root() {
                let var_override = (*self.configuration).borrow_mut().remove(decl.name);
                if !matches!(
                    var_override,
                    Some(ConfiguredValue {
                        value: Value::Null,
                        ..
                    }) | None
                ) {
                    self.env.insert_var(
                        name,
                        None,
                        var_override.unwrap().value,
                        true,
                        self.flags.in_semi_global_scope(),
                    )?;
                    return Ok(None);
                }
            }

            if self.env.var_exists(decl.name, decl.namespace)? {
                let value = self.env.get_var(name, decl.namespace).unwrap();

                if value != Value::Null {
                    return Ok(None);
                }
            }
        }

        if decl.is_global && !self.env.global_scope().borrow().var_exists(decl.name) {
            // todo: deprecation: true
            if self.env.at_root() {
                self.emit_warning(Cow::Borrowed("As of Dart Sass 2.0.0, !global assignments won't be able to declare new variables.\n\nSince this assignment is at the root of the stylesheet, the !global flag is\nunnecessary and can safely be removed."), decl.span);
            } else {
                self.emit_warning(Cow::Borrowed("As of Dart Sass 2.0.0, !global assignments won't be able to declare new variables.\n\nRecommendation: add `${node.originalName}: null` at the stylesheet root."), decl.span);
            }
        }

        let value = self.visit_expr(decl.value)?;
        let value = self.without_slash(value);

        self.env.insert_var(
            name,
            decl.namespace,
            value,
            decl.is_global,
            self.flags.in_semi_global_scope(),
        )?;
        // if decl.is_global || self.env.at_root() {
        //     self.env.global_scope_mut().insert_var(decl.name, value);
        // } else {
        //     // basically, if in_semi_global_scope AND var is global AND not re-declared, insert into last scope
        //     // why? i don't know
        //     self.env.scopes.borrow_mut().__insert_var(
        //         decl.name,
        //         value,
        //         &self.env.global_scope,
        //         self.flags.in_semi_global_scope(),
        //     );
        // }

        //   var value = _addExceptionSpan(node,
        //       () => _environment.getVariable(node.name, namespace: node.namespace));
        //   if (value != null && value != sassNull) return null;
        // }

        // var value =
        //     _withoutSlash(await node.expression.accept(this), node.expression);
        // _addExceptionSpan(node, () {
        //   _environment.setVariable(
        //       node.name, value, _expressionNode(node.expression),
        //       namespace: node.namespace, global: node.isGlobal);
        // });
        // return null
        // todo!()
        Ok(None)
    }

    fn interpolation_to_value(
        &mut self,
        interpolation: Interpolation,
        // default=false
        trim: bool,
        // default=false
        warn_for_color: bool,
    ) -> SassResult<String> {
        let result = self.perform_interpolation(interpolation, warn_for_color)?;

        Ok(if trim {
            trim_ascii(&result, true).to_owned()
        } else {
            result
        })
    }

    fn perform_interpolation(
        &mut self,
        interpolation: Interpolation,
        warn_for_color: bool,
    ) -> SassResult<String> {
        let span = self.parser.span_before;

        // todo: potential optimization for contents len == 1 and no exprs

        let result = interpolation.contents.into_iter().map(|part| match part {
            InterpolationPart::String(s) => Ok(s),
            InterpolationPart::Expr(e) => {
                let result = self.visit_expr(e)?;
                // todo: span for specific expr
                self.serialize(result, QuoteKind::None, span)
            }
        });

        result.collect()
    }

    fn evaluate_to_css(
        &mut self,
        expr: AstExpr,
        quote: QuoteKind,
        span: Span,
    ) -> SassResult<String> {
        let result = self.visit_expr(expr)?;
        self.serialize(result, quote, span)
    }

    fn without_slash(&mut self, v: Value) -> Value {
        match v {
            Value::Dimension { .. } if v.as_slash().is_some() => {
                //   String recommendation(SassNumber number) {
                //     var asSlash = number.asSlash;
                //     if (asSlash != null) {
                //       return "math.div(${recommendation(asSlash.item1)}, "
                //           "${recommendation(asSlash.item2)})";
                //     } else {
                //       return number.toString();
                //     }
                self.emit_warning(
                    Cow::Borrowed("Using / for division is deprecated and will be removed"),
                    self.parser.span_before,
                );
                //   _warn(
                //       "Using / for division is deprecated and will be removed in Dart Sass "
                //       "2.0.0.\n"
                //       "\n"
                //       "Recommendation: ${recommendation(value)}\n"
                //       "\n"
                //       "More info and automated migrator: "
                //       "https://sass-lang.com/d/slash-div",
                //       nodeForSpan.span,
                //       deprecation: true);
                // }
            }
            _ => {}
        }

        v.without_slash()
    }

    fn eval_maybe_args(
        &mut self,
        args: MaybeEvaledArguments,
        span: Span,
    ) -> SassResult<ArgumentResult> {
        match args {
            MaybeEvaledArguments::Invocation(args) => self.eval_args(args, span),
            MaybeEvaledArguments::Evaled(args) => Ok(args),
        }
    }

    fn eval_args(
        &mut self,
        arguments: ArgumentInvocation,
        span: Span,
    ) -> SassResult<ArgumentResult> {
        let mut positional = Vec::new();

        for expr in arguments.positional {
            let val = self.visit_expr(expr)?;
            positional.push(self.without_slash(val));
        }

        let mut named = BTreeMap::new();

        for (key, expr) in arguments.named {
            let val = self.visit_expr(expr)?;
            named.insert(key, self.without_slash(val));
        }

        if arguments.rest.is_none() {
            return Ok(ArgumentResult {
                positional,
                named,
                separator: ListSeparator::Undecided,
                span,
                touched: BTreeSet::new(),
            });
        }

        let rest = self.visit_expr(arguments.rest.unwrap())?;

        let mut separator = ListSeparator::Undecided;

        match rest {
            Value::Map(rest) => self.add_rest_map(&mut named, rest)?,
            Value::List(elems, list_separator, _) => {
                let mut list = elems
                    .into_iter()
                    .map(|e| self.without_slash(e))
                    .collect::<Vec<_>>();
                positional.append(&mut list);
                separator = list_separator;
            }
            Value::ArgList(arglist) => {
                // todo: superfluous clone
                for (&key, value) in arglist.keywords().into_iter() {
                    named.insert(key, self.without_slash(value.clone()));
                }

                let mut list = arglist
                    .elems
                    .into_iter()
                    .map(|e| self.without_slash(e))
                    .collect::<Vec<_>>();
                positional.append(&mut list);
                separator = arglist.separator;
            }
            _ => {
                positional.push(self.without_slash(rest));
            }
        }

        if arguments.keyword_rest.is_none() {
            return Ok(ArgumentResult {
                positional,
                named,
                separator: ListSeparator::Undecided,
                span: arguments.span,
                touched: BTreeSet::new(),
            });
        }

        match self.visit_expr(arguments.keyword_rest.unwrap())? {
            Value::Map(keyword_rest) => {
                self.add_rest_map(&mut named, keyword_rest)?;

                Ok(ArgumentResult {
                    positional,
                    named,
                    separator,
                    span: arguments.span,
                    touched: BTreeSet::new(),
                })
            }
            _ => {
                todo!("Variable keyword arguments must be a map (was $keywordRest).")
            }
        }
    }

    fn add_rest_map(
        &mut self,
        named: &mut BTreeMap<Identifier, Value>,
        rest: SassMap,
    ) -> SassResult<()> {
        for (key, val) in rest {
            match key {
                Value::String(text, ..) => {
                    named.insert(Identifier::from(text), val);
                }
                _ => todo!("Variable keyword argument map must have string keys.\n"),
            }
        }

        Ok(())
    }

    fn run_user_defined_callable<F: UserDefinedCallable, V: fmt::Debug>(
        &mut self,
        arguments: MaybeEvaledArguments,
        func: F,
        env: Environment,
        span: Span,
        run: impl FnOnce(F, &mut Self) -> SassResult<V>,
    ) -> SassResult<V> {
        let mut evaluated = self.eval_maybe_args(arguments, span)?;

        let mut name = func.name().to_string();

        if name != "@content" {
            name.push_str("()");
        }

        let val = self.with_environment::<SassResult<V>>(env, |visitor| {
            visitor.with_scope(false, true, move |visitor| {
                func.arguments()
                    .verify(evaluated.positional.len(), &evaluated.named)?;

                // todo: superfluous clone
                let declared_arguments = func.arguments().args.clone();
                let min_len = evaluated.positional.len().min(declared_arguments.len());

                for i in 0..min_len {
                    // todo: superfluous clone
                    visitor.env.scopes_mut().insert_var_last(
                        declared_arguments[i].name,
                        evaluated.positional[i].clone(),
                    );
                }

                // todo: better name for var
                let additional_declared_args =
                    if declared_arguments.len() > evaluated.positional.len() {
                        &declared_arguments[evaluated.positional.len()..declared_arguments.len()]
                    } else {
                        &[]
                    };

                for argument in additional_declared_args {
                    let name = argument.name;
                    let value = evaluated
                        .named
                        .remove(&argument.name)
                        .map(SassResult::Ok)
                        .unwrap_or_else(|| {
                            // todo: superfluous clone
                            let v = visitor.visit_expr(argument.default.clone().unwrap())?;
                            Ok(visitor.without_slash(v))
                        })?;
                    visitor.env.scopes_mut().insert_var_last(name, value);
                }

                let were_keywords_accessed = Arc::new(Cell::new(false));

                let argument_list = if let Some(rest_arg) = func.arguments().rest {
                    let rest = if evaluated.positional.len() > declared_arguments.len() {
                        &evaluated.positional[declared_arguments.len()..]
                    } else {
                        &[]
                    };

                    let arg_list = Value::ArgList(ArgList::new(
                        rest.to_vec(),
                        // todo: superfluous clone
                        Arc::clone(&were_keywords_accessed),
                        evaluated.named.clone(),
                        if evaluated.separator == ListSeparator::Undecided {
                            ListSeparator::Comma
                        } else {
                            ListSeparator::Space
                        },
                    ));

                    visitor
                        .env
                        .scopes_mut()
                        // todo: superfluous clone
                        .insert_var_last(rest_arg, arg_list.clone());

                    Some(arg_list)
                } else {
                    None
                };

                let val = run(func, visitor)?;

                if argument_list.is_none() || evaluated.named.is_empty() {
                    return Ok(val);
                }

                if (*were_keywords_accessed).get() {
                    return Ok(val);
                }
                //   if (argumentList.wereKeywordsAccessed) return result;

                let argument_word = if evaluated.named.len() == 1 {
                    "argument"
                } else {
                    "arguments"
                };

                let argument_names = to_sentence(
                    evaluated
                        .named
                        .keys()
                        .map(|key| format!("${key}"))
                        .collect(),
                    "or",
                );

                Err((format!("No {argument_word} named {argument_names}."), span).into())
                //   var argumentWord = pluralize('argument', evaluated.named.keys.length);
                //   var argumentNames =
                //       toSentence(evaluated.named.keys.map((name) => "\$$name"), 'or');
                //   throw MultiSpanSassRuntimeException(
                //       "No $argumentWord named $argumentNames.",
                //       nodeWithSpan.span,
                //       "invocation",
                //       {callable.declaration.arguments.spanWithName: "declaration"},
                //       _stackTrace(nodeWithSpan.span));
                // });
                // todo!("No arguments named")
            })
        });

        val
    }

    fn run_built_in_callable(
        &mut self,
        args: ArgumentInvocation,
        func: Builtin,
    ) -> SassResult<Value> {
        todo!()
    }

    pub(crate) fn run_function_callable(
        &mut self,
        func: SassFunction,
        arguments: ArgumentInvocation,
        span: Span,
    ) -> SassResult<Value> {
        self.run_function_callable_with_maybe_evaled(
            func,
            MaybeEvaledArguments::Invocation(arguments),
            span,
        )
    }

    pub(crate) fn run_function_callable_with_maybe_evaled(
        &mut self,
        func: SassFunction,
        arguments: MaybeEvaledArguments,
        span: Span,
    ) -> SassResult<Value> {
        match func {
            SassFunction::Builtin(func, name) => {
                let mut evaluated = self.eval_maybe_args(arguments, span)?;
                let val = func.0(evaluated, self)?;
                Ok(self.without_slash(val))
            }
            SassFunction::UserDefined(UserDefinedFunction {
                function,
                // scope_idx,
                env,
                ..
            }) => self.run_user_defined_callable(
                arguments,
                *function,
                env,
                span,
                |function, visitor| {
                    for stmt in function.children {
                        let result = visitor.visit_stmt(stmt)?;

                        if let Some(val) = result {
                            return Ok(val);
                        }
                    }

                    Err(("Function finished without @return.", span).into())
                },
            ),
            SassFunction::Plain { name } => {
                let arguments = match arguments {
                    MaybeEvaledArguments::Invocation(args) => args,
                    MaybeEvaledArguments::Evaled(..) => unreachable!(),
                };

                if !arguments.named.is_empty() || arguments.keyword_rest.is_some() {
                    return Err(
                        ("Plain CSS functions don't support keyword arguments.", span).into(),
                    );
                }

                let mut buffer = format!("{}(", name.as_str());
                let mut first = true;

                for argument in arguments.positional {
                    if first {
                        first = false;
                    } else {
                        buffer.push_str(", ");
                    }

                    buffer.push_str(&self.evaluate_to_css(argument, QuoteKind::Quoted, span)?);
                }

                if let Some(rest_arg) = arguments.rest {
                    let rest = self.visit_expr(rest_arg)?;
                    if !first {
                        buffer.push_str(", ");
                    }
                    buffer.push_str(&self.serialize(rest, QuoteKind::Quoted, span)?);
                }
                buffer.push(')');

                Ok(Value::String(buffer, QuoteKind::None))
            }
        }
    }

    fn visit_list_expr(&mut self, list: ListExpr) -> SassResult<Value> {
        let elems = list
            .elems
            .into_iter()
            .map(|e| {
                let span = e.span;
                let value = self.visit_expr(e.node)?;
                Ok(value)
            })
            .collect::<SassResult<Vec<_>>>()?;

        Ok(Value::List(elems, list.separator, list.brackets))
    }

    fn visit_function_call_expr(&mut self, func_call: FunctionCallExpr) -> SassResult<Value> {
        let name = func_call.name;

        let func = match self.env.get_fn(name, func_call.namespace)? {
            Some(func) => func,
            None => {
                if let Some(f) = GLOBAL_FUNCTIONS.get(name.as_str()) {
                    SassFunction::Builtin(f.clone(), name)
                } else {
                    if func_call.namespace.is_some() {
                        return Err(("Undefined function.", func_call.span).into());
                    }

                    SassFunction::Plain { name }
                }
            }
        };

        let old_in_function = self.flags.in_function();
        self.flags.set(ContextFlags::IN_FUNCTION, true);
        let value = self.run_function_callable(func, *func_call.arguments, func_call.span)?;
        self.flags.set(ContextFlags::IN_FUNCTION, old_in_function);

        Ok(value)

        //             var function = _addExceptionSpan(
        //     node, () => _getFunction(node.name, namespace: node.namespace));

        // if (function == null) {
        //   if (node.namespace != null) {
        //     throw _exception("Undefined function.", node.span);
        //   }

        //   function = PlainCssCallable(node.originalName);
        // }

        // var oldInFunction = _inFunction;
        // _inFunction = true;
        // var result = await _addErrorSpan(
        //     node, () => _runFunctionCallable(node.arguments, function, node));
        // _inFunction = oldInFunction;
        // return result;
        // todo!()
    }

    fn visit_interpolated_func_expr(&mut self, func: InterpolatedFunction) -> SassResult<Value> {
        let InterpolatedFunction {
            name,
            arguments: args,
            span,
        } = func;
        let fn_name = self.perform_interpolation(name, false)?;

        if !args.named.is_empty() || args.keyword_rest.is_some() {
            return Err(("Plain CSS functions don't support keyword arguments.", span).into());
        }

        let mut buffer = format!("{}(", fn_name);

        let mut first = true;
        for arg in args.positional {
            if first {
                first = false;
            } else {
                buffer.push_str(", ");
            }
            let evaluated = self.evaluate_to_css(arg, QuoteKind::Quoted, span)?;
            buffer.push_str(&evaluated);
        }

        if let Some(rest_arg) = args.rest {
            let rest = self.visit_expr(rest_arg)?;
            if !first {
                buffer.push_str(", ");
            }
            buffer.push_str(&self.serialize(rest, QuoteKind::None, span)?);
        }

        buffer.push(')');

        Ok(Value::String(buffer, QuoteKind::None))
    }

    fn visit_parent_selector(&self) -> Value {
        match &self.style_rule_ignoring_at_root {
            Some(selector) => selector.as_selector_list().clone().to_sass_list(),
            None => Value::Null,
        }
    }

    fn visit_expr(&mut self, expr: AstExpr) -> SassResult<Value> {
        Ok(match expr {
            AstExpr::Color(color) => Value::Color(color),
            AstExpr::Number { n, unit } => Value::Dimension {
                num: n,
                unit,
                as_slash: None,
            },
            AstExpr::List(list) => self.visit_list_expr(list)?,
            AstExpr::String(StringExpr(text, quote), span) => {
                self.visit_string(text, quote, span)?
            }
            AstExpr::BinaryOp {
                lhs,
                op,
                rhs,
                allows_slash,
                span,
            } => self.visit_bin_op(*lhs, op, *rhs, allows_slash, span)?,
            AstExpr::True => Value::True,
            AstExpr::False => Value::False,
            AstExpr::Calculation { name, args } => self.visit_calculation_expr(name, args)?,
            AstExpr::FunctionCall(func_call) => self.visit_function_call_expr(func_call)?,
            AstExpr::If(if_expr) => self.visit_ternary(*if_expr)?,
            AstExpr::InterpolatedFunction(func) => self.visit_interpolated_func_expr(func)?,
            AstExpr::Map(map) => self.visit_map(map)?,
            AstExpr::Null => Value::Null,
            AstExpr::Paren(expr) => self.visit_expr(*expr)?,
            AstExpr::ParentSelector => self.visit_parent_selector(),
            AstExpr::UnaryOp(op, expr) => self.visit_unary_op(op, *expr)?,
            AstExpr::Variable { name, namespace } => self.env.get_var(name, namespace)?,
        })
    }

    fn visit_calculation_value(
        &mut self,
        expr: AstExpr,
        in_min_or_max: bool,
    ) -> SassResult<CalculationArg> {
        Ok(match expr {
            AstExpr::Paren(inner) => match &*inner {
                AstExpr::FunctionCall(FunctionCallExpr { ref name, .. })
                    if name.as_str().to_ascii_lowercase() == "var" =>
                {
                    let result = self.visit_calculation_value(*inner, in_min_or_max)?;

                    if let CalculationArg::String(text) = result {
                        CalculationArg::String(format!("({})", text))
                    } else {
                        result
                    }
                }
                _ => self.visit_calculation_value(*inner, in_min_or_max)?,
            },
            AstExpr::String(string_expr, span) => {
                debug_assert!(string_expr.1 == QuoteKind::None);
                CalculationArg::String(self.perform_interpolation(string_expr.0, false)?)
            }
            AstExpr::BinaryOp { lhs, op, rhs, .. } => SassCalculation::operate_internal(
                op,
                self.visit_calculation_value(*lhs, in_min_or_max)?,
                self.visit_calculation_value(*rhs, in_min_or_max)?,
                in_min_or_max,
                !self.flags.in_supports_declaration(),
            )?,
            AstExpr::Number { .. }
            | AstExpr::Calculation { .. }
            | AstExpr::Variable { .. }
            | AstExpr::FunctionCall { .. }
            | AstExpr::If(..) => {
                let result = self.visit_expr(expr)?;
                match result {
                    Value::Dimension {
                        num,
                        unit,
                        as_slash,
                    } => CalculationArg::Number(SassNumber {
                        num: num.0,
                        unit: unit,
                        as_slash,
                    }),
                    Value::Calculation(calc) => CalculationArg::Calculation(calc),
                    Value::String(s, quotes) if quotes == QuoteKind::None => {
                        CalculationArg::String(s)
                    }
                    _ => todo!("Value $result can't be used in a calculation."),
                }
            }
            v => unreachable!("{:?}", v),
        })
    }

    fn visit_calculation_expr(
        &mut self,
        name: CalculationName,
        args: Vec<AstExpr>,
    ) -> SassResult<Value> {
        let mut args = args
            .into_iter()
            .map(|arg| self.visit_calculation_value(arg, name.in_min_or_max()))
            .collect::<SassResult<Vec<_>>>()?;

        if self.flags.in_supports_declaration() {
            return Ok(Value::Calculation(SassCalculation::unsimplified(
                name, args,
            )));
        }

        match name {
            CalculationName::Calc => {
                debug_assert_eq!(args.len(), 1);
                SassCalculation::calc(args.remove(0))
            }
            CalculationName::Min => SassCalculation::min(args),
            CalculationName::Max => SassCalculation::max(args),
            CalculationName::Clamp => {
                let min = args.remove(0);
                let value = if args.is_empty() {
                    None
                } else {
                    Some(args.remove(0))
                };
                let max = if args.is_empty() {
                    None
                } else {
                    Some(args.remove(0))
                };
                SassCalculation::clamp(min, value, max)
            }
        }
    }

    fn visit_unary_op(&mut self, op: UnaryOp, expr: AstExpr) -> SassResult<Value> {
        let operand = self.visit_expr(expr)?;

        match op {
            UnaryOp::Plus => operand.unary_plus(self),
            UnaryOp::Neg => operand.unary_neg(self),
            UnaryOp::Div => operand.unary_div(self),
            UnaryOp::Not => operand.unary_not(),
        }
    }

    fn visit_ternary(&mut self, if_expr: Ternary) -> SassResult<Value> {
        IF_ARGUMENTS().verify(if_expr.0.positional.len(), &if_expr.0.named)?;

        let mut positional = if_expr.0.positional;
        let mut named = if_expr.0.named;

        let condition = if positional.is_empty() {
            named.remove(&Identifier::from("condition")).unwrap()
        } else {
            positional.remove(0)
        };

        let if_true = if positional.is_empty() {
            named.remove(&Identifier::from("if_true")).unwrap()
        } else {
            positional.remove(0)
        };

        let if_false = if positional.is_empty() {
            named.remove(&Identifier::from("if_false")).unwrap()
        } else {
            positional.remove(0)
        };

        let value = if self.visit_expr(condition)?.is_true() {
            self.visit_expr(if_true)?
        } else {
            self.visit_expr(if_false)?
        };

        Ok(self.without_slash(value))
    }

    fn visit_string(
        &mut self,
        text: Interpolation,
        quote: QuoteKind,
        span: Span,
    ) -> SassResult<Value> {
        // Don't use [performInterpolation] here because we need to get the raw text
        // from strings, rather than the semantic value.
        let old_in_supports_declaration = self.flags.in_supports_declaration();
        self.flags.set(ContextFlags::IN_SUPPORTS_DECLARATION, false);

        let result = text
            .contents
            .into_iter()
            .map(|part| match part {
                InterpolationPart::String(s) => Ok(s),
                InterpolationPart::Expr(e) => match self.visit_expr(e)? {
                    Value::String(s, ..) => Ok(s),
                    e => self.serialize(e, QuoteKind::None, span),
                },
            })
            .collect::<SassResult<String>>()?;

        self.flags.set(
            ContextFlags::IN_SUPPORTS_DECLARATION,
            old_in_supports_declaration,
        );

        Ok(Value::String(result, quote))
    }

    fn visit_map(&mut self, map: AstSassMap) -> SassResult<Value> {
        let mut sass_map = SassMap::new();

        for pair in map.0 {
            let key = self.visit_expr(pair.0)?;
            let value = self.visit_expr(pair.1)?;

            if let Some(old_value) = sass_map.get_ref(&key) {
                todo!("Duplicate key.")
            }

            sass_map.insert(key, value);
        }

        Ok(Value::Map(sass_map))
    }

    fn visit_bin_op(
        &mut self,
        lhs: AstExpr,
        op: BinaryOp,
        rhs: AstExpr,
        allows_slash: bool,
        span: Span,
    ) -> SassResult<Value> {
        let left = self.visit_expr(lhs)?;

        Ok(match op {
            BinaryOp::SingleEq => {
                let right = self.visit_expr(rhs)?;
                single_eq(left, right, self.parser.options, span)?
            }
            BinaryOp::Or => {
                if left.is_true() {
                    left
                } else {
                    self.visit_expr(rhs)?
                }
            }
            BinaryOp::And => {
                if left.is_true() {
                    self.visit_expr(rhs)?
                } else {
                    left
                }
            }
            BinaryOp::Equal => {
                let right = self.visit_expr(rhs)?;
                Value::bool(left == right)
            }
            BinaryOp::NotEqual => {
                let right = self.visit_expr(rhs)?;
                Value::bool(left != right)
            }
            BinaryOp::GreaterThan
            | BinaryOp::GreaterThanEqual
            | BinaryOp::LessThan
            | BinaryOp::LessThanEqual => {
                let right = self.visit_expr(rhs)?;
                cmp(left, right, self.parser.options, span, op)?
            }
            BinaryOp::Plus => {
                let right = self.visit_expr(rhs)?;
                add(left, right, self.parser.options, span)?
            }
            BinaryOp::Minus => {
                let right = self.visit_expr(rhs)?;
                sub(left, right, self.parser.options, span)?
            }
            BinaryOp::Mul => {
                let right = self.visit_expr(rhs)?;
                mul(left, right, self.parser.options, span)?
            }
            BinaryOp::Div => {
                let right = self.visit_expr(rhs)?;

                let left_is_number = matches!(left, Value::Dimension { .. });
                let right_is_number = matches!(right, Value::Dimension { .. });

                let result = div(left.clone(), right.clone(), self.parser.options, span)?;

                if left_is_number && right_is_number && allows_slash {
                    return result.with_slash(left.assert_number()?, right.assert_number()?);
                } else if left_is_number && right_is_number {
                    //       String recommendation(Expression expression) {
                    //         if (expression is BinaryOperationExpression &&
                    //             expression.operator == BinaryOperator.dividedBy) {
                    //           return "math.div(${recommendation(expression.left)}, "
                    //               "${recommendation(expression.right)})";
                    //         } else if (expression is ParenthesizedExpression) {
                    //           return expression.expression.toString();
                    //         } else {
                    //           return expression.toString();
                    //         }
                    //       }

                    //       _warn(
                    //           "Using / for division outside of calc() is deprecated "
                    //           "and will be removed in Dart Sass 2.0.0.\n"
                    //           "\n"
                    //           "Recommendation: ${recommendation(node)} or calc($node)\n"
                    //           "\n"
                    //           "More info and automated migrator: "
                    //           "https://sass-lang.com/d/slash-div",
                    //           node.span,
                    //           deprecation: true);
                    // todo!()
                    self.emit_warning(
                        Cow::Owned(format!(
                            "Using / for division outside of calc() is deprecated"
                        )),
                        span,
                    );
                }

                result
            }
            BinaryOp::Rem => {
                let right = self.visit_expr(rhs)?;
                rem(left, right, self.parser.options, span)?
            }
        })
    }

    // todo: superfluous clone and non-use of cow
    fn serialize(&mut self, mut expr: Value, quote: QuoteKind, span: Span) -> SassResult<String> {
        if quote == QuoteKind::None {
            expr = expr.unquote();
        }

        Ok(expr
            .to_css_string(span, self.parser.options.is_compressed())?
            .into_owned())
    }

    pub fn visit_ruleset(&mut self, ruleset: AstRuleSet) -> SassResult<Option<Value>> {
        // NOTE: this logic is largely duplicated in [visitCssStyleRule]. Most
        // changes here should be mirrored there.

        if self.declaration_name.is_some() {
            todo!("Style rules may not be used within nested declarations.")
        }

        let AstRuleSet {
            selector: ruleset_selector,
            body: ruleset_body,
            ..
        } = ruleset;

        let selector_text = self.interpolation_to_value(ruleset_selector, true, true)?;

        if self.flags.in_keyframes() {
            // NOTE: this logic is largely duplicated in [visitCssKeyframeBlock]. Most
            // changes here should be mirrored there.

            let mut sel_toks = Lexer::new(
                selector_text
                    .chars()
                    .map(|x| Token::new(self.parser.span_before, x))
                    .collect(),
            );
            let parsed_selector = KeyframesSelectorParser::new(&mut Parser {
                toks: &mut sel_toks,
                map: self.parser.map,
                path: self.parser.path,
                is_plain_css: false,
                span_before: self.parser.span_before,
                flags: self.parser.flags,
                options: self.parser.options,
            })
            .parse_keyframes_selector()?;

            let keyframes_ruleset = Stmt::KeyframesRuleSet(Box::new(KeyframesRuleSet {
                selector: parsed_selector,
                body: Vec::new(),
            }));

            let parent_idx = self.css_tree.add_stmt(keyframes_ruleset, self.parent);

            self.with_parent::<SassResult<()>>(parent_idx, true, |visitor| {
                for stmt in ruleset_body {
                    let result = visitor.visit_stmt(stmt)?;
                    assert!(result.is_none());
                }

                Ok(())
            })?;

            return Ok(None);
        }

        let mut sel_toks = Lexer::new(
            selector_text
                .chars()
                .map(|x| Token::new(self.parser.span_before, x))
                .collect(),
        );

        let mut parsed_selector = SelectorParser::new(
            &mut Parser {
                toks: &mut sel_toks,
                map: self.parser.map,
                path: self.parser.path,
                is_plain_css: false,
                span_before: self.parser.span_before,
                flags: self.parser.flags,
                options: self.parser.options,
            },
            !self.is_plain_css,
            !self.is_plain_css,
            self.parser.span_before,
        )
        .parse()?;

        parsed_selector = parsed_selector.resolve_parent_selectors(
            self.style_rule_ignoring_at_root
                .as_ref()
                // todo: this clone should be superfluous(?)
                .map(|x| x.as_selector_list().clone()),
            !self.flags.at_root_excluding_style_rule(),
        )?;

        // todo: _mediaQueries
        let selector = self
            .extender
            .add_selector(parsed_selector, &self.media_queries);

        let rule = Stmt::RuleSet {
            selector: selector.clone(),
            body: Vec::new(),
        };

        let parent_idx = self.css_tree.add_stmt(rule, self.parent);

        let old_at_root_excluding_style_rule = self.flags.at_root_excluding_style_rule();

        self.flags
            .set(ContextFlags::AT_ROOT_EXCLUDING_STYLE_RULE, false);

        let old_style_rule_ignoring_at_root = self.style_rule_ignoring_at_root.take();
        self.style_rule_ignoring_at_root = Some(selector);

        self.with_parent::<SassResult<()>>(parent_idx, true, |visitor| {
            for stmt in ruleset_body {
                let result = visitor.visit_stmt(stmt)?;
                assert!(result.is_none());
            }

            Ok(())
        })?;

        self.style_rule_ignoring_at_root = old_style_rule_ignoring_at_root;
        self.flags.set(
            ContextFlags::AT_ROOT_EXCLUDING_STYLE_RULE,
            old_at_root_excluding_style_rule,
        );

        Ok(None)
        // Ok(vec![result])
        // Ok(vec![Stmt::RuleSet { selector, body }])

        // if (_declarationName != null) {
        //   throw _exception(
        //       "Style rules may not be used within nested declarations.", node.span);
        // }

        // var selectorText = await _interpolationToValue(node.selector,
        //     trim: true, warnForColor: true);
        // if (_inKeyframes) {

        //   var parsedSelector = _adjustParseError(
        //       node.selector,
        //       () => KeyframeSelectorParser(selectorText.value, logger: _logger)
        //           .parse());
        //   var rule = ModifiableCssKeyframeBlock(
        //       CssValue(List.unmodifiable(parsedSelector), node.selector.span),
        //       node.span);
        //   await _withParent(rule, () async {
        //     for (var child in node.children) {
        //       await child.accept(this);
        //     }
        //   },
        //       through: (node) => node is CssStyleRule,
        //       scopeWhen: node.hasDeclarations);
        //   return null;
        // }

        // var parsedSelector = _adjustParseError(
        //     node.selector,
        //     () => SelectorList.parse(selectorText.value,
        //         allowParent: !_stylesheet.plainCss,
        //         allowPlaceholder: !_stylesheet.plainCss,
        //         logger: _logger));
        // parsedSelector = _addExceptionSpan(
        //     node.selector,
        //     () => parsedSelector.resolveParentSelectors(
        //         _styleRuleIgnoringAtRoot?.originalSelector,
        //         implicitParent: !_atRootExcludingStyleRule));

        // var selector = _extensionStore.addSelector(
        //     parsedSelector, node.selector.span, _mediaQueries);
        // var rule = ModifiableCssStyleRule(selector, node.span,
        //     originalSelector: parsedSelector);
        // var oldAtRootExcludingStyleRule = _atRootExcludingStyleRule;
        // _atRootExcludingStyleRule = false;
        // await _withParent(rule, () async {
        //   await _withStyleRule(rule, () async {
        //     for (var child in node.children) {
        //       await child.accept(this);
        //     }
        //   });
        // },
        //     through: (node) => node is CssStyleRule,
        //     scopeWhen: node.hasDeclarations);
        // _atRootExcludingStyleRule = oldAtRootExcludingStyleRule;

        // if (!rule.isInvisibleOtherThanBogusCombinators) {
        //   for (var complex in parsedSelector.components) {
        //     if (!complex.isBogus) continue;

        //     if (complex.isUseless) {
        //       _warn(
        //           'The selector "${complex.toString().trim()}" is invalid CSS. It '
        //           'will be omitted from the generated CSS.\n'
        //           'This will be an error in Dart Sass 2.0.0.\n'
        //           '\n'
        //           'More info: https://sass-lang.com/d/bogus-combinators',
        //           node.selector.span,
        //           deprecation: true);
        //     } else if (complex.leadingCombinators.isNotEmpty) {
        //       _warn(
        //           'The selector "${complex.toString().trim()}" is invalid CSS.\n'
        //           'This will be an error in Dart Sass 2.0.0.\n'
        //           '\n'
        //           'More info: https://sass-lang.com/d/bogus-combinators',
        //           node.selector.span,
        //           deprecation: true);
        //     } else {
        //       _warn(
        //           'The selector "${complex.toString().trim()}" is only valid for '
        //                   "nesting and shouldn't\n"
        //                   'have children other than style rules.' +
        //               (complex.isBogusOtherThanLeadingCombinator
        //                   ? ' It will be omitted from the generated CSS.'
        //                   : '') +
        //               '\n'
        //                   'This will be an error in Dart Sass 2.0.0.\n'
        //                   '\n'
        //                   'More info: https://sass-lang.com/d/bogus-combinators',
        //           MultiSpan(node.selector.span, 'invalid selector', {
        //             rule.children.first.span: "this is not a style rule" +
        //                 (rule.children.every((child) => child is CssComment)
        //                     ? '\n(try converting to a //-style comment)'
        //                     : '')
        //           }),
        //           deprecation: true);
        //     }
        //   }
        // }

        // if (_styleRule == null && _parent.children.isNotEmpty) {
        //   var lastChild = _parent.children.last;
        //   lastChild.isGroupEnd = true;
        // }

        // return null;
        // todo!()
    }

    fn style_rule_exists(&self) -> bool {
        !self.flags.at_root_excluding_style_rule() && self.style_rule_ignoring_at_root.is_some()
    }

    pub fn visit_style(&mut self, style: AstStyle) -> SassResult<Option<Value>> {
        if !self.style_rule_exists()
            && !self.flags.in_unknown_at_rule()
            && !self.flags.in_keyframes()
        {
            todo!("Declarations may only be used within style rules.")
        }

        let is_custom_property = style.is_custom_property();

        let mut name = self.interpolation_to_value(style.name, false, true)?;

        if let Some(declaration_name) = &self.declaration_name {
            name = format!("{}-{}", declaration_name, name);
        }

        if let Some(value) = style
            .value
            .map(|s| {
                SassResult::Ok(Spanned {
                    node: self.visit_expr(s.node)?,
                    span: s.span,
                })
            })
            .transpose()?
        {
            // If the value is an empty list, preserve it, because converting it to CSS
            // will throw an error that we want the user to see.
            if !value.is_null() || value.is_empty_list() {
                // todo: superfluous clones?
                self.css_tree.add_stmt(
                    Stmt::Style(Style {
                        property: InternedString::get_or_intern(&name),
                        value: Box::new(value),
                        declared_as_custom_property: is_custom_property,
                    }),
                    self.parent,
                );
            } else if name.starts_with("--") {
                return Err(("Custom property values may not be empty.", style.span).into());
            }
        }

        let children = style.body;

        if !children.is_empty() {
            let old_declaration_name = self.declaration_name.take();
            self.declaration_name = Some(name);
            self.with_scope::<SassResult<()>>(false, true, |visitor| {
                for stmt in children {
                    let result = visitor.visit_stmt(stmt)?;
                    assert!(result.is_none());
                }

                Ok(())
            })?;
            name = self.declaration_name.take().unwrap();
            self.declaration_name = old_declaration_name;
        }

        Ok(None)
    }
}
