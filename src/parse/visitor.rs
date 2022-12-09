use std::{
    borrow::Borrow,
    cell::{Ref, RefCell, RefMut},
    collections::{BTreeMap, BTreeSet, HashSet},
    fmt,
    iter::FromIterator,
    mem,
    ops::{Deref, Index, IndexMut},
    sync::Arc,
};

use codemap::{Span, Spanned};
use num_traits::ToPrimitive;

use crate::{
    atrule::{
        keyframes::KeyframesRuleSet,
        media::{MediaQuery, MediaQueryMergeResult, MediaRule},
        mixin::Mixin,
        UnknownAtRule,
    },
    builtin::{meta::IF_ARGUMENTS, modules::Modules, Builtin, GLOBAL_FUNCTIONS},
    color::Color,
    common::{unvendor, BinaryOp, Identifier, ListSeparator, QuoteKind, UnaryOp},
    error::SassError,
    interner::InternedString,
    lexer::Lexer,
    parse::SassResult,
    scope::{Scope, Scopes},
    selector::{
        ComplexSelectorComponent, ExtendRule, ExtendedSelector, Extender, Selector, SelectorList,
        SelectorParser,
    },
    style::Style,
    token::Token,
    value::{ArgList, Number, SassFunction, SassMap, UserDefinedFunction, Value},
    Options,
};

use super::{
    common::ContextFlags,
    keyframes::KeyframesSelectorParser,
    value::{add, cmp, div, mul, rem, single_eq, sub},
    value_new::{
        Argument, ArgumentDeclaration, ArgumentInvocation, ArgumentResult, AstExpr, AstSassMap,
        StringExpr,
    },
    AstAtRootRule, AstContentBlock, AstEach, AstErrorRule, AstExtendRule, AstFor, AstFunctionDecl,
    AstIf, AstInclude, AstLoudComment, AstMedia, AstMixin, AstRuleSet, AstStmt, AstStyle,
    AstUnknownAtRule, AstVariableDecl, AstWarn, AstWhile, AtRootQuery, CssMediaQuery,
    Interpolation, InterpolationPart, Parser, Stmt, StyleSheet,
};

#[derive(Debug, Clone)]
pub(crate) enum AstStmtEvalResult {
    // todo: single stmt result to avoid superfluous allocation
    // Stmt(Stmt),
    Stmts(Vec<Stmt>),
    Return(Value),
}

#[derive(Debug, Clone)]
struct CssTree {
    stmts: Vec<Stmt>,
    // parent=>children
    parent_mapping: BTreeMap<CssTreeIdx, Vec<CssTreeIdx>>,
}

impl CssTree {
    pub fn new() -> Self {
        Self {
            stmts: Vec::new(),
            parent_mapping: BTreeMap::new(),
        }
    }

    fn add_child(&mut self, child: Stmt, parent: CssTreeIdx) -> CssTreeIdx {
        let child_idx = self.add_toplevel(child);
        self.parent_mapping
            .entry(parent)
            .or_default()
            .push(child_idx);
        child_idx
    }

    pub fn add_stmt(&mut self, child: Stmt, parent: Option<CssTreeIdx>) -> CssTreeIdx {
        match parent {
            Some(parent) => self.add_child(child, parent),
            None => self.add_toplevel(child),
        }
    }

    fn add_toplevel(&mut self, stmt: Stmt) -> CssTreeIdx {
        let idx = CssTreeIdx(self.stmts.len());
        self.stmts.push(stmt);
        idx
    }
}

impl Index<CssTreeIdx> for CssTree {
    type Output = Stmt;
    fn index(&self, index: CssTreeIdx) -> &Self::Output {
        &self.stmts[index.0]
    }
}

impl IndexMut<CssTreeIdx> for CssTree {
    fn index_mut(&mut self, index: CssTreeIdx) -> &mut Self::Output {
        &mut self.stmts[index.0]
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
        self.name
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

#[derive(Debug, Clone)]
pub(crate) struct Environment {
    pub scopes: Scopes,
    pub global_scope: Arc<RefCell<Scope>>,
    pub modules: Modules,
    // todo: content
}

impl Environment {
    pub fn new() -> Self {
        Self {
            scopes: Scopes::new(),
            global_scope: Arc::new(RefCell::new(Scope::new())),
            modules: Modules::default(),
        }
    }

    pub fn new_closure(&self) -> Self {
        Self {
            scopes: self.scopes.clone(),
            global_scope: Arc::clone(&self.global_scope),
            modules: self.modules.clone(),
        }
    }

    pub fn at_root(&self) -> bool {
        self.scopes.is_empty()
    }

    pub fn global_scope(&self) -> Ref<Scope> {
        (*self.global_scope).borrow()
    }

    pub fn global_scope_mut(&mut self) -> RefMut<Scope> {
        (*self.global_scope).borrow_mut()
    }
}

pub(crate) struct Visitor<'a> {
    pub declaration_name: Option<String>,
    pub flags: ContextFlags,
    pub parser: &'a mut Parser<'a, 'a>,
    pub env: Environment,
    pub style_rule_ignoring_at_root: Option<ExtendedSelector>,
    pub content: Option<AstContentBlock>,
    // avoid emitting duplicate warnings for the same span
    pub warnings_emitted: HashSet<Span>,
    pub media_queries: Option<Vec<MediaQuery>>,
    pub media_query_sources: Option<HashSet<MediaQuery>>,
    pub extender: Extender,
    css_tree: CssTree,
    parent: Option<CssTreeIdx>,
}

impl<'a> Visitor<'a> {
    pub fn new(parser: &'a mut Parser<'a, 'a>) -> Self {
        let mut flags = ContextFlags::empty();
        flags.set(ContextFlags::IN_SEMI_GLOBAL_SCOPE, true);

        let extender = Extender::new(parser.span_before);
        Self {
            declaration_name: None,
            parser,
            style_rule_ignoring_at_root: None,
            flags,
            content: None,
            warnings_emitted: HashSet::new(),
            media_queries: None,
            media_query_sources: None,
            env: Environment::new(),
            extender,
            css_tree: CssTree::new(),
            parent: None,
        }
    }

    pub fn visit_stylesheet(mut self, style_sheet: StyleSheet) -> SassResult<Vec<Stmt>> {
        let mut body = Vec::new();
        for stmt in style_sheet.body {
            match self.visit_stmt(stmt)? {
                AstStmtEvalResult::Return(..) => unreachable!(),
                AstStmtEvalResult::Stmts(mut stmts) => body.append(&mut stmts),
            }
        }

        Ok(self.css_tree.stmts)
    }

    pub fn visit_stmt(&mut self, stmt: AstStmt) -> SassResult<AstStmtEvalResult> {
        let res = match stmt {
            AstStmt::RuleSet(ruleset) => self.visit_ruleset(ruleset),
            AstStmt::Style(style) => self.visit_style(style),
            AstStmt::SilentComment(..) => Ok(Vec::new()),
            AstStmt::If(if_stmt) => return self.visit_if_stmt(if_stmt),
            AstStmt::For(for_stmt) => return self.visit_for_stmt(for_stmt),
            AstStmt::Return(ret) => {
                return Ok(AstStmtEvalResult::Return(
                    self.visit_expr(ret.val)?.unwrap(),
                ))
            }
            AstStmt::Each(each_stmt) => return self.visit_each_stmt(each_stmt),
            AstStmt::Media(media_rule) => return self.visit_media_rule(media_rule),
            AstStmt::Include(include_stmt) => self.visit_include_stmt(include_stmt),
            AstStmt::While(while_stmt) => return self.visit_while_stmt(while_stmt),
            AstStmt::VariableDecl(decl) => self.visit_variable_decl(decl),
            AstStmt::LoudComment(comment) => self.visit_loud_comment(comment),
            AstStmt::PlainCssImport(_) => todo!(),
            AstStmt::AstSassImport(_) => todo!(),
            AstStmt::FunctionDecl(func) => self.visit_function_decl(func),
            AstStmt::Mixin(mixin) => self.visit_mixin(mixin),
            AstStmt::ContentRule(_) => todo!(),
            AstStmt::Warn(warn_rule) => {
                self.warn(warn_rule)?;
                Ok(Vec::new())
            }
            AstStmt::UnknownAtRule(unknown_at_rule) => self.visit_unknown_at_rule(unknown_at_rule),
            AstStmt::ErrorRule(error_rule) => return Err(self.visit_error_rule(error_rule)?),
            AstStmt::Extend(extend_rule) => self.visit_extend_rule(extend_rule),
            AstStmt::AtRootRule(at_root_rule) => self.visit_at_root_rule(at_root_rule),
        };

        Ok(AstStmtEvalResult::Stmts(res?))
    }

    fn visit_at_root_rule(&mut self, at_root_rule: AstAtRootRule) -> SassResult<Vec<Stmt>> {
        let query = match at_root_rule.query {
            Some(val) => {
                let resolved = self.perform_interpolation(val, true)?;
                //   query = _adjustParseError(
                //       unparsedQuery, () => AtRootQuery.parse(resolved, logger: _logger));

                todo!()
            }
            None => AtRootQuery::default(),
        };

        // var parent = _parent;
        // var included = <ModifiableCssParentNode>[];
        // while (parent is! CssStylesheet) {
        //   if (!query.excludes(parent)) included.add(parent);

        //   var grandparent = parent.parent;
        //   if (grandparent == null) {
        //     throw StateError(
        //         "CssNodes must have a CssStylesheet transitive parent node.");
        //   }

        //   parent = grandparent;
        // }
        // var root = _trimIncluded(included);

        // for child in at_root_rule.children {
        //     match self.visit_stmt(child)? {
        //         AstStmtEvalResult::Return(..) => unreachable!(),
        //         AstStmtEvalResult::Stmts(mut stmts) => self.root.append(&mut stmts),
        //     }
        // }

        // // If we didn't exclude any rules, we don't need to use the copies we might
        // // have created.
        // if (root == _parent) {
        //   await _environment.scope(() async {
        //     for (var child in node.children) {
        //       await child.accept(this);
        //     }
        //   }, when: node.hasDeclarations);
        //   return null;
        // }

        // var innerCopy = root;
        // if (included.isNotEmpty) {
        //   innerCopy = included.first.copyWithoutChildren();
        //   var outerCopy = innerCopy;
        //   for (var node in included.skip(1)) {
        //     var copy = node.copyWithoutChildren();
        //     copy.addChild(outerCopy);
        //     outerCopy = copy;
        //   }

        //   root.addChild(outerCopy);
        // }

        // await _scopeForAtRoot(node, innerCopy, query, included)(() async {
        //   for (var child in node.children) {
        //     await child.accept(this);
        //   }
        // });

        // return null;
        // todo!()
        Ok(Vec::new())
    }

    fn visit_function_decl(&mut self, fn_decl: AstFunctionDecl) -> SassResult<Vec<Stmt>> {
        let name = fn_decl.name;
        // todo: no global scope?
        // todo: independency
        self.env.scopes.insert_fn(
            name,
            SassFunction::UserDefined(UserDefinedFunction {
                function: Box::new(fn_decl),
                name,
                env: self.env.new_closure(),
            }),
        );
        Ok(Vec::new())
    }

    fn parse_selector_from_string(&mut self, selector_text: String) -> SassResult<SelectorList> {
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
                scopes: self.parser.scopes,
                // global_scope: self.parser.global_scope,
                // super_selectors: self.parser.super_selectors,
                span_before: self.parser.span_before,
                content: self.parser.content,
                flags: self.parser.flags,
                at_root: self.parser.at_root,
                at_root_has_selector: self.parser.at_root_has_selector,
                // extender: self.parser.extender,
                content_scopes: self.parser.content_scopes,
                options: self.parser.options,
                modules: self.parser.modules,
                module_config: self.parser.module_config,
            },
            !self.flags.in_plain_css(),
            !self.flags.in_plain_css(),
            self.parser.span_before,
        )
        .parse()
    }

    fn visit_extend_rule(&mut self, extend_rule: AstExtendRule) -> SassResult<Vec<Stmt>> {
        if self.style_rule_ignoring_at_root.is_none() || self.declaration_name.is_some() {
            todo!("@extend may only be used within style rules.")
        }

        let super_selector = self.style_rule_ignoring_at_root.clone().unwrap();

        let target_text = self.interpolation_to_value(extend_rule.value, false, true)?;

        let list = self.parse_selector_from_string(target_text)?;

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

        Ok(Vec::new())
    }

    fn visit_error_rule(&mut self, error_rule: AstErrorRule) -> SassResult<Box<SassError>> {
        let value = self
            .visit_expr(error_rule.value)?
            .unwrap()
            .inspect(error_rule.span)?
            .into_owned();

        Ok((value, error_rule.span).into())
    }

    fn merge_media_queries(
        &mut self,
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

    fn visit_media_rule(&mut self, media_rule: AstMedia) -> SassResult<AstStmtEvalResult> {
        // NOTE: this logic is largely duplicated in [visitCssMediaRule]. Most
        // changes here should be mirrored there.
        if self.declaration_name.is_some() {
            todo!("Media rules may not be used within nested declarations.")
        }

        let queries1 = self.visit_media_queries(media_rule.query)?;
        let queries2 = self.media_queries.take();
        let merged_queries = queries2
            .as_ref()
            .and_then(|queries2| self.merge_media_queries(&queries1, queries2));

        // if let Some(merged_queries) = merged_queries {
        //     if merged_queries.is_empty() {
        //         return Ok(Vec::new());
        //     }
        // }

        let merged_sources = match &merged_queries {
            Some(merged_queries) if merged_queries.is_empty() => {
                return Ok(AstStmtEvalResult::Stmts(Vec::new()))
            }
            Some(merged_queries) => {
                let mut set = HashSet::new();
                set.extend(self.media_query_sources.clone().unwrap().into_iter());
                set.extend(self.media_queries.clone().unwrap().into_iter());
                set.extend(queries1.clone().into_iter());
                set
            }
            None => HashSet::new(),
        };

        //     through: (node) =>
        //         node is CssStyleRule ||
        //         (mergedSources.isNotEmpty &&
        //             node is CssMediaRule &&
        //             node.queries.every(mergedSources.contains)),
        //     scopeWhen: node.hasDeclarations);

        let children = media_rule.body;

        let query = merged_queries.clone().unwrap_or_else(|| queries1.clone());

        let result = self.with_scope::<SassResult<AstStmtEvalResult>>(false, true, |visitor| {
            visitor.with_media_queries(
                Some(merged_queries.unwrap_or(queries1)),
                Some(merged_sources),
                |visitor| {
                    let mut result = Vec::new();
                    // todo: exists
                    if !visitor.style_rule_exists() {
                        for child in children {
                            match visitor.visit_stmt(child)? {
                                AstStmtEvalResult::Return(..) => unreachable!(),
                                AstStmtEvalResult::Stmts(mut stmts) => result.append(&mut stmts),
                            }
                        }
                    } else {
                        // If we're in a style rule, copy it into the media query so that
                        // declarations immediately inside @media have somewhere to go.
                        //
                        // For example, "a {@media screen {b: c}}" should produce
                        // "@media screen {a {b: c}}".
                        return visitor.with_scope(false, false, |visitor| {
                            let selector = visitor.style_rule_ignoring_at_root.clone().unwrap();

                            let mut result = Vec::new();

                            for child in children {
                                match visitor.visit_stmt(child)? {
                                    AstStmtEvalResult::Return(..) => unreachable!(),
                                    AstStmtEvalResult::Stmts(mut stmts) => {
                                        result.append(&mut stmts)
                                    }
                                }
                            }

                            let ruleset = Stmt::RuleSet {
                                selector,
                                body: result,
                            };

                            Ok(AstStmtEvalResult::Stmts(vec![Stmt::Media(Box::new(
                                MediaRule {
                                    query: query
                                        .into_iter()
                                        .map(|query| query.to_string())
                                        .collect::<Vec<String>>()
                                        .join(", "),
                                    body: vec![ruleset],
                                },
                            ))]))
                        });
                    }

                    Ok(AstStmtEvalResult::Stmts(vec![Stmt::Media(Box::new(
                        MediaRule {
                            query: query
                                .into_iter()
                                .map(|query| query.to_string())
                                .collect::<Vec<String>>()
                                .join(", "),
                            body: result,
                        },
                    ))]))
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
        Ok(result)
    }

    fn visit_unknown_at_rule(
        &mut self,
        unknown_at_rule: AstUnknownAtRule,
    ) -> SassResult<Vec<Stmt>> {
        // NOTE: this logic is largely duplicated in [visitCssAtRule]. Most changes
        // here should be mirrored there.

        if self.declaration_name.is_some() {
            todo!("At-rules may not be used within nested declarations.")
        }

        let name = self.interpolation_to_value(unknown_at_rule.name, false, false)?;

        let value = unknown_at_rule
            .value
            .map(|v| self.interpolation_to_value(v, true, true))
            .transpose()?;

        if unknown_at_rule.children.is_none() {
            return Ok(vec![Stmt::UnknownAtRule(Box::new(UnknownAtRule {
                name,
                params: value.unwrap_or_default(),
                body: Vec::new(),
                has_body: false,
            }))]);
        }

        let was_in_keyframes = self.flags.in_keyframes();
        let was_in_unknown_at_rule = self.flags.in_unknown_at_rule();

        if unvendor(&name) == "keyframes" {
            self.flags.set(ContextFlags::IN_KEYFRAMES, true);
        } else {
            self.flags.set(ContextFlags::IN_UNKNOWN_AT_RULE, true);
        }

        let children = unknown_at_rule.children.unwrap();

        let body = self.with_scope::<SassResult<Vec<Stmt>>>(false, true, |visitor| {
            let mut result = Vec::new();
            if !visitor.style_rule_exists() || visitor.flags.in_keyframes() {
                for child in children {
                    match visitor.visit_stmt(child)? {
                        AstStmtEvalResult::Return(..) => unreachable!(),
                        AstStmtEvalResult::Stmts(mut stmts) => result.append(&mut stmts),
                    }
                }
            } else {
                // If we're in a style rule, copy it into the at-rule so that
                // declarations immediately inside it have somewhere to go.
                //
                // For example, "a {@foo {b: c}}" should produce "@foo {a {b: c}}".
                return visitor.with_scope(false, false, |visitor| {
                    let selector = visitor.style_rule_ignoring_at_root.clone().unwrap();

                    for child in children {
                        match visitor.visit_stmt(child)? {
                            AstStmtEvalResult::Return(..) => unreachable!(),
                            AstStmtEvalResult::Stmts(mut stmts) => result.append(&mut stmts),
                        }
                    }

                    Ok(vec![Stmt::RuleSet {
                        selector,
                        body: result,
                    }])
                });
            }

            Ok(result)
        })?;

        // await _withParent(ModifiableCssAtRule(name, node.span, value: value),
        //     () async {
        //   var styleRule = _styleRule;
        //   if (styleRule == null || _inKeyframes) {
        //     for (var child in children) {
        //       await child.accept(this);
        //     }
        //   } else {
        //   }
        // },
        //     through: (node) => node is CssStyleRule,
        //     scopeWhen: node.hasDeclarations);

        self.flags.set(ContextFlags::IN_KEYFRAMES, was_in_keyframes);
        self.flags
            .set(ContextFlags::IN_UNKNOWN_AT_RULE, was_in_unknown_at_rule);

        // _inUnknownAtRule = wasInUnknownAtRule;
        // _inKeyframes = wasInKeyframes;
        // return null;
        Ok(vec![Stmt::UnknownAtRule(Box::new(UnknownAtRule {
            name,
            params: value.unwrap_or_default(),
            body,
            has_body: true,
        }))])
    }

    fn emit_warning(&mut self, message: crate::Cow<str>, span: Span) {
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

    fn warn(&mut self, warn_rule: AstWarn) -> SassResult<()> {
        if self.warnings_emitted.insert(warn_rule.span) {
            let value = self.visit_expr(warn_rule.value)?.unwrap();
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
        sources: Option<HashSet<MediaQuery>>,
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
        // default=false
        semi_global: bool,
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

        self.env.scopes.enter_new_scope();

        let v = callback(self);

        self.flags
            .set(ContextFlags::IN_SEMI_GLOBAL_SCOPE, was_in_semi_global_scope);
        self.env.scopes.exit_scope();

        v
    }

    fn visit_include_stmt(&mut self, include_stmt: AstInclude) -> SassResult<Vec<Stmt>> {
        let mixin = self.env.scopes.get_mixin(
            Spanned {
                node: include_stmt.name,
                span: self.parser.span_before,
            },
            self.env.global_scope(),
        )?;

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

                let args = include_stmt.args;
                let new_content = include_stmt.content;

                let old_in_mixin = self.flags.in_mixin();
                self.flags.set(ContextFlags::IN_MIXIN, true);

                let result = self.run_user_defined_callable::<_, Vec<Stmt>>(
                    args,
                    mixin,
                    env,
                    |mixin, visitor| {
                        let old_content = visitor.content.take();
                        visitor.content = new_content;

                        let mut result = Vec::new();

                        for stmt in mixin.body {
                            match visitor.visit_stmt(stmt)? {
                                AstStmtEvalResult::Stmts(mut stmts) => result.append(&mut stmts),
                                AstStmtEvalResult::Return(..) => unreachable!(),
                            }
                        }

                        visitor.content = old_content;

                        Ok(result)
                    },
                )?;

                self.flags.set(ContextFlags::IN_MIXIN, old_in_mixin);

                Ok(result)
            }
        }
    }

    fn visit_mixin(&mut self, mixin: AstMixin) -> SassResult<Vec<Stmt>> {
        if self.style_rule_exists() {
            self.env.scopes.insert_mixin(
                mixin.name,
                Mixin::UserDefined(mixin, self.env.new_closure()),
            );
        } else {
            self.env.global_scope.borrow_mut().insert_mixin(
                mixin.name,
                Mixin::UserDefined(mixin, self.env.new_closure()),
            );
        }
        Ok(Vec::new())
    }

    fn visit_each_stmt(&mut self, each_stmt: AstEach) -> SassResult<AstStmtEvalResult> {
        let list = self.visit_expr(each_stmt.list)?.unwrap().as_list();

        self.env.scopes.enter_new_scope();

        let mut result = Vec::new();

        for val in list {
            if each_stmt.variables.len() == 1 {
                self.env.scopes.insert_var_last(each_stmt.variables[0], val);
            } else {
                for (&var, val) in each_stmt.variables.iter().zip(
                    val.as_list()
                        .into_iter()
                        .chain(std::iter::once(Value::Null).cycle()),
                ) {
                    self.env.scopes.insert_var_last(var, val);
                }
            }

            for stmt in each_stmt.body.clone() {
                match self.visit_stmt(stmt)? {
                    AstStmtEvalResult::Return(val) => {
                        debug_assert!(result.is_empty());
                        return Ok(AstStmtEvalResult::Return(val));
                    }
                    AstStmtEvalResult::Stmts(mut stmts) => result.append(&mut stmts),
                }
            }
        }

        self.env.scopes.exit_scope();

        Ok(AstStmtEvalResult::Stmts(result))
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

    fn visit_for_stmt(&mut self, for_stmt: AstFor) -> SassResult<AstStmtEvalResult> {
        let from_number = self
            .visit_expr(for_stmt.from.node)?
            .unwrap()
            .assert_number()?;
        let to_number = self
            .visit_expr(for_stmt.to.node)?
            .unwrap()
            .assert_number()?;

        assert!(to_number.unit.comparable(&from_number.unit));

        let from = from_number.num.to_i64().unwrap();
        let mut to = to_number
            .num
            .convert(&to_number.unit, &from_number.unit)
            .to_i64()
            .unwrap();

        let direction = if from > to { -1 } else { 1 };

        if !for_stmt.is_exclusive {
            to += direction;
        }

        if from == to {
            return Ok(AstStmtEvalResult::Stmts(Vec::new()));
        }

        self.env.scopes.enter_new_scope();

        let mut result = Vec::new();

        let mut i = from;
        while i != to {
            self.env.scopes.insert_var_last(
                for_stmt.variable.node,
                Value::Dimension(Some(Number::from(i)), from_number.unit.clone(), true),
            );

            for stmt in for_stmt.body.clone() {
                match self.visit_stmt(stmt)? {
                    AstStmtEvalResult::Return(val) => {
                        debug_assert!(result.is_empty());
                        return Ok(AstStmtEvalResult::Return(val));
                    }
                    AstStmtEvalResult::Stmts(mut stmts) => result.append(&mut stmts),
                }
            }

            i += direction;
        }

        self.env.scopes.exit_scope();

        Ok(AstStmtEvalResult::Stmts(result))

        //     var fromNumber = await _addExceptionSpanAsync(
        //     node.from, () async => (await node.from.accept(this)).assertNumber());
        // var toNumber = await _addExceptionSpanAsync(
        //     node.to, () async => (await node.to.accept(this)).assertNumber());

        // var from = _addExceptionSpan(node.from, () => fromNumber.assertInt());
        // var to = _addExceptionSpan(
        //     node.to,
        //     () => toNumber
        //         .coerce(fromNumber.numeratorUnits, fromNumber.denominatorUnits)
        //         .assertInt());

        // var direction = from > to ? -1 : 1;
        // if (!node.isExclusive) to += direction;
        // if (from == to) return null;

        // return _environment.scope(() async {
        //   var nodeWithSpan = _expressionNode(node.from);
        //   for (var i = from; i != to; i += direction) {
        //     _environment.setLocalVariable(
        //         node.variable,
        //         SassNumber.withUnits(i,
        //             numeratorUnits: fromNumber.numeratorUnits,
        //             denominatorUnits: fromNumber.denominatorUnits),
        //         nodeWithSpan);
        //     var result = await _handleReturn<Statement>(
        //         node.children, (child) => child.accept(this));
        //     if (result != null) return result;
        //   }
        //   return null;
        // }, semiGlobal: true);
        // todo!()
    }

    fn visit_while_stmt(&mut self, while_stmt: AstWhile) -> SassResult<AstStmtEvalResult> {
        self.with_scope::<SassResult<AstStmtEvalResult>>(
            true,
            while_stmt.has_declarations(),
            |visitor| {
                let mut result = Vec::new();

                while visitor
                    .visit_expr(while_stmt.condition.clone())?
                    .unwrap()
                    .is_true()
                {
                    for stmt in while_stmt.body.clone() {
                        match visitor.visit_stmt(stmt)? {
                            AstStmtEvalResult::Return(val) => {
                                debug_assert!(result.is_empty());
                                return Ok(AstStmtEvalResult::Return(val));
                            }
                            AstStmtEvalResult::Stmts(mut stmts) => result.append(&mut stmts),
                        }
                    }
                }

                Ok(AstStmtEvalResult::Stmts(result))
            },
        )
        // todo!()
        //     return _environment.scope(() async {
        //   while ((await node.condition.accept(this)).isTruthy) {
        //     var result = await _handleReturn<Statement>(
        //         node.children, (child) => child.accept(this));
        //     if (result != null) return result;
        //   }
        //   return null;
        // }, semiGlobal: true, when: node.hasDeclarations);
    }

    fn visit_if_stmt(&mut self, if_stmt: AstIf) -> SassResult<AstStmtEvalResult> {
        let mut clause: Option<Vec<AstStmt>> = if_stmt.else_clause;
        for clause_to_check in if_stmt.if_clauses {
            if self
                .visit_expr(clause_to_check.condition)?
                .unwrap()
                .is_true()
            {
                clause = Some(clause_to_check.body);
                break;
            }
        }

        self.env.scopes.enter_new_scope();

        let stmts = match clause {
            Some(stmts) => {
                let mut result = Vec::new();
                for stmt in stmts {
                    match self.visit_stmt(stmt)? {
                        AstStmtEvalResult::Return(val) => {
                            debug_assert!(result.is_empty());
                            return Ok(AstStmtEvalResult::Return(val));
                        }
                        AstStmtEvalResult::Stmts(mut stmts) => result.append(&mut stmts),
                    }
                }

                AstStmtEvalResult::Stmts(result)
            }
            None => AstStmtEvalResult::Stmts(Vec::new()),
        };

        self.env.scopes.exit_scope();

        Ok(stmts)
    }

    fn visit_loud_comment(&mut self, comment: AstLoudComment) -> SassResult<Vec<Stmt>> {
        if self.flags.in_function() {
            return Ok(Vec::new());
        }

        // todo:
        // // Comments are allowed to appear between CSS imports.
        // if (_parent == _root && _endOfImports == _root.children.length) {
        //   _endOfImports++;
        // }

        Ok(vec![Stmt::Comment(
            self.perform_interpolation(comment.text, false)?,
        )])
    }

    fn visit_variable_decl(&mut self, decl: AstVariableDecl) -> SassResult<Vec<Stmt>> {
        if decl.is_guarded {
            if decl.namespace.is_none() && self.env.at_root() {
                todo!()
                //     if (node.isGuarded) {
                //   if (node.namespace == null && _environment.atRoot) {
                //     var override = _configuration.remove(node.name);
                //     if (override != null && override.value != sassNull) {
                //       _addExceptionSpan(node, () {
                //         _environment.setVariable(
                //             node.name, override.value, override.assignmentNode,
                //             global: true);
                //       });
                //       return null;
                //     }
                //   }
            }

            if self
                .env
                .scopes
                .var_exists(decl.name, self.env.global_scope())
            {
                let value = self
                    .env
                    .scopes
                    .get_var(
                        Spanned {
                            node: decl.name,
                            span: self.parser.span_before,
                        },
                        self.env.global_scope(),
                    )
                    .unwrap();

                if value.deref() != &Value::Null {
                    return Ok(Vec::new());
                }
            }
        }

        if decl.is_global && !self.env.global_scope().borrow().var_exists(decl.name) {
            // todo: deprecation: true
            if self.env.at_root() {
                self.emit_warning(crate::Cow::const_str("As of Dart Sass 2.0.0, !global assignments won't be able to declare new variables.\n\nSince this assignment is at the root of the stylesheet, the !global flag is\nunnecessary and can safely be removed."), decl.span);
            } else {
                self.emit_warning(crate::Cow::const_str("As of Dart Sass 2.0.0, !global assignments won't be able to declare new variables.\n\nRecommendation: add `${node.originalName}: null` at the stylesheet root."), decl.span);
            }
        }

        let value = self.visit_expr(decl.value)?.unwrap();
        let value = self.without_slash(value)?;

        if decl.is_global || self.env.at_root() {
            self.env.global_scope_mut().insert_var(decl.name, value);
        } else {
            // basically, if in_semi_global_scope AND var is global AND not re-declared, insert into last scope
            // why? i don't know
            self.env.scopes.__insert_var(
                decl.name,
                value,
                &&*self.env.global_scope,
                self.flags.in_semi_global_scope(),
            );
        }

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
        return Ok(Vec::new());
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
            result.trim().to_owned()
        } else {
            result
        })
    }

    fn perform_interpolation(
        &mut self,
        interpolation: Interpolation,
        warn_for_color: bool,
    ) -> SassResult<String> {
        let result = interpolation.contents.into_iter().map(|part| match part {
            InterpolationPart::String(s) => Ok(s),
            InterpolationPart::Expr(e) => {
                let result = self.visit_expr(e)?.unwrap();
                self.serialize(result, QuoteKind::None)
            }
        });

        result.collect()
    }

    fn evaluate_to_css(&mut self, expr: AstExpr, quote: QuoteKind) -> SassResult<String> {
        let result = self.visit_expr(expr)?.unwrap();
        self.serialize(result, quote)
    }

    fn without_slash(&mut self, v: Value) -> SassResult<Value> {
        match v {
            Value::Dimension(..) if v.as_slash().is_some() => {
                //   String recommendation(SassNumber number) {
                //     var asSlash = number.asSlash;
                //     if (asSlash != null) {
                //       return "math.div(${recommendation(asSlash.item1)}, "
                //           "${recommendation(asSlash.item2)})";
                //     } else {
                //       return number.toString();
                //     }
                todo!()
            }
            _ => {}
        }

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

        // return value.withoutSlash();
        Ok(v.without_slash())
    }

    fn eval_args(&mut self, arguments: ArgumentInvocation) -> SassResult<ArgumentResult> {
        let mut positional = Vec::new();

        for expr in arguments.positional {
            let val = self.visit_expr(expr)?.unwrap();
            positional.push(self.without_slash(val)?);
        }

        let mut named = BTreeMap::new();

        for (key, expr) in arguments.named {
            let val = self.visit_expr(expr)?.unwrap();
            named.insert(key, self.without_slash(val)?);
        }

        if arguments.rest.is_none() {
            return Ok(ArgumentResult {
                positional,
                named,
                separator: ListSeparator::Undecided,
                span: self.parser.span_before,
                touched: BTreeSet::new(),
            });
        }

        let rest = self.visit_expr(arguments.rest.unwrap())?.unwrap();

        let mut separator = ListSeparator::Undecided;

        match rest {
            Value::Map(rest) => self.add_rest_map(&mut named, rest)?,
            Value::List(elems, list_separator, _) => {
                let mut list = elems
                    .into_iter()
                    .map(|e| self.without_slash(e))
                    .collect::<SassResult<Vec<_>>>()?;
                positional.append(&mut list);
                separator = list_separator;
            }
            Value::ArgList(ArgList {
                elems,
                keywords,
                separator: list_separator,
                ..
            }) => {
                let mut list = elems
                    .into_iter()
                    .map(|e| self.without_slash(e))
                    .collect::<SassResult<Vec<_>>>()?;
                positional.append(&mut list);
                separator = list_separator;

                for (key, value) in keywords {
                    named.insert(key, self.without_slash(value)?);
                }
            }
            _ => {
                positional.push(self.without_slash(rest)?);
            }
        }

        if arguments.keyword_rest.is_none() {
            return Ok(ArgumentResult {
                positional,
                named,
                separator: ListSeparator::Undecided,
                span: self.parser.span_before,
                touched: BTreeSet::new(),
            });
        }

        match self.visit_expr(arguments.keyword_rest.unwrap())?.unwrap() {
            Value::Map(keyword_rest) => {
                self.add_rest_map(&mut named, keyword_rest)?;

                return Ok(ArgumentResult {
                    positional,
                    named,
                    separator,
                    span: self.parser.span_before,
                    touched: BTreeSet::new(),
                });
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
        for (key, val) in rest.into_iter() {
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
        arguments: ArgumentInvocation,
        func: F,
        env: Environment,
        run: impl FnOnce(F, &mut Self) -> SassResult<V>,
    ) -> SassResult<V> {
        let mut evaluated = self.eval_args(arguments)?;

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
                    visitor.env.scopes.insert_var_last(
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
                        .map(|n| Ok(n))
                        .unwrap_or_else(|| {
                            // todo: superfluous clone
                            let v = visitor
                                .visit_expr(argument.default.clone().unwrap())?
                                .unwrap();
                            visitor.without_slash(v)
                        })?;
                    visitor.env.scopes.insert_var_last(name, value);
                }

                let argument_list = if let Some(rest_arg) = func.arguments().rest {
                    let rest = if evaluated.positional.len() > declared_arguments.len() {
                        &evaluated.positional[declared_arguments.len()..]
                    } else {
                        &[]
                    };

                    let arg_list = Value::ArgList(ArgList::new(
                        rest.to_vec(),
                        // todo: superfluous clone
                        evaluated.named.clone(),
                        if evaluated.separator == ListSeparator::Undecided {
                            ListSeparator::Comma
                        } else {
                            ListSeparator::Space
                        },
                    ));

                    // todo: potentially superfluous clone
                    visitor
                        .env
                        .scopes
                        .insert_var_last(rest_arg, arg_list.clone());

                    Some(arg_list)
                } else {
                    None
                };

                let val = run(func, visitor)?;

                if argument_list.is_none() || evaluated.named.is_empty() {
                    return Ok(val);
                }

                //   if (argumentList.wereKeywordsAccessed) return result;

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
                todo!("argument list mutable")
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

    fn run_function_callable(
        &mut self,
        func: SassFunction,
        arguments: ArgumentInvocation,
    ) -> SassResult<Value> {
        match func {
            SassFunction::Builtin(func, name) => {
                let mut evaluated = self.eval_args(arguments)?;
                let val = func.0(evaluated, self)?;
                return self.without_slash(val);
            }
            SassFunction::UserDefined(UserDefinedFunction { function, env, .. }) => self
                .run_user_defined_callable(arguments, *function, env, |function, visitor| {
                    for stmt in function.children {
                        let mut stmts = visitor.visit_stmt(stmt)?;

                        match stmts {
                            AstStmtEvalResult::Stmts(s) => assert!(s.is_empty(), "{:?}", s),
                            AstStmtEvalResult::Return(val) => return Ok(val),
                        }
                    }

                    todo!("Function finished without @return.")
                }),
            SassFunction::Plain { name } => {
                if !arguments.named.is_empty() || arguments.keyword_rest.is_some() {
                    todo!("Plain CSS functions don't support keyword arguments.");
                }

                let mut buffer = format!("{}(", name.as_str());
                let mut first = true;

                for argument in arguments.positional {
                    if first {
                        first = false;
                    } else {
                        buffer.push_str(", ");
                    }

                    buffer.push_str(&self.evaluate_to_css(argument, QuoteKind::Quoted)?);
                }

                if let Some(rest_arg) = arguments.rest {
                    let rest = self.visit_expr(rest_arg)?.unwrap();
                    if !first {
                        buffer.push_str(", ");
                    }
                    buffer.push_str(&self.serialize(rest, QuoteKind::Quoted)?);
                }
                buffer.push(')');

                Ok(Value::String(buffer, QuoteKind::None))
            }
        }
    }

    fn visit_expr(&mut self, expr: AstExpr) -> SassResult<Option<Value>> {
        Ok(Some(match expr {
            AstExpr::Color(color) => Value::Color(color),
            AstExpr::Number { n, unit } => Value::Dimension(Some(n), unit, false),
            AstExpr::List {
                elems,
                separator,
                brackets,
            } => {
                let elems = elems
                    .into_iter()
                    .map(|e| {
                        let span = e.span;
                        let value = self.visit_expr(e.node)?.unwrap();
                        Ok(value)
                    })
                    .collect::<SassResult<Vec<_>>>()?;

                Value::List(elems, separator, brackets)
            }
            AstExpr::String(StringExpr(text, quote)) => self.visit_string(text, quote)?,
            AstExpr::BinaryOp {
                lhs,
                op,
                rhs,
                allows_slash,
            } => self.visit_bin_op(lhs, op, rhs, allows_slash)?,
            AstExpr::True => Value::True,
            AstExpr::False => Value::False,
            AstExpr::Calculation { name, args } => todo!(),
            AstExpr::FunctionRef(_) => todo!(),
            AstExpr::FunctionCall {
                namespace,
                name,
                arguments,
            } => {
                let func = match self.env.scopes.get_fn(name, self.env.global_scope()) {
                    Some(func) => func,
                    None => {
                        if let Some(f) = GLOBAL_FUNCTIONS.get(name.as_str()) {
                            SassFunction::Builtin(f.clone(), name)
                        } else {
                            if namespace.is_some() {
                                todo!("Undefined function.");
                            }

                            SassFunction::Plain { name }
                        }
                    }
                };

                let old_in_function = self.flags.in_function();
                self.flags.set(ContextFlags::IN_FUNCTION, true);
                let value = self.run_function_callable(func, *arguments)?;
                self.flags.set(ContextFlags::IN_FUNCTION, old_in_function);

                value

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
            AstExpr::If(if_expr) => {
                IF_ARGUMENTS().verify(if_expr.0.positional.len(), &if_expr.0.named)?;

                let positional = if_expr.0.positional;
                let named = if_expr.0.named;

                let condition = if positional.len() > 0 {
                    &positional[0]
                } else {
                    named.get(&Identifier::from("condition")).unwrap()
                };

                let if_true = if positional.len() > 1 {
                    &positional[1]
                } else {
                    named.get(&Identifier::from("if_true")).unwrap()
                };

                let if_false = if positional.len() > 2 {
                    &positional[2]
                } else {
                    named.get(&Identifier::from("if_false")).unwrap()
                };

                let value = if self.visit_expr(condition.clone())?.unwrap().is_true() {
                    self.visit_expr(if_true.clone())?.unwrap()
                } else {
                    self.visit_expr(if_false.clone())?.unwrap()
                };

                self.without_slash(value)?
            }
            AstExpr::InterpolatedFunction { name, arguments } => todo!(),
            AstExpr::Map(map) => self.visit_map(map)?,
            AstExpr::Null => Value::Null,
            AstExpr::Paren(expr) => self.visit_expr(*expr)?.unwrap(),
            AstExpr::ParentSelector => match &self.style_rule_ignoring_at_root {
                Some(selector) => selector.as_selector_list().clone().to_sass_list(),
                None => Value::Null,
            },
            AstExpr::UnaryOp(operator, expr) => {
                let operand = self.visit_expr(*expr)?.unwrap();

                let value = match operator {
                    UnaryOp::Plus => operand.unary_plus(self)?,
                    UnaryOp::Neg => operand.unary_neg(self)?,
                    UnaryOp::Div => operand.unary_div(self)?,
                    UnaryOp::Not => operand.unary_not()?,
                };

                value
            }
            AstExpr::Value(_) => todo!(),
            AstExpr::Variable { name, namespace } => {
                if namespace.is_some() {
                    todo!()
                }

                self.env
                    .scopes
                    .get_var(
                        Spanned {
                            node: name,
                            span: self.parser.span_before,
                        },
                        self.env.global_scope(),
                    )?
                    .clone()
            }
        }))
    }

    fn visit_string(&mut self, text: Interpolation, quote: QuoteKind) -> SassResult<Value> {
        // Don't use [performInterpolation] here because we need to get the raw text
        // from strings, rather than the semantic value.
        let old_in_supports_declaration = self.flags.in_supports_declaration();
        self.flags.set(ContextFlags::IN_SUPPORTS_DECLARATION, false);

        let result = text
            .contents
            .into_iter()
            .map(|part| match part {
                InterpolationPart::String(s) => Ok(s),
                InterpolationPart::Expr(e) => match self.visit_expr(e)?.unwrap() {
                    Value::String(s, ..) => Ok(s),
                    e => self.serialize(e, QuoteKind::None),
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
            let key = self.visit_expr(pair.0)?.unwrap();
            let value = self.visit_expr(pair.1)?.unwrap();

            if let Some(old_value) = sass_map.get_ref(&key) {
                todo!("Duplicate key.")
            }

            sass_map.insert(key, value);
        }

        Ok(Value::Map(sass_map))
    }

    fn visit_bin_op(
        &mut self,
        lhs: Box<AstExpr>,
        op: BinaryOp,
        rhs: Box<AstExpr>,
        allows_slash: bool,
    ) -> SassResult<Value> {
        let left = self.visit_expr(*lhs)?.unwrap();

        Ok(match op {
            BinaryOp::SingleEq => {
                let right = self.visit_expr(*rhs)?.unwrap();
                single_eq(left, right, self.parser.options, self.parser.span_before)?
            }
            BinaryOp::Or => {
                if left.is_true() {
                    left
                } else {
                    self.visit_expr(*rhs)?.unwrap()
                }
            }
            BinaryOp::And => {
                if left.is_true() {
                    self.visit_expr(*rhs)?.unwrap()
                } else {
                    left
                }
            }
            BinaryOp::Equal => {
                let right = self.visit_expr(*rhs)?.unwrap();
                Value::bool(left == right)
            }
            BinaryOp::NotEqual => {
                let right = self.visit_expr(*rhs)?.unwrap();
                Value::bool(left != right)
            }
            BinaryOp::GreaterThan
            | BinaryOp::GreaterThanEqual
            | BinaryOp::LessThan
            | BinaryOp::LessThanEqual => {
                let right = self.visit_expr(*rhs)?.unwrap();
                cmp(
                    left,
                    right,
                    self.parser.options,
                    self.parser.span_before,
                    op,
                )?
            }
            BinaryOp::Plus => {
                let right = self.visit_expr(*rhs)?.unwrap();
                add(left, right, self.parser.options, self.parser.span_before)?
            }
            BinaryOp::Minus => {
                let right = self.visit_expr(*rhs)?.unwrap();
                sub(left, right, self.parser.options, self.parser.span_before)?
            }
            BinaryOp::Mul => {
                let right = self.visit_expr(*rhs)?.unwrap();
                mul(left, right, self.parser.options, self.parser.span_before)?
            }
            BinaryOp::Div => {
                let right = self.visit_expr(*rhs)?.unwrap();

                let left_is_number = matches!(left, Value::Dimension(..));
                let right_is_number = matches!(right, Value::Dimension(..));

                let result = div(left, right, self.parser.options, self.parser.span_before)?;

                if left_is_number && right_is_number && allows_slash {
                    //     return (result as SassNumber).withSlash(left, right);
                    todo!()
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
                    todo!()
                }

                result
            }
            BinaryOp::Rem => {
                let right = self.visit_expr(*rhs)?.unwrap();
                rem(left, right, self.parser.options, self.parser.span_before)?
            }
        })
    }

    fn visit_color(&mut self, color: Color) -> SassResult<Value> {
        Ok(Value::Color(Box::new(color)))
    }

    // todo: superfluous clone and non-use of cow
    fn serialize(&mut self, mut expr: Value, quote: QuoteKind) -> SassResult<String> {
        if quote == QuoteKind::None {
            expr = expr.unquote();
        }

        Ok(expr
            .to_css_string(self.parser.span_before, self.parser.options.is_compressed())?
            .into_owned())
    }

    pub fn visit_ruleset(&mut self, ruleset: AstRuleSet) -> SassResult<Vec<Stmt>> {
        // NOTE: this logic is largely duplicated in [visitCssStyleRule]. Most
        // changes here should be mirrored there.

        if self.declaration_name.is_some() {
            todo!("Style rules may not be used within nested declarations.")
        }

        let AstRuleSet {
            selector: ruleset_selector,
            body: ruleset_body,
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
                scopes: self.parser.scopes,
                // global_scope: self.parser.global_scope,
                // super_selectors: self.parser.super_selectors,
                span_before: self.parser.span_before,
                content: self.parser.content,
                flags: self.parser.flags,
                at_root: self.parser.at_root,
                at_root_has_selector: self.parser.at_root_has_selector,
                // extender: self.parser.extender,
                content_scopes: self.parser.content_scopes,
                options: self.parser.options,
                modules: self.parser.modules,
                module_config: self.parser.module_config,
            })
            .parse_keyframes_selector()?;

            let keyframes_ruleset = Stmt::KeyframesRuleSet(Box::new(KeyframesRuleSet {
                selector: parsed_selector,
                body: Vec::new(),
            }));

            let parent_idx = self.css_tree.add_stmt(keyframes_ruleset, self.parent);

            let body = self.with_parent::<SassResult<()>>(parent_idx, false, true, |visitor| {
                for child in ruleset_body {
                    match visitor.visit_stmt(child)? {
                        AstStmtEvalResult::Return(..) => unreachable!(),
                        AstStmtEvalResult::Stmts(mut stmts) => unreachable!(), //result.append(&mut stmts),
                    }
                }

                Ok(())
            })?;

            return Ok(Vec::new());
            // return Ok(vec![Stmt::KeyframesRuleSet(Box::new(KeyframesRuleSet {
            //     selector: parsed_selector,
            //     body,
            // }))]);
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
                scopes: self.parser.scopes,
                // global_scope: self.parser.global_scope,
                // super_selectors: self.parser.super_selectors,
                span_before: self.parser.span_before,
                content: self.parser.content,
                flags: self.parser.flags,
                at_root: self.parser.at_root,
                at_root_has_selector: self.parser.at_root_has_selector,
                // extender: self.parser.extender,
                content_scopes: self.parser.content_scopes,
                options: self.parser.options,
                modules: self.parser.modules,
                module_config: self.parser.module_config,
            },
            !self.flags.in_plain_css(),
            !self.flags.in_plain_css(),
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

        let result = self.with_scope::<SassResult<Stmt>>(false, true, |visitor| {
            let selector = visitor.extender.add_selector(parsed_selector, None);

            let old_at_root_excluding_style_rule = visitor.flags.at_root_excluding_style_rule();

            visitor
                .flags
                .set(ContextFlags::AT_ROOT_EXCLUDING_STYLE_RULE, false);

            let mut body = Vec::new();

            let old = visitor.style_rule_ignoring_at_root.take();
            visitor.style_rule_ignoring_at_root = Some(selector);

            for child in ruleset_body {
                match visitor.visit_stmt(child)? {
                    AstStmtEvalResult::Return(..) => unreachable!(),
                    AstStmtEvalResult::Stmts(mut stmts) => body.append(&mut stmts),
                }
            }

            let selector = visitor.style_rule_ignoring_at_root.take().unwrap();
            visitor.style_rule_ignoring_at_root = old;

            visitor.flags.set(
                ContextFlags::AT_ROOT_EXCLUDING_STYLE_RULE,
                old_at_root_excluding_style_rule,
            );

            Ok(Stmt::RuleSet { selector, body })
        })?;

        Ok(vec![result])
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

    // todo: early exit if blank
    pub fn visit_style(&mut self, style: AstStyle) -> SassResult<Vec<Stmt>> {
        if !self.style_rule_exists()
            && !self.flags.in_unknown_at_rule()
            && !self.flags.in_keyframes()
        {
            todo!("Declarations may only be used within style rules.")
        }

        let mut name = self.interpolation_to_value(style.name, false, true)?;

        if let Some(declaration_name) = &self.declaration_name {
            name = format!("{}-{}", declaration_name, name);
        }

        let value = self.visit_expr(style.value.unwrap())?;

        // If the value is an empty list, preserve it, because converting it to CSS
        // will throw an error that we want the user to see.
        match value {
            // Some(v) if !v.is_blank() || v.is_empty_list() => {

            // }
            Some(v) if name.starts_with("--") => {
                todo!("Custom property values may not be empty.")
            }
            _ => {}
        }

        let children = style.body;

        if children.len() > 0 {
            let old_declaration_name = self.declaration_name.take();
            self.declaration_name = Some(name);
            for child in children {
                todo!()
            }
            name = self.declaration_name.take().unwrap();
            self.declaration_name = old_declaration_name;
        }

        Ok(vec![Stmt::Style(Style {
            property: InternedString::get_or_intern(name),
            value: Box::new(value.unwrap().span(self.parser.span_before)),
        })])

        //         Future<Value?> visitDeclaration(Declaration node) async {
        //     if (_styleRule == null && !_inUnknownAtRule && !_inKeyframes) {
        //       throw _exception(
        //           "Declarations may only be used within style rules.", node.span);
        //     }

        //     var name = await _interpolationToValue(node.name, warnForColor: true);
        //     if (_declarationName != null) {
        //       name = CssValue("$_declarationName-${name.value}", name.span);
        //     }
        //     var cssValue = await node.value.andThen(
        //         (value) async => CssValue(await value.accept(this), value.span));

        //     // If the value is an empty list, preserve it, because converting it to CSS
        //     // will throw an error that we want the user to see.
        //     if (cssValue != null &&
        //         (!cssValue.value.isBlank || _isEmptyList(cssValue.value))) {
        //       _parent.addChild(ModifiableCssDeclaration(name, cssValue, node.span,
        //           parsedAsCustomProperty: node.isCustomProperty,
        //           valueSpanForMap:
        //               _sourceMap ? node.value.andThen(_expressionNode)?.span : null));
        //     } else if (name.value.startsWith('--') && cssValue != null) {
        //       throw _exception(
        //           "Custom property values may not be empty.", cssValue.span);
        //     }

        //     var children = node.children;
        //     if (children != null) {
        //       var oldDeclarationName = _declarationName;
        //       _declarationName = name.value;
        //       await _environment.scope(() async {
        //         for (var child in children) {
        //           await child.accept(this);
        //         }
        //       }, when: node.hasDeclarations);
        //       _declarationName = oldDeclarationName;
        //     }

        //     return null;
        //   }
        // todo!()
    }
}
