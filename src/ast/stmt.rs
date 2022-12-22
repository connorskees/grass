use std::{
    cell::RefCell,
    collections::{BTreeMap, HashSet},
    path::PathBuf,
    sync::Arc,
};

use codemap::{Span, Spanned};

use crate::{
    ast::Interpolation,
    ast::{ArgumentDeclaration, ArgumentInvocation, AstExpr},
    atrule::media::MediaQuery,
    common::Identifier,
    parse::Stmt,
    utils::{BaseMapView, LimitedMapView, MapView, UnprefixedMapView},
    value::Value,
};

#[derive(Debug, Clone)]
pub(crate) struct AstSilentComment {
    pub text: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct AstPlainCssImport {
    pub url: Interpolation,
    pub modifiers: Option<Interpolation>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct AstSassImport {
    pub url: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct AstIf {
    pub if_clauses: Vec<AstIfClause>,
    pub else_clause: Option<Vec<AstStmt>>,
}

#[derive(Debug, Clone)]
pub(crate) struct AstIfClause {
    pub condition: AstExpr,
    pub body: Vec<AstStmt>,
}

#[derive(Debug, Clone)]
pub(crate) struct AstFor {
    pub variable: Spanned<Identifier>,
    pub from: Spanned<AstExpr>,
    pub to: Spanned<AstExpr>,
    pub is_exclusive: bool,
    pub body: Vec<AstStmt>,
}

#[derive(Debug, Clone)]
pub(crate) struct AstReturn {
    pub val: AstExpr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct AstRuleSet {
    pub selector: Interpolation,
    pub body: Vec<AstStmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct AstStyle {
    pub name: Interpolation,
    pub value: Option<Spanned<AstExpr>>,
    pub body: Vec<AstStmt>,
    pub span: Span,
}

impl AstStyle {
    pub fn is_custom_property(&self) -> bool {
        self.name.initial_plain().starts_with("--")
    }
}

#[derive(Debug, Clone)]
pub(crate) struct AstEach {
    pub variables: Vec<Identifier>,
    pub list: AstExpr,
    pub body: Vec<AstStmt>,
}

#[derive(Debug, Clone)]
pub(crate) struct AstMedia {
    pub query: Interpolation,
    pub body: Vec<AstStmt>,
}

pub(crate) type CssMediaQuery = MediaQuery;

#[derive(Debug, Clone)]
pub(crate) struct AstWhile {
    pub condition: AstExpr,
    pub body: Vec<AstStmt>,
}

impl AstWhile {
    pub fn has_declarations(&self) -> bool {
        self.body.iter().any(|child| {
            matches!(
                child,
                AstStmt::VariableDecl(..)
                    | AstStmt::FunctionDecl(..)
                    | AstStmt::Mixin(..)
                    // todo: read imports in this case (only counts if dynamic)
                    | AstStmt::ImportRule(..)
            )
        })
    }
}

#[derive(Debug, Clone)]
pub(crate) struct AstVariableDecl {
    pub namespace: Option<Spanned<Identifier>>,
    pub name: Identifier,
    pub value: AstExpr,
    pub is_guarded: bool,
    pub is_global: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct AstFunctionDecl {
    pub name: Spanned<Identifier>,
    pub arguments: ArgumentDeclaration,
    pub children: Vec<AstStmt>,
}

#[derive(Debug, Clone)]
pub(crate) struct AstDebugRule {
    pub value: AstExpr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct AstWarn {
    pub value: AstExpr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct AstErrorRule {
    pub value: AstExpr,
    pub span: Span,
}

impl PartialEq for AstFunctionDecl {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for AstFunctionDecl {}

#[derive(Debug, Clone)]
pub(crate) struct AstLoudComment {
    pub text: Interpolation,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct AstMixin {
    pub name: Identifier,
    pub args: ArgumentDeclaration,
    pub body: Vec<AstStmt>,
    /// Whether the mixin contains a `@content` rule.
    pub has_content: bool,
}

#[derive(Debug, Clone)]
pub(crate) struct AstContentRule {
    pub args: ArgumentInvocation,
}

#[derive(Debug, Clone)]
pub(crate) struct AstContentBlock {
    pub args: ArgumentDeclaration,
    pub body: Vec<AstStmt>,
}

#[derive(Debug, Clone)]
pub(crate) struct AstInclude {
    pub namespace: Option<Spanned<Identifier>>,
    pub name: Spanned<Identifier>,
    pub args: ArgumentInvocation,
    pub content: Option<AstContentBlock>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct AstUnknownAtRule {
    pub name: Interpolation,
    pub value: Option<Interpolation>,
    pub children: Option<Vec<AstStmt>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct AstExtendRule {
    pub value: Interpolation,
    pub is_optional: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct AstAtRootRule {
    pub children: Vec<AstStmt>,
    pub query: Option<Interpolation>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct AtRootQuery {
    pub include: bool,
    pub names: HashSet<String>,
    pub all: bool,
    pub rule: bool,
}

impl AtRootQuery {
    pub fn new(include: bool, names: HashSet<String>) -> Self {
        let all = names.contains("all");
        let rule = names.contains("rule");

        Self {
            include,
            names,
            all,
            rule,
        }
    }

    pub fn excludes_name(&self, name: &str) -> bool {
        (self.all || self.names.contains(name)) != self.include
    }

    pub fn excludes_style_rules(&self) -> bool {
        (self.all || self.rule) != self.include
    }

    pub fn excludes(&self, stmt: &Stmt) -> bool {
        if self.all {
            return !self.include;
        }

        match stmt {
            Stmt::RuleSet { .. } => self.excludes_style_rules(),
            Stmt::Media(..) => self.excludes_name("media"),
            Stmt::Supports(..) => self.excludes_name("supports"),
            Stmt::UnknownAtRule(rule, ..) => self.excludes_name(&rule.name.to_ascii_lowercase()),
            _ => false,
        }
    }
}

impl Default for AtRootQuery {
    fn default() -> Self {
        Self {
            include: false,
            names: HashSet::new(),
            all: false,
            rule: true,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct AstImportRule {
    pub imports: Vec<AstImport>,
}

#[derive(Debug, Clone)]
pub(crate) enum AstImport {
    Plain(AstPlainCssImport),
    Sass(AstSassImport),
}

impl AstImport {
    pub fn is_dynamic(&self) -> bool {
        matches!(self, AstImport::Sass(..))
    }
}

#[derive(Debug, Clone)]
pub(crate) struct AstUseRule {
    pub url: PathBuf,
    pub namespace: Option<String>,
    pub configuration: Vec<ConfiguredVariable>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct ConfiguredVariable {
    pub name: Spanned<Identifier>,
    pub expr: Spanned<AstExpr>,
    pub is_guarded: bool,
}

#[derive(Debug, Clone)]
pub(crate) struct Configuration {
    pub values: Arc<dyn MapView<Value = ConfiguredValue>>,
    pub original_config: Option<Arc<RefCell<Self>>>,
    pub span: Option<Span>,
}

impl Configuration {
    pub fn through_forward(
        config: Arc<RefCell<Self>>,
        forward: &AstForwardRule,
    ) -> Arc<RefCell<Self>> {
        if (*config).borrow().is_empty() {
            return Arc::new(RefCell::new(Configuration::empty()));
        }

        let mut new_values = Arc::clone(&(*config).borrow().values);

        // Only allow variables that are visible through the `@forward` to be
        // configured. These views support [Map.remove] so we can mark when a
        // configuration variable is used by removing it even when the underlying
        // map is wrapped.
        if let Some(prefix) = &forward.prefix {
            new_values = Arc::new(UnprefixedMapView(new_values, prefix.clone()));
        }

        if let Some(shown_variables) = &forward.shown_variables {
            new_values = Arc::new(LimitedMapView::safelist(new_values, shown_variables));
        } else if let Some(hidden_variables) = &forward.hidden_variables {
            new_values = Arc::new(LimitedMapView::blocklist(new_values, hidden_variables));
        }

        Arc::new(RefCell::new(Self::with_values(
            config,
            Arc::clone(&new_values),
        )))
    }

    fn with_values(
        config: Arc<RefCell<Self>>,
        values: Arc<dyn MapView<Value = ConfiguredValue>>,
    ) -> Self {
        Self {
            values,
            original_config: Some(config),
            span: None,
        }
    }

    pub fn first(&self) -> Option<Spanned<Identifier>> {
        let name = *self.values.keys().get(0)?;
        let value = self.values.get(name)?;

        Some(Spanned {
            node: name,
            span: value.configuration_span?,
        })
    }

    pub fn remove(&mut self, name: Identifier) -> Option<ConfiguredValue> {
        self.values.remove(name)
    }

    pub fn is_implicit(&self) -> bool {
        self.span.is_none()
    }

    pub fn implicit(values: BTreeMap<Identifier, ConfiguredValue>) -> Self {
        Self {
            values: Arc::new(BaseMapView(Arc::new(RefCell::new(values)))),
            original_config: None,
            span: None,
        }
    }

    pub fn explicit(values: BTreeMap<Identifier, ConfiguredValue>, span: Span) -> Self {
        Self {
            values: Arc::new(BaseMapView(Arc::new(RefCell::new(values)))),
            original_config: None,
            span: Some(span),
        }
    }

    pub fn empty() -> Self {
        Self {
            values: Arc::new(BaseMapView(Arc::new(RefCell::new(BTreeMap::new())))),
            original_config: None,
            span: None,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    pub fn original_config(config: Arc<RefCell<Configuration>>) -> Arc<RefCell<Configuration>> {
        match (*config).borrow().original_config.as_ref() {
            Some(v) => Arc::clone(v),
            None => Arc::clone(&config),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ConfiguredValue {
    pub value: Value,
    pub configuration_span: Option<Span>,
}

impl ConfiguredValue {
    pub fn explicit(value: Value, configuration_span: Span) -> Self {
        Self {
            value,
            configuration_span: Some(configuration_span),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct AstForwardRule {
    pub url: PathBuf,
    pub shown_mixins_and_functions: Option<HashSet<Identifier>>,
    pub shown_variables: Option<HashSet<Identifier>>,
    pub hidden_mixins_and_functions: Option<HashSet<Identifier>>,
    pub hidden_variables: Option<HashSet<Identifier>>,
    pub prefix: Option<String>,
    pub configuration: Vec<ConfiguredVariable>,
}

impl AstForwardRule {
    pub fn new(
        url: PathBuf,
        prefix: Option<String>,
        configuration: Option<Vec<ConfiguredVariable>>,
    ) -> Self {
        Self {
            url,
            shown_mixins_and_functions: None,
            shown_variables: None,
            hidden_mixins_and_functions: None,
            hidden_variables: None,
            prefix,
            configuration: configuration.unwrap_or_default(),
        }
    }

    pub fn show(
        url: PathBuf,
        shown_mixins_and_functions: HashSet<Identifier>,
        shown_variables: HashSet<Identifier>,
        prefix: Option<String>,
        configuration: Option<Vec<ConfiguredVariable>>,
    ) -> Self {
        Self {
            url,
            shown_mixins_and_functions: Some(shown_mixins_and_functions),
            shown_variables: Some(shown_variables),
            hidden_mixins_and_functions: None,
            hidden_variables: None,
            prefix,
            configuration: configuration.unwrap_or_default(),
        }
    }

    pub fn hide(
        url: PathBuf,
        hidden_mixins_and_functions: HashSet<Identifier>,
        hidden_variables: HashSet<Identifier>,
        prefix: Option<String>,
        configuration: Option<Vec<ConfiguredVariable>>,
    ) -> Self {
        Self {
            url,
            shown_mixins_and_functions: None,
            shown_variables: None,
            hidden_mixins_and_functions: Some(hidden_mixins_and_functions),
            hidden_variables: Some(hidden_variables),
            prefix,
            configuration: configuration.unwrap_or_default(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum AstSupportsCondition {
    Anything {
        contents: Interpolation,
    },
    Declaration {
        name: AstExpr,
        value: AstExpr,
    },
    Function {
        name: Interpolation,
        args: Interpolation,
    },
    Interpolation(AstExpr),
    Negation(Box<Self>),
    Operation {
        left: Box<Self>,
        operator: Option<String>,
        right: Box<Self>,
    },
}

#[derive(Debug, Clone)]
pub(crate) struct AstSupportsRule {
    pub condition: AstSupportsCondition,
    pub children: Vec<AstStmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) enum AstStmt {
    If(AstIf),
    For(AstFor),
    Return(AstReturn),
    RuleSet(AstRuleSet),
    Style(AstStyle),
    Each(AstEach),
    Media(AstMedia),
    Include(AstInclude),
    While(AstWhile),
    VariableDecl(AstVariableDecl),
    LoudComment(AstLoudComment),
    SilentComment(AstSilentComment),
    FunctionDecl(AstFunctionDecl),
    Mixin(AstMixin),
    ContentRule(AstContentRule),
    Warn(AstWarn),
    UnknownAtRule(AstUnknownAtRule),
    ErrorRule(AstErrorRule),
    Extend(AstExtendRule),
    AtRootRule(AstAtRootRule),
    Debug(AstDebugRule),
    ImportRule(AstImportRule),
    Use(AstUseRule),
    Forward(AstForwardRule),
    Supports(AstSupportsRule),
}

#[derive(Debug, Clone)]
pub(crate) struct StyleSheet {
    pub body: Vec<AstStmt>,
    pub url: PathBuf,
    pub is_plain_css: bool,
    pub uses: Vec<AstUseRule>,
    pub forwards: Vec<AstForwardRule>,
}

impl StyleSheet {
    pub fn new(is_plain_css: bool, url: PathBuf) -> Self {
        Self {
            body: Vec::new(),
            url,
            is_plain_css,
            uses: Vec::new(),
            forwards: Vec::new(),
        }
    }
}
