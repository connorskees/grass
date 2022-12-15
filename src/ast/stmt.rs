use std::collections::HashSet;

use codemap::{Span, Spanned};

use crate::{
    ast::Interpolation,
    ast::{ArgumentDeclaration, ArgumentInvocation, AstExpr},
    atrule::media::MediaQuery,
    common::Identifier,
    parse::Stmt,
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
    pub namespace: Option<Identifier>,
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
    pub namespace: Option<Identifier>,
    pub name: Spanned<Identifier>,
    pub args: ArgumentInvocation,
    pub content: Option<AstContentBlock>,
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
            Stmt::UnknownAtRule(rule) => self.excludes_name(&rule.name.to_ascii_lowercase()),
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
}

#[derive(Debug, Clone)]
pub(crate) struct AstUseRule {}

#[derive(Debug, Clone)]
pub(crate) struct AstForwardRule {}

#[derive(Debug, Clone)]
pub(crate) struct StyleSheet {
    pub body: Vec<AstStmt>,
    pub is_plain_css: bool,
    pub uses: Vec<AstUseRule>,
    pub forwards: Vec<AstForwardRule>,
}

impl StyleSheet {
    pub fn new() -> Self {
        Self {
            body: Vec::new(),
            is_plain_css: false,
            uses: Vec::new(),
            forwards: Vec::new(),
        }
    }
}
