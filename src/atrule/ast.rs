use crate::{
    args::CallArgs,
    atrule::{
        keyframes::Keyframes, media::MediaRule, mixin::UserDefinedMixin, SupportsRule,
        UnknownAtRule,
    },
    common::Identifier,
};

#[derive(Debug, Clone)]
pub(crate) enum Interpolation {
    /// There is no interpolation done
    ///
    /// This is an optimization for the common case
    Literal(String),

    /// Interpolation occurred
    Interpolated(Vec<InterpolationPart>),
}

#[derive(Debug, Clone)]
pub(crate) enum InterpolationPart {
    Literal(String),
    Interpolated(AstValue),
}

#[derive(Debug, Clone)]
pub(crate) struct Style {
    property: Interpolation,
    value: AstValue,
}

#[derive(Debug, Clone)]
pub(crate) enum AstNode {
    RuleSet {
        selector: Interpolation,
        body: Vec<Self>,
    },
    Style(Style),

    Propset,
    Mixin(UserDefinedMixin),
    Content,
    Function,
    Return(Box<AstValue>),
    AtRoot {
        body: Vec<Self>,
    },
    VariableDeclaration {
        name: Identifier,
        value: AstValue,
        default: bool,
        global: bool,
    },
    Include {
        module: Option<Identifier>,
        name: Identifier,
        args: CallArgs,
    },

    // Special CSS at-rules
    Media(Box<MediaRule>),
    Supports(Box<SupportsRule>),
    Keyframes(Box<Keyframes>),
    // KeyframesRuleSet(Box<KeyframesRuleSet>),
    UnknownAtRule(Box<UnknownAtRule>),

    /// A plain import such as `@import "foo.css";` or
    /// `@import url(https://fonts.google.com/foo?bar);`
    CssImport(String),
    SassImport(String),
    Comment(Interpolation),

    // Control flow
    Each(Each),
    If(If),
    For(For),
    While(While),
}

#[derive(Debug, Clone)]
pub(crate) enum AstValue {
    Variable(Identifier),
    FunctionCall(Identifier, CallArgs),

    /// The parent selector, `&`
    ParentSelector,
}

#[derive(Debug, Clone)]
pub(crate) struct For {
    var: Identifier,
    from: i32,
    to: i32,
    /// whether or not `@for` was declared with `through`
    through: bool,
    body: Vec<AstNode>,
}

#[derive(Debug, Clone)]
pub(crate) struct While {
    cond: AstValue,
    body: Vec<AstNode>,
}

#[derive(Debug, Clone)]
pub(crate) struct Each {
    vars: Vec<Identifier>,
    value: AstValue,
    body: Vec<AstNode>,
}

#[derive(Debug, Clone)]
pub(crate) struct If {
    cond: AstValue,
    body: Vec<AstNode>,
    branches: Vec<Branch>,
    else_body: Vec<AstNode>,
}

#[derive(Debug, Clone)]
pub(crate) struct Branch {
    cond: AstValue,
    body: Vec<AstNode>,
}
