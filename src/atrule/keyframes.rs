use crate::parse::Stmt;

#[derive(Debug, Clone)]
pub(crate) struct Keyframes {
    /// `@keyframes` can contain a browser prefix,
    /// e.g. `@-webkit-keyframes { ... }`, and therefore
    /// we cannot be certain of the name of the at-rule
    pub rule: String,
    pub name: String,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub(crate) struct KeyframesRuleSet {
    pub selector: Vec<KeyframesSelector>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub(crate) enum KeyframesSelector {
    To,
    From,
    Percent(Box<str>),
}
