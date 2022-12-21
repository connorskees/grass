use crate::parse::Stmt;

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
