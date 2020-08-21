use crate::{parse::AstNode, selector::Selector};

#[derive(Debug, Clone)]
pub(crate) struct UnknownAtRule {
    pub name: String,
    pub super_selector: Selector,
    pub params: String,
    pub body: Vec<AstNode>,
}
