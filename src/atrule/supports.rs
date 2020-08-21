use crate::parse::AstNode;

#[derive(Debug, Clone)]
pub(crate) struct SupportsRule {
    pub params: String,
    pub body: Vec<AstNode>,
}
