use crate::parse::Stmt;

#[derive(Debug, Clone)]
pub(crate) struct SupportsRule {
    pub params: String,
    pub body: Vec<Stmt>,
}
