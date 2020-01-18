use crate::common::Scope;
use crate::{Stmt, StyleSheet};
use std::path::Path;

pub fn import(name: String) -> (Vec<Stmt>, Scope) {
    let mut rules: Vec<Stmt> = Vec::new();
    let mut scope = Scope::new();
    for name in &[
        &name,
        &format!("{}.scss", name),
        &format!("_{}.scss", name),
        &format!("{}/index.scss", name),
        &format!("{}/_index.scss", name),
        &format!("{}.css", name),
    ] {
        let p = Path::new(&name);
        if p.exists() && p.is_file() {
            let (rules2, scope2) = StyleSheet::export_from_path(*name).unwrap();
            rules.extend(rules2);
            scope.merge(scope2);
        }
    }
    (rules, scope)
}
