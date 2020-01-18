use crate::common::Scope;
use crate::{Stmt, StyleSheet};
use std::path::Path;

pub fn import<P: AsRef<Path>>(name: P) -> (Vec<Stmt>, Scope) {
    let mut rules: Vec<Stmt> = Vec::new();
    let mut scope = Scope::new();
    let path = name.as_ref().to_path_buf();
    let name = path.file_name().unwrap();
    if path.extension() == Some(std::ffi::OsStr::new(".css")) {// || name.starts_with("http://") || name.starts_with("https://") {
        todo!("handle css imports")
    }
    let mut p1 = path.clone();
    p1.push("/index.scss");
    let mut p2 = path.clone();
    p2.push("/_index.scss");
    let paths = [
        path.with_file_name(format!("{}.scss", name.to_str().unwrap())),
        path.with_file_name(format!("_{}.scss", name.to_str().unwrap())),
        path,
        p1,
        p2,
    ];
    for name in &paths {
        if name.is_file() {
            let (rules2, scope2) = StyleSheet::export_from_path(name.to_str().unwrap()).unwrap();
            rules.extend(rules2);
            scope.merge(scope2);
        }
    }
    (rules, scope)
}
