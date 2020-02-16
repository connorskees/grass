use crate::common::Scope;
use crate::error::SassResult;
use crate::{Stmt, StyleSheet};
use std::ffi::OsStr;
use std::path::Path;

pub(crate) fn import<P: AsRef<Path>>(path: P) -> SassResult<(Vec<Stmt>, Scope)> {
    let mut rules: Vec<Stmt> = Vec::new();
    let mut scope = Scope::new();
    let path_buf = path.as_ref().to_path_buf();
    let name = path_buf.file_name().expect("todo! path ended in `..`");
    if path_buf.extension() == Some(OsStr::new(".css")) {
        // || name.starts_with("http://") || name.starts_with("https://") {
        todo!("handle css imports")
    }
    let mut p1 = path_buf.clone();
    p1.push("index.scss");
    let mut p2 = path_buf.clone();
    p2.push("_index.scss");
    let paths = [
        path_buf.with_file_name(format!(
            "{}.scss",
            name.to_str().expect("path should be UTF-8")
        )),
        path_buf.with_file_name(format!(
            "_{}.scss",
            name.to_str().expect("path should be UTF-8")
        )),
        path_buf,
        p1,
        p2,
    ];
    for name in &paths {
        if name.is_file() {
            let (rules2, scope2) =
                StyleSheet::export_from_path(name.to_str().expect("path should be UTF-8"))?;
            rules.extend(rules2);
            scope.extend(scope2);
        }
    }
    Ok((rules, scope))
}
