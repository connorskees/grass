use std::ffi::{OsStr, OsString};
use std::path::Path;

use codemap::{CodeMap, Spanned};

use crate::error::SassResult;
use crate::scope::Scope;
use crate::{Stmt, StyleSheet};

pub(crate) fn import(
    ctx: &Path,
    path: &Path,
    map: &mut CodeMap,
) -> SassResult<(Vec<Spanned<Stmt>>, Scope)> {
    let mut rules = Vec::new();
    let mut scope = Scope::new();
    if path.is_absolute() {
        todo!("absolute import")
    }
    let path_buf = ctx.parent().unwrap_or_else(|| Path::new("")).join(path);
    // "todo: will panic if path ended in `..`"
    let name = path_buf.file_name().unwrap();
    if path_buf.extension() == Some(OsStr::new(".css")) {
        // || name.starts_with("http://") || name.starts_with("https://") {
        todo!("handle css imports")
    }
    let mut p1 = path_buf.clone();
    p1.push(OsString::from("index.scss"));
    let mut p2 = path_buf.clone();
    p2.push(OsString::from("_index.scss"));
    let paths = [
        path_buf.with_file_name(name).with_extension("scss"),
        path_buf.with_file_name(format!("_{}.scss", name.to_str().unwrap())),
        path_buf,
        p1,
        p2,
    ];
    for name in &paths {
        if name.is_file() {
            let (rules2, scope2) = StyleSheet::export_from_path(&name.to_str().unwrap(), map)?;
            rules.extend(rules2);
            scope.extend(scope2);
        }
    }
    Ok((rules, scope))
}
