use std::{ffi::OsStr, fs, path::Path, path::PathBuf};

use codemap::Spanned;
use peekmore::PeekMore;

use crate::{common::QuoteKind, error::SassResult, lexer::Lexer, value::Value, Token};

use super::{Parser, Stmt};

/// Searches the current directory of the file then searches in load paths directories
/// if the import has not yet been found.
/// <https://sass-lang.com/documentation/at-rules/import#finding-the-file>
/// <https://sass-lang.com/documentation/at-rules/import#load-paths>
fn find_import(file_path: &PathBuf, name: &OsStr, load_paths: &[&Path]) -> Option<PathBuf> {
    let paths = [
        file_path.with_file_name(name).with_extension("scss"),
        file_path
            .with_file_name(format!("_{}", name.to_str().unwrap()))
            .with_extension("scss"),
        file_path.clone(),
        file_path.join("index.scss"),
        file_path.join("_index.scss"),
    ];

    for name in &paths {
        if name.is_file() {
            return Some(name.to_path_buf());
        }
    }

    for path in load_paths {
        let paths: Vec<PathBuf> = if path.is_dir() {
            vec![
                path.join(format!("{}.scss", name.to_str().unwrap())),
                path.join(format!("_{}.scss", name.to_str().unwrap())),
                path.join("index.scss"),
                path.join("_index.scss"),
            ]
        } else {
            vec![
                path.to_path_buf(),
                path.with_file_name(name).with_extension("scss"),
                path.with_file_name(format!("_{}", name.to_str().unwrap()))
                    .with_extension("scss"),
                path.join("index.scss"),
                path.join("_index.scss"),
            ]
        };

        for name in paths {
            if name.is_file() {
                return Some(name);
            }
        }
    }

    None
}

impl<'a> Parser<'a> {
    pub(super) fn import(&mut self) -> SassResult<Vec<Stmt>> {
        self.whitespace();

        match self.toks.peek() {
            Some(Token { kind: '\'', .. })
            | Some(Token { kind: '"', .. })
            | Some(Token { kind: 'u', .. }) => {}
            Some(Token { pos, .. }) => return Err(("Expected string.", *pos).into()),
            None => return Err(("expected more input.", self.span_before).into()),
        };

        let Spanned {
            node: file_name_as_value,
            span,
        } = self.parse_value(true)?;
        let file_name = match file_name_as_value {
            Value::String(s, QuoteKind::Quoted) => {
                if s.ends_with(".css") || s.starts_with("http://") || s.starts_with("https://") {
                    return Ok(vec![Stmt::Import(format!("\"{}\"", s))]);
                } else {
                    s
                }
            }
            Value::String(s, QuoteKind::None) => {
                if s.starts_with("url(") {
                    return Ok(vec![Stmt::Import(s)]);
                } else {
                    s
                }
            }
            _ => return Err(("Expected string.", span).into()),
        };

        self.whitespace();

        let path: &Path = file_name.as_ref();

        let path_buf = if path.is_absolute() {
            // todo: test for absolute path imports
            path.into()
        } else {
            self.path
                .parent()
                .unwrap_or_else(|| Path::new(""))
                .join(path)
        };

        let name = path_buf.file_name().unwrap_or_else(|| OsStr::new(".."));

        if let Some(name) = find_import(&path_buf, name, self.load_paths) {
            let file = self.map.add_file(
                name.to_string_lossy().into(),
                String::from_utf8(fs::read(&name)?)?,
            );

            return Parser {
                toks: &mut Lexer::new(&file)
                    .collect::<Vec<Token>>()
                    .into_iter()
                    .peekmore(),
                map: self.map,
                path: &name,
                scopes: self.scopes,
                global_scope: self.global_scope,
                super_selectors: self.super_selectors,
                span_before: file.span.subspan(0, 0),
                content: self.content,
                flags: self.flags,
                at_root: self.at_root,
                at_root_has_selector: self.at_root_has_selector,
                extender: self.extender,
                content_scopes: self.content_scopes,
                load_paths: self.load_paths,
            }
            .parse();
        }

        Err(("Can't find stylesheet to import.", span).into())
    }
}
