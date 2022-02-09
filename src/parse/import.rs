use std::{ffi::OsStr, path::Path, path::PathBuf};

use codemap::{Span, Spanned};

use crate::{
    common::{ListSeparator::Comma, QuoteKind},
    error::SassResult,
    lexer::Lexer,
    value::Value,
    Token,
};

use super::{Parser, Stmt};

#[allow(clippy::case_sensitive_file_extension_comparisons)]
fn is_plain_css_import(url: &str) -> bool {
    if url.len() < 5 {
        return false;
    }

    let lower = url.to_ascii_lowercase();

    lower.ends_with(".css")
        || lower.starts_with("http://")
        || lower.starts_with("https://")
        || lower.starts_with("//")
}

impl<'a, 'b> Parser<'a, 'b> {
    /// Searches the current directory of the file then searches in `load_paths` directories
    /// if the import has not yet been found.
    ///
    /// <https://sass-lang.com/documentation/at-rules/import#finding-the-file>
    /// <https://sass-lang.com/documentation/at-rules/import#load-paths>
    pub(super) fn find_import(&self, path: &Path) -> Option<PathBuf> {
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

        macro_rules! try_path {
            ($name:expr) => {
                let name = $name;
                if self.options.fs.is_file(&name) {
                    return Some(name);
                }
            };
        }

        try_path!(path_buf.with_file_name(name).with_extension("scss"));
        try_path!(path_buf
            .with_file_name(format!("_{}", name.to_str().unwrap()))
            .with_extension("scss"));
        try_path!(path_buf.clone());
        try_path!(path_buf.join("index.scss"));
        try_path!(path_buf.join("_index.scss"));

        for path in &self.options.load_paths {
            if self.options.fs.is_dir(path) {
                try_path!(path
                    .join(&path_buf)
                    .with_file_name(name)
                    .with_extension("scss"));
                try_path!(path
                    .join(&path_buf)
                    .with_file_name(format!("_{}", name.to_str().unwrap()))
                    .with_extension("scss"));
                try_path!(path.join(&path_buf).join("index.scss"));
                try_path!(path.join(&path_buf).join("_index.scss"));
            } else {
                try_path!(path.to_path_buf());
                try_path!(path.with_file_name(name).with_extension("scss"));
                try_path!(path
                    .with_file_name(format!("_{}", name.to_str().unwrap()))
                    .with_extension("scss"));
                try_path!(path.join("index.scss"));
                try_path!(path.join("_index.scss"));
            }
        }

        None
    }

    pub(crate) fn parse_single_import(
        &mut self,
        file_name: &str,
        span: Span,
    ) -> SassResult<Vec<Stmt>> {
        let path: &Path = file_name.as_ref();

        if let Some(name) = self.find_import(path) {
            let file = self.map.add_file(
                name.to_string_lossy().into(),
                String::from_utf8(self.options.fs.read(&name)?)?,
            );
            return Parser {
                toks: &mut Lexer::new_from_file(&file),
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
                options: self.options,
                modules: self.modules,
                module_config: self.module_config,
            }
            .parse();
        }

        Err(("Can't find stylesheet to import.", span).into())
    }

    pub(super) fn import(&mut self) -> SassResult<Vec<Stmt>> {
        if self.flags.in_function() {
            return Err(("This at-rule is not allowed here.", self.span_before).into());
        }

        self.whitespace_or_comment();

        match self.toks.peek() {
            Some(Token { kind: '\'', .. })
            | Some(Token { kind: '"', .. })
            | Some(Token { kind: 'u', .. }) => {}
            Some(Token { pos, .. }) => return Err(("Expected string.", pos).into()),
            None => return Err(("expected more input.", self.span_before).into()),
        };
        let Spanned {
            node: file_name_as_value,
            span,
        } = self.parse_value(true, &|_| false)?;

        match file_name_as_value {
            Value::String(s, QuoteKind::Quoted) => {
                if is_plain_css_import(&s) {
                    Ok(vec![Stmt::Import(format!("\"{}\"", s))])
                } else {
                    self.parse_single_import(&s, span)
                }
            }
            Value::String(s, QuoteKind::None) => {
                if s.starts_with("url(") {
                    Ok(vec![Stmt::Import(s)])
                } else {
                    self.parse_single_import(&s, span)
                }
            }
            Value::List(v, Comma, _) => {
                let mut list_of_imports: Vec<Stmt> = Vec::new();
                for file_name_element in v {
                    match file_name_element {
                        #[allow(clippy::case_sensitive_file_extension_comparisons)]
                        Value::String(s, QuoteKind::Quoted) => {
                            let lower = s.to_ascii_lowercase();
                            if lower.ends_with(".css")
                                || lower.starts_with("http://")
                                || lower.starts_with("https://")
                            {
                                list_of_imports.push(Stmt::Import(format!("\"{}\"", s)));
                            } else {
                                list_of_imports.append(&mut self.parse_single_import(&s, span)?);
                            }
                        }
                        Value::String(s, QuoteKind::None) => {
                            if s.starts_with("url(") {
                                list_of_imports.push(Stmt::Import(s));
                            } else {
                                list_of_imports.append(&mut self.parse_single_import(&s, span)?);
                            }
                        }
                        _ => return Err(("Expected string.", span).into()),
                    }
                }

                Ok(list_of_imports)
            }
            _ => Err(("Expected string.", span).into()),
        }
    }
}
