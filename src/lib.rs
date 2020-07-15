/*! # grass
An implementation of the Sass specification in pure rust.

Spec progress as of 2020-07-04:

| Passing | Failing | Total |
|---------|---------|-------|
| 2880    | 2213    | 5093  |

## Use as library
```
fn main() -> Result<(), Box<grass::Error>> {
    let sass = grass::from_string("a { b { color: &; } }".to_string(), &grass::Options::default())?;
    assert_eq!(sass, "a b {\n  color: a b;\n}\n");
    Ok(())
}
```

## Use as binary
```bash
cargo install grass
grass input.scss
```
*/

#![warn(
    clippy::all,
    clippy::restriction,
    clippy::pedantic,
    clippy::nursery,
    clippy::cargo
)]
#![deny(missing_debug_implementations)]
#![allow(
    // explicit return makes some things look ugly
    clippy::implicit_return,
    clippy::use_self,
    clippy::missing_docs_in_private_items,
    clippy::unreachable,
    // this disallows binding as well, e.g. `v => ...`
    clippy::wildcard_enum_match_arm,
    clippy::module_name_repetitions,
    // it is sometimes useful to break up `impl`s
    clippy::multiple_inherent_impl,
    // filter isn't fallible
    clippy::filter_map,
    clippy::else_if_without_else,
    clippy::new_ret_no_self,
    renamed_and_removed_lints,
    clippy::unknown_clippy_lints,
    clippy::replace_consts,
    clippy::single_match,

    // temporarily allowed while under heavy development.
    // eventually these allows should be refactored away
    // to no longer be necessary
    clippy::as_conversions,
    clippy::todo,
    clippy::too_many_lines,
    clippy::panic,
    clippy::unwrap_used,
    clippy::option_unwrap_used,
    clippy::result_unwrap_used,
    clippy::cast_possible_truncation,
    clippy::single_match_else,
    clippy::indexing_slicing,
    clippy::redundant_pub_crate,
    // the api is changing too often to allot this
    clippy::missing_errors_doc,

    clippy::integer_arithmetic,
    clippy::string_add,
    clippy::get_unwrap,
    clippy::wrong_self_convention,
    clippy::items_after_statements,
    clippy::shadow_reuse,
    clippy::shadow_unrelated,
    // this is only available on nightly
    clippy::unnested_or_patterns,
)]
#![cfg_attr(feature = "nightly", feature(track_caller))]
#![cfg_attr(feature = "profiling", inline(never))]
use std::{fs, path::Path};

#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

pub(crate) use beef::lean::Cow;

use codemap::CodeMap;

use peekmore::PeekMore;

pub use crate::error::{SassError as Error, SassResult as Result};
pub(crate) use crate::token::Token;
use crate::{
    lexer::Lexer,
    output::Css,
    parse::{
        common::{ContextFlags, NeverEmptyVec},
        Parser,
    },
    scope::{Scope, Scopes},
    selector::{Extender, Selector},
};

mod args;
mod atrule;
mod builtin;
mod color;
mod common;
mod error;
mod interner;
mod lexer;
mod output;
mod parse;
mod scope;
mod selector;
mod style;
mod token;
mod unit;
mod utils;
mod value;

#[non_exhaustive]
#[derive(Debug)]
pub enum OutputStyle {
    Expanded,
    Compressed,
}

#[derive(Debug)]
pub struct Options<'a> {
    pub style: OutputStyle,
    pub load_paths: Vec<&'a Path>,
    pub allows_charset: bool,
    pub unicode_error_messages: bool,
    pub quiet: bool,
}

///
/// `load_paths` - list of paths/files to check for imports for more information see the docs:
/// - <https://sass-lang.com/documentation/at-rules/import#finding-the-file>
/// - <https://sass-lang.com/documentation/at-rules/import#load-paths>
impl<'a> Default for Options<'a> {
    fn default() -> Self {
        Self {
            style: OutputStyle::Expanded,
            load_paths: Vec::new(),
            allows_charset: true,
            unicode_error_messages: true,
            quiet: false,
        }
    }
}

#[allow(clippy::missing_const_for_fn)]
impl<'a> Options<'a> {
    /// `grass` currently offers 2 different output styles
    ///
    ///  - `OutputStyle::Expanded` writes each selector and declaration on its own line.
    ///  - `OutputStyle::Compressed` removes as many extra characters as possible, and writes the entire stylesheet on a single line.
    ///
    /// By default, output is expanded.
    #[must_use]
    #[inline]
    pub fn style(mut self, style: OutputStyle) -> Self {
        self.style = style;
        self
    }

    #[must_use]
    #[inline]
    pub fn quiet(mut self, quiet: bool) -> Self {
        self.quiet = quiet;
        self
    }

    #[must_use]
    #[inline]
    pub fn load_path(mut self, path: &'a Path) -> Self {
        self.load_paths.push(path);
        self
    }

    /// adds on to the `load_path` vec, does not set the vec to paths
    #[must_use]
    #[inline]
    pub fn load_paths(mut self, paths: &'a [&'a Path]) -> Self {
        self.load_paths.extend_from_slice(paths);
        self
    }
    #[must_use]
    #[inline]
    pub fn allows_charset(mut self, allows_charset: bool) -> Self {
        self.allows_charset = allows_charset;
        self
    }

    #[must_use]
    #[inline]
    pub fn unicode_error_messages(mut self, unicode_error_messages: bool) -> Self {
        self.unicode_error_messages = unicode_error_messages;
        self
    }
}

fn raw_to_parse_error(map: &CodeMap, err: Error) -> Box<Error> {
    let (message, span) = err.raw();
    Box::new(Error::from_loc(message, map.look_up_span(span)))
}

/// Compile CSS from a path
///
/// ```
/// fn main() -> Result<(), Box<grass::Error>> {
///     let sass = grass::from_path("input.scss", &grass::Options::default())?;
///     Ok(())
/// }
/// ```
/// (grass does not currently allow files or paths that are not valid UTF-8)
#[cfg_attr(feature = "profiling", inline(never))]
#[cfg_attr(not(feature = "profiling"), inline)]
#[cfg(not(feature = "wasm"))]
pub fn from_path(p: &str, options: &Options) -> Result<String> {
    let mut map = CodeMap::new();
    let file = map.add_file(p.into(), String::from_utf8(fs::read(p)?)?);
    let empty_span = file.span.subspan(0, 0);

    let stmts = Parser {
        toks: &mut Lexer::new(&file)
            .collect::<Vec<Token>>()
            .into_iter()
            .peekmore(),
        map: &mut map,
        path: p.as_ref(),
        scopes: &mut Scopes::new(),
        global_scope: &mut Scope::new(),
        super_selectors: &mut NeverEmptyVec::new(Selector::new(empty_span)),
        span_before: empty_span,
        content: &mut Vec::new(),
        flags: ContextFlags::empty(),
        at_root: true,
        at_root_has_selector: false,
        extender: &mut Extender::new(empty_span),
        content_scopes: &mut Scopes::new(),
        options,
    }
    .parse()
    .map_err(|e| raw_to_parse_error(&map, *e))?;

    Css::from_stmts(stmts, false)
        .map_err(|e| raw_to_parse_error(&map, *e))?
        .pretty_print(&map)
        .map_err(|e| raw_to_parse_error(&map, *e))
}

/// Compile CSS from a string
///
/// ```
/// fn main() -> Result<(), Box<grass::Error>> {
///     let sass = grass::from_string("a { b { color: &; } }".to_string(), &grass::Options::default())?;
///     assert_eq!(sass, "a b {\n  color: a b;\n}\n");
///     Ok(())
/// }
/// ```
#[cfg_attr(feature = "profiling", inline(never))]
#[cfg_attr(not(feature = "profiling"), inline)]
#[cfg(not(feature = "wasm"))]
pub fn from_string(p: String, options: &Options) -> Result<String> {
    let mut map = CodeMap::new();
    let file = map.add_file("stdin".into(), p);
    let empty_span = file.span.subspan(0, 0);
    let stmts = Parser {
        toks: &mut Lexer::new(&file)
            .collect::<Vec<Token>>()
            .into_iter()
            .peekmore(),
        map: &mut map,
        path: Path::new(""),
        scopes: &mut Scopes::new(),
        global_scope: &mut Scope::new(),
        super_selectors: &mut NeverEmptyVec::new(Selector::new(empty_span)),
        span_before: empty_span,
        content: &mut Vec::new(),
        flags: ContextFlags::empty(),
        at_root: true,
        at_root_has_selector: false,
        extender: &mut Extender::new(empty_span),
        content_scopes: &mut Scopes::new(),
        options,
    }
    .parse()
    .map_err(|e| raw_to_parse_error(&map, *e))?;

    Css::from_stmts(stmts, false)
        .map_err(|e| raw_to_parse_error(&map, *e))?
        .pretty_print(&map)
        .map_err(|e| raw_to_parse_error(&map, *e))
}

#[cfg(feature = "wasm")]
#[wasm_bindgen]
pub fn from_string(p: String, options: &'_ Options<'_>) -> std::result::Result<String, JsValue> {
    let mut map = CodeMap::new();
    let file = map.add_file("stdin".into(), p);
    let empty_span = file.span.subspan(0, 0);

    let stmts = Parser {
        toks: &mut Lexer::new(&file)
            .collect::<Vec<Token>>()
            .into_iter()
            .peekmore(),
        map: &mut map,
        path: Path::new(""),
        scopes: &mut Scopes::new(),
        global_scope: &mut Scope::new(),
        super_selectors: &mut NeverEmptyVec::new(Selector::new(empty_span)),
        span_before: empty_span,
        content: &mut Vec::new(),
        flags: ContextFlags::empty(),
        at_root: true,
        at_root_has_selector: false,
        extender: &mut Extender::new(empty_span),
        content_scopes: &mut Scopes::new(),
        options,
    }
    .parse()
    .map_err(|e| raw_to_parse_error(&map, *e).to_string())?;

    Ok(Css::from_stmts(stmts, false)
        .map_err(|e| raw_to_parse_error(&map, *e).to_string())?
        .pretty_print(&map)
        .map_err(|e| raw_to_parse_error(&map, *e).to_string())?)
}
