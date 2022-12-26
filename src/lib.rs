/*!
This crate provides functionality for compiling [Sass](https://sass-lang.com/) to CSS.

## Use as library
```
fn main() -> Result<(), Box<grass::Error>> {
    let css = grass::from_string(
        "a { b { color: &; } }".to_owned(),
        &grass::Options::default()
    )?;
    assert_eq!(css, "a b {\n  color: a b;\n}\n");
    Ok(())
}
```

## Use as binary
```bash
cargo install grass
grass input.scss
```
*/

#![warn(clippy::all, clippy::cargo)]
#![deny(missing_debug_implementations)]
#![allow(
    clippy::use_self,
    clippy::missing_docs_in_private_items,
    clippy::unreachable,
    clippy::module_name_repetitions,
    // filter isn't fallible
    clippy::manual_filter_map,
    clippy::new_ret_no_self,
    renamed_and_removed_lints,
    clippy::unknown_clippy_lints,
    clippy::single_match,
    clippy::unimplemented,
    clippy::option_if_let_else,
    clippy::branches_sharing_code,
    clippy::derive_partial_eq_without_eq,

    // temporarily allowed while under heavy development.
    // eventually these allows should be refactored away
    // to no longer be necessary
    clippy::too_many_lines,
    clippy::cast_possible_truncation,
    clippy::single_match_else,
    clippy::redundant_pub_crate,
    // the api is changing too often to allot this
    clippy::missing_errors_doc,
    clippy::missing_const_for_fn,
    clippy::multiple_crate_versions,

    clippy::wrong_self_convention,
    clippy::items_after_statements,
    // this is only available on nightly
    clippy::unnested_or_patterns,
    clippy::uninlined_format_args,

    // todo:
    clippy::cast_sign_loss,
    clippy::cast_lossless,
    clippy::cast_precision_loss,
    clippy::float_cmp,
    clippy::wildcard_imports,
    clippy::comparison_chain,
    clippy::bool_to_int_with_if,
)]

use std::path::Path;

use parse::{CssParser, SassParser, StylesheetParser};
use serializer::Serializer;
#[cfg(feature = "wasm-exports")]
use wasm_bindgen::prelude::*;

use codemap::CodeMap;

pub use crate::error::{
    PublicSassErrorKind as ErrorKind, SassError as Error, SassResult as Result,
};
pub use crate::fs::{Fs, NullFs, StdFs};
pub use crate::options::{InputSyntax, Options, OutputStyle};
pub(crate) use crate::{context_flags::ContextFlags, token::Token};
use crate::{evaluate::Visitor, lexer::Lexer, parse::ScssParser};

mod ast;
mod builtin;
mod color;
mod common;
mod context_flags;
mod error;
mod evaluate;
mod fs;
mod interner;
mod lexer;
mod options;
mod parse;
mod selector;
mod serializer;
mod token;
mod unit;
mod utils;
mod value;

fn raw_to_parse_error(map: &CodeMap, err: Error, unicode: bool) -> Box<Error> {
    let (message, span) = err.raw();
    Box::new(Error::from_loc(message, map.look_up_span(span), unicode))
}

fn from_string_with_file_name(input: String, file_name: &str, options: &Options) -> Result<String> {
    let mut map = CodeMap::new();
    let file = map.add_file(file_name.to_owned(), input);
    let empty_span = file.span.subspan(0, 0);
    let lexer = Lexer::new_from_file(&file);

    let path = Path::new(file_name);

    let input_syntax = options
        .input_syntax
        .unwrap_or_else(|| InputSyntax::for_path(path));

    let stylesheet = match input_syntax {
        InputSyntax::Scss => {
            ScssParser::new(lexer, &mut map, options, empty_span, file_name.as_ref()).__parse()
        }
        InputSyntax::Sass => {
            SassParser::new(lexer, &mut map, options, empty_span, file_name.as_ref()).__parse()
        }
        InputSyntax::Css => {
            CssParser::new(lexer, &mut map, options, empty_span, file_name.as_ref()).__parse()
        }
    };

    let stylesheet = match stylesheet {
        Ok(v) => v,
        Err(e) => return Err(raw_to_parse_error(&map, *e, options.unicode_error_messages)),
    };

    let mut visitor = Visitor::new(path, options, &mut map, empty_span);
    match visitor.visit_stylesheet(stylesheet) {
        Ok(_) => {}
        Err(e) => return Err(raw_to_parse_error(&map, *e, options.unicode_error_messages)),
    }
    let stmts = visitor.finish();

    let mut serializer = Serializer::new(options, &map, false, empty_span);

    let mut prev_was_group_end = false;
    let mut prev_requires_semicolon = false;
    for stmt in stmts {
        if stmt.is_invisible() {
            continue;
        }

        let is_group_end = stmt.is_group_end();
        let requires_semicolon = Serializer::requires_semicolon(&stmt);

        serializer
            .visit_group(stmt, prev_was_group_end, prev_requires_semicolon)
            .map_err(|e| raw_to_parse_error(&map, *e, options.unicode_error_messages))?;

        prev_was_group_end = is_group_end;
        prev_requires_semicolon = requires_semicolon;
    }

    Ok(serializer.finish(prev_requires_semicolon))
}

/// Compile CSS from a path
///
/// n.b. grass does not currently support files or paths that are not valid UTF-8
///
/// ```
/// fn main() -> Result<(), Box<grass::Error>> {
///     let sass = grass::from_path("input.scss", &grass::Options::default())?;
///     Ok(())
/// }
/// ```

#[inline]
pub fn from_path(p: &str, options: &Options) -> Result<String> {
    from_string_with_file_name(
        String::from_utf8(options.fs.read(Path::new(p))?)?,
        p,
        options,
    )
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

#[inline]
pub fn from_string(input: String, options: &Options) -> Result<String> {
    from_string_with_file_name(input, "stdin", options)
}

#[cfg(feature = "wasm-exports")]
#[wasm_bindgen(js_name = from_string)]
pub fn from_string_js(input: String) -> std::result::Result<String, String> {
    from_string(input, &Options::default()).map_err(|e| e.to_string())
}
