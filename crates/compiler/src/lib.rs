/*!
This crate provides functionality for compiling [Sass](https://sass-lang.com/) to CSS.

This crate targets compatibility with the reference implementation in Dart. If
upgrading from the [now deprecated](https://sass-lang.com/blog/libsass-is-deprecated)
`libsass`, one may have to modify their stylesheets. These changes will not differ
from those necessary to upgrade to `dart-sass`, and in general such changes should
be quite rare.

This crate is capable of compiling Bootstrap 4 and 5, bulma and bulma-scss, Bourbon,
as well as most other large Sass libraries with complete accuracy. For the vast
majority of use cases there should be no perceptible differences from the reference
implementation.

## Use as library
```
# use grass_compiler as grass;
fn main() -> Result<(), Box<grass::Error>> {
    let css = grass::from_string(
        "a { b { color: &; } }".to_owned(),
        &grass::Options::default().style(grass::OutputStyle::Compressed)
    )?;
    assert_eq!(css, "a b{color:a b}");
    Ok(())
}
```

## Use as binary
```bash
cargo install grass
grass input.scss
```
*/

#![cfg_attr(doc_cfg, feature(doc_cfg))]
#![warn(clippy::all, clippy::cargo, clippy::dbg_macro)]
#![deny(missing_debug_implementations)]
#![allow(
    clippy::use_self,
    // filter isn't fallible
    clippy::manual_filter_map,
    renamed_and_removed_lints,
    clippy::unknown_clippy_lints,
    clippy::single_match,
    clippy::new_without_default,
    clippy::single_match_else,
    clippy::multiple_crate_versions,
    clippy::wrong_self_convention,
    clippy::comparison_chain,

    // these features are only available on nightly
    clippy::unnested_or_patterns,
    clippy::uninlined_format_args,

    // todo: these should be enabled
    clippy::cast_sign_loss,
    clippy::cast_lossless,
    clippy::cast_precision_loss,
    clippy::float_cmp,

    // todo: unignore once we bump MSRV
    clippy::format_push_string,
    clippy::unnecessary_unwrap,
    clippy::needless_late_init,

    unknown_lints,
)]

use std::io::Write;
use std::path::Path;

use parse::{CssParser, SassParser, StylesheetParser};
use sass_ast::StyleSheet;
use serializer::Serializer;
#[cfg(feature = "wasm-exports")]
use wasm_bindgen::prelude::*;

use codemap::CodeMap;

pub use crate::error::{
    PublicSassErrorKind as ErrorKind, SassError as Error, SassResult as Result,
};
pub use crate::fs::{Fs, NullFs, StdFs};
pub use crate::options::{InputSyntax, Options, OutputStyle};
pub use crate::{builtin::Builtin, evaluate::Visitor};
pub(crate) use crate::{context_flags::ContextFlags, lexer::Token};
use crate::{lexer::Lexer, parse::ScssParser};

pub mod sass_value {
    pub use crate::{
        ast::ArgumentResult,
        color::Color,
        common::{BinaryOp, Brackets, ListSeparator, QuoteKind},
        unit::{ComplexUnit, Unit},
        value::{
            ArgList, CalculationArg, CalculationName, Number, SassCalculation, SassFunction,
            SassMap, SassNumber, Value,
        },
    };
}

pub mod sass_ast {
    pub use crate::ast::*;
}

use crate::serializer::requires_semicolon;
pub use codemap;

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
mod unit;
mod utils;
mod value;

fn raw_to_parse_error(map: &CodeMap, err: Error, unicode: bool) -> Box<Error> {
    let (message, span) = err.raw();
    Box::new(Error::from_loc(message, map.look_up_span(span), unicode))
}

pub fn parse_stylesheet<P: AsRef<Path>>(
    input: String,
    file_name: P,
    options: &Options,
) -> Result<StyleSheet> {
    // todo: much of this logic is duplicated in `from_string_with_file_name`
    let mut map = CodeMap::new();
    let path = file_name.as_ref();
    let file = map.add_file(path.to_string_lossy().into_owned(), input);
    let empty_span = file.span.subspan(0, 0);
    let lexer = Lexer::new_from_file(&file);

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

    Ok(stylesheet)
}

fn write_from_string_with_file_name<P: AsRef<Path>, W: Write>(
    input: String,
    file_name: P,
    options: &Options,
    writer: W,
) -> Result<()> {
    let mut map = CodeMap::new();
    let path = file_name.as_ref();
    let file = map.add_file(path.to_string_lossy().into_owned(), input);
    let empty_span = file.span.subspan(0, 0);
    let lexer = Lexer::new_from_file(&file);

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
    let is_ascii = visitor.is_ascii;
    let stmts = visitor.finish();

    let mut serializer = Serializer::new(options, &map, false, empty_span, is_ascii, writer);

    serializer.start()?;

    let mut prev_was_group_end = false;
    let mut prev_requires_semicolon = false;
    let mut is_first_group = true;
    for stmt in stmts {
        if stmt.is_invisible() {
            continue;
        }

        let is_group_end = stmt.is_group_end();
        let requires_semicolon = requires_semicolon(&stmt);

        serializer
            .visit_group(
                stmt,
                prev_was_group_end,
                prev_requires_semicolon,
                is_first_group,
            )
            .map_err(|e| raw_to_parse_error(&map, *e, options.unicode_error_messages))?;

        prev_was_group_end = is_group_end;
        prev_requires_semicolon = requires_semicolon;
        is_first_group = false;
    }

    let wrote = !is_ascii || !is_first_group;
    serializer.finish(prev_requires_semicolon, wrote)
}

/// Compile CSS from a path, and write output to `writer`.
///
/// n.b. `grass` does not currently support files or paths that are not valid UTF-8
///
/// ```
/// # use grass_compiler as grass;
/// fn main() -> Result<(), Box<grass::Error>> {
///     let css = grass::from_path("input.scss", &grass::Options::default())?;
///     Ok(())
/// }
/// ```
pub fn write_from_path<P: AsRef<Path>, W: Write>(p: P, options: &Options, writer: W) -> Result<()> {
    write_from_string_with_file_name(
        String::from_utf8(options.fs.read(p.as_ref())?)?,
        p,
        options,
        writer,
    )
}

/// Compile CSS from a path
///
/// n.b. `grass` does not currently support files or paths that are not valid UTF-8
///
/// ```
/// # use grass_compiler as grass;
/// fn main() -> Result<(), Box<grass::Error>> {
///     let css = grass::from_path("input.scss", &grass::Options::default())?;
///     Ok(())
/// }
/// ```
#[inline]
pub fn from_path<P: AsRef<Path>>(p: P, options: &Options) -> Result<String> {
    let mut buf = Vec::new();

    write_from_path(p, options, &mut buf)?;

    // SAFETY: todo
    Ok(unsafe { String::from_utf8_unchecked(buf) })
}

/// Compile CSS from a string, and write output to `writer`.
///
/// ```
/// # use grass_compiler as grass;
/// fn main() -> Result<(), Box<grass::Error>> {
///     let css = grass::from_string("a { b { color: &; } }".to_string(), &grass::Options::default())?;
///     assert_eq!(css, "a b {\n  color: a b;\n}\n");
///     Ok(())
/// }
/// ```
pub fn write_from_string<S: Into<String>, W: Write>(
    input: S,
    options: &Options,
    writer: W,
) -> Result<()> {
    write_from_string_with_file_name(input.into(), "stdin", options, writer)
}

/// Compile CSS from a string
///
/// ```
/// # use grass_compiler as grass;
/// fn main() -> Result<(), Box<grass::Error>> {
///     let css = grass::from_string("a { b { color: &; } }".to_string(), &grass::Options::default())?;
///     assert_eq!(css, "a b {\n  color: a b;\n}\n");
///     Ok(())
/// }
/// ```
#[inline]
pub fn from_string<S: Into<String>>(input: S, options: &Options) -> Result<String> {
    let mut buf = Vec::new();

    write_from_string(input, options, &mut buf)?;

    // SAFETY: todo
    Ok(unsafe { String::from_utf8_unchecked(buf) })
}

#[cfg(feature = "wasm-exports")]
#[wasm_bindgen(js_name = from_string)]
pub fn from_string_js(input: String) -> std::result::Result<String, String> {
    from_string(input, &Options::default()).map_err(|e| e.to_string())
}
