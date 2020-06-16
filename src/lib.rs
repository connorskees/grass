/*! # grass
An implementation of the sass specification in pure rust.

All functionality is currently exposed through [`StyleSheet`].

Spec progress as of 2020-05-01:

| Passing | Failing | Total |
|---------|---------|-------|
| 2193    | 2900    | 5093  |

## Use as library
```
fn main() -> Result<(), grass::Error> {
    let sass = grass::from_string("a { b { color: &; } }".to_string())?;
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
    // Self { .. } is less explicit than Foo { .. }
    clippy::use_self,
    // this is too pedantic -- some things don't need docs!
    clippy::missing_docs_in_private_items,
    clippy::unreachable,
    // this disallows binding as well
    clippy::wildcard_enum_match_arm,
    // this is too pedantic -- we are allowed to add numbers!
    clippy::integer_arithmetic,
    // this is too pedantic for now -- the library is changing too quickly for
    // good docs to be written
    clippy::missing_errors_doc,
    // this is too pedantic -- it results in some names being less explicit
    // than they should
    clippy::module_name_repetitions,
    // this is too pedantic -- it is sometimes useful to break up `impl`s
    clippy::multiple_inherent_impl,
    // filter isn't fallible
    clippy::filter_map,
    clippy::else_if_without_else,
    clippy::new_ret_no_self,
    renamed_and_removed_lints,
    clippy::unknown_clippy_lints,
    clippy::replace_consts,

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

    clippy::string_add,
    clippy::get_unwrap,
    clippy::unit_arg,
    clippy::wrong_self_convention,
    clippy::items_after_statements,
    clippy::shadow_reuse,
    clippy::shadow_unrelated,
)]
#![cfg_attr(feature = "nightly", feature(track_caller))]
#![cfg_attr(feature = "profiling", inline(never))]
use std::{fs, path::Path};

#[cfg(target_pointer_width = "64")]
pub(crate) use beef::lean::Cow;
#[cfg(not(target_pointer_width = "64"))]
pub(crate) use beef::Cow;

use codemap::CodeMap;

use peekmore::PeekMore;

pub use crate::error::{SassError as Error, SassResult as Result};
pub(crate) use crate::token::Token;
use crate::{
    lexer::Lexer,
    output::Css,
    parse::{common::NeverEmptyVec, Parser},
    scope::Scope,
    selector::Selector,
};

mod args;
mod atrule;
mod builtin;
mod color;
mod common;
mod error;
mod lexer;
mod output;
mod parse;
mod scope;
mod selector;
mod style;
mod stylesheet;
mod token;
mod unit;
mod utils;
mod value;

fn raw_to_parse_error(map: &CodeMap, err: Error) -> Error {
    let (message, span) = err.raw();
    Error::from_loc(message, map.look_up_span(span))
}

/// Write CSS to `buf`, constructed from a path
///
/// ```
/// fn main() -> Result<(), grass::Error> {
///     let sass = grass::from_path("input.scss")?;
///     Ok(())
/// }
/// ```
#[cfg_attr(feature = "profiling", inline(never))]
#[cfg_attr(not(feature = "profiling"), inline)]
#[cfg(not(feature = "wasm"))]
pub fn from_path(p: &str) -> Result<String> {
    let mut map = CodeMap::new();
    let file = map.add_file(p.into(), String::from_utf8(fs::read(p)?)?);
    Css::from_stmts(
        Parser {
            toks: &mut Lexer::new(&file)
                .collect::<Vec<Token>>()
                .into_iter()
                .peekmore(),
            map: &mut map,
            path: p.as_ref(),
            scopes: &mut NeverEmptyVec::new(Scope::new()),
            global_scope: &mut Scope::new(),
            super_selectors: &mut NeverEmptyVec::new(Selector::new()),
            span_before: file.span.subspan(0, 0),
            content: None,
            in_mixin: false,
            in_function: false,
            in_control_flow: false,
            at_root: true,
            at_root_has_selector: false,
        }
        .parse()
        .map_err(|e| raw_to_parse_error(&map, e))?,
    )
    .map_err(|e| raw_to_parse_error(&map, e))?
    .pretty_print(&map)
    .map_err(|e| raw_to_parse_error(&map, e))
}

/// Write CSS to `buf`, constructed from a string
///
/// ```
/// fn main() -> Result<(), grass::Error> {
///     let sass = grass::from_string("a { b { color: &; } }".to_string())?;
///     assert_eq!(sass, "a b {\n  color: a b;\n}\n");
///     Ok(())
/// }
/// ```
#[cfg_attr(feature = "wasm", wasm_bindgen)]
#[cfg_attr(feature = "profiling", inline(never))]
#[cfg_attr(not(feature = "profiling"), inline)]
pub fn from_string(p: String) -> Result<String> {
    let mut map = CodeMap::new();
    let file = map.add_file("stdin".into(), p);
    Css::from_stmts(
        Parser {
            toks: &mut Lexer::new(&file)
                .collect::<Vec<Token>>()
                .into_iter()
                .peekmore(),
            map: &mut map,
            path: Path::new(""),
            scopes: &mut NeverEmptyVec::new(Scope::new()),
            global_scope: &mut Scope::new(),
            super_selectors: &mut NeverEmptyVec::new(Selector::new()),
            span_before: file.span.subspan(0, 0),
            content: None,
            in_mixin: false,
            in_function: false,
            in_control_flow: false,
            at_root: true,
            at_root_has_selector: false,
        }
        .parse()
        .map_err(|e| raw_to_parse_error(&map, e))?,
    )
    .map_err(|e| raw_to_parse_error(&map, e))?
    .pretty_print(&map)
    .map_err(|e| raw_to_parse_error(&map, e))
}
