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

#![cfg_attr(doc, feature(doc_cfg))]
#![warn(clippy::all, clippy::cargo, clippy::dbg_macro)]
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

    unknown_lints,
)]

pub use grass_compiler::*;

/// Include CSS in your binary at compile time from a Sass source file
///
/// ```
/// static CSS: &str = grass::include!("../static/_index.scss");
/// ```
///
/// This requires the `"macro"` feature, which is not enabled by default.
///
/// By default `grass` will track files using [`include_str!`]. This allows incremental
/// compilation to be updated when any Sass files are modified.
///
/// If compiling with a nightly version of rust, `grass` can make use of
/// [proc_macro::tracked_path](https://github.com/rust-lang/rust/issues/99515)
/// in order to force incremental recompilation, which is more robust and potentially
/// faster. This is enabled by the `"nightly"` feature.
///
/// ###### Limitations
///
/// Compilation options are not configurable with this macro. The default values
/// for all options are used, except for output style, which is compressed.
#[macro_export]
#[cfg(any(feature = "macro", doc))]
#[cfg_attr(doc, doc(cfg(feature = "macro")))]
macro_rules! include {
    ($path:literal) => {
        $crate::__internal::include_sass::include_sass!($path);
    };
}

#[doc(hidden)]
#[cfg(feature = "macro")]
pub mod __internal {
    #[doc(hidden)]
    pub use include_sass;
}
