# grass

This crate aims to provide a high level interface for compiling Sass into
plain CSS. It offers a very limited API, currently exposing only 2 functions.

In addition to a library, this crate also includes a binary that is intended to act as an invisible
replacement to the Sass commandline executable.

This crate aims to achieve complete feature parity with the `dart-sass` reference
implementation. A deviation from the `dart-sass` implementation can be considered
a bug except for in the case of error messages and error spans.

[Documentation](https://docs.rs/grass/)  
[crates.io](https://crates.io/crates/grass)

## Status

`grass` has reached a stage where one can be quite confident in its output. For the average user there should not be perceptible differences from `dart-sass`.

Every commit of `grass` is tested against bootstrap v5.0.2, and every release is tested against the last 2,500 commits of bootstrap's `main` branch.

That said, there are a number of known missing features and bugs. The rough edges of `grass` largely include `@forward` and more complex uses of `@uses`. We support basic usage of these rules, but more advanced features such as `@import`ing modules containing `@forward` with prefixes may not behave as expected.

All known missing features and bugs are tracked in [#19](https://github.com/connorskees/grass/issues/19).

`grass` is not a drop-in replacement for `libsass` and does not intend to be. If you are upgrading to `grass` from `libsass`, you may have to make modifications to your stylesheets, though these changes should not differ from those you would have to make if upgrading to `dart-sass`.

## Web Assembly

`grass` experimentally releases a
[WASM version of the library to npm](https://www.npmjs.com/package/@connorskees/grass),
compiled using wasm-bindgen. To use `grass` in your JavaScript projects, run
`npm install @connorskees/grass` to add it to your package.json. This version of grass is not currently well documented, but one can find example usage in the [`grassmeister` repository](https://github.com/connorskees/grassmeister).

## Cargo Features

### commandline

(enabled by default): build a binary using clap

### random

(enabled by default): enable the builtin functions `random([$limit])` and `unique-id()`

In the future this feature will be removed when it is no longer necessary to rely on `rand` for
random numbers.

## Testing

As much as possible this library attempts to follow the same [philosophy for testing as
`rust-analyzer`](https://internals.rust-lang.org/t/experience-report-contributing-to-rust-lang-rust/12012/17).
Namely, all one should have to do is run `cargo test` to run all its tests.
This library maintains a test suite distinct from the `sass-spec`, though it
does include some spec tests verbatim. This has the benefit of allowing tests
to be run without ruby as well as allowing the tests more granular than they
are in the official spec.

Having said that, to run the official test suite,

```bash
# This script expects node >=v14.14.0. Check version with `node --version`
git clone https://github.com/connorskees/grass --recursive
cd grass && cargo b --release
cd sass-spec && npm install
npm run sass-spec -- --impl=dart-sass --command '../target/release/grass'
```

The spec runner does not work on Windows.

Using a modified version of the spec runner that ignores warnings and error spans (but does include error messages), we get the following results:

```
2022-12-26
PASSING: 6024
FAILING: 881
TOTAL: 6905
```

The majority of the failing tests are purely aesthetic, relating to whitespace
around comments in expanded mode or error messages.

<!-- todo: msrv 1.41.1 -->