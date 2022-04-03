# grass

This crate aims to provide a high level interface for compiling Sass into
plain CSS. It offers a very limited API, currently exposing only 2 functions.

In addition to a library, also included is a binary that is intended to act as an invisible
replacement to the Sass commandline executable.

This crate aims to achieve complete feature parity with the `dart-sass` reference
implementation. A deviation from the `dart-sass` implementation can be considered
a bug except for in the following situations:

- Error messages
- Error spans
- Certain aspects of the indented syntax
- Potentially others in the future

[Documentation](https://docs.rs/grass/)  
[crates.io](https://crates.io/crates/grass)

## Status

`grass` has reached a stage where one can be quite confident in its output. For the average user there should not be perceptible differences from `dart-sass`.

Every commit of `grass` is tested against bootstrap v5.0.2, and every release is tested against the last 2,500 commits of bootstrap's `main` branch.

That said, there are a number of known missing features and bugs. The notable features remaining are

```
indented syntax
@forward and more complex uses of @use
@at-root and @import media queries
@media query merging
/ as a separator in color functions, e.g. rgba(255, 255, 255 / 0)
Infinity and -Infinity
builtin meta function `keywords`
```

All known missing features and bugs are tracked in [#19](https://github.com/connorskees/grass/issues/19).

`grass` is not a drop-in replacement for `libsass` and does not intend to be. If you are upgrading to `grass` from `libsass`, you may have to make modifications to your stylesheets, though these changes should not differ from those you would have to make if upgrading to `dart-sass`.

## Web Assembly

`grass` experimentally releases a
[WASM version of the library to npm](https://www.npmjs.com/package/@connorskees/grass),
compiled using wasm-bindgen. To use `grass` in your JavaScript projects, just run
`npm install @connorskees/grass` to add it to your package.json. Better documentation
for this version will be provided when the library becomes more stable.

## Features

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
git clone https://github.com/connorskees/grass --recursive
cd grass
cargo b --release
./sass-spec/sass-spec.rb -c './target/release/grass'
```

Note: you will have to install [ruby](https://www.ruby-lang.org/en/downloads/),
[bundler](https://bundler.io/) and run `bundle install` in `./sass-spec/`.
This might also require you to install the requirements separately
for [curses](https://github.com/ruby/curses).

Alternatively, it is possible to use nodejs to run the spec,

```bash
# This script expects node >=v14.14.0. Check version with `node --version`
git clone https://github.com/connorskees/grass --recursive
cd grass && cargo b --release
cd sass-spec && npm install
npm run sass-spec -- --command '../target/release/grass'
```

These numbers come from a default run of the Sass specification as shown above.

```
2021-09-20
PASSING: 4205
FAILING: 2051
TOTAL: 6256
```
