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

The large features remaining are

```
indented syntax
css imports
@forward
compressed output
```

This is in addition to dozens of smaller features, edge cases, and miscompilations.

Starting from `grass v0.9.4`, it is possible to compile Twitter Bootstrap 4 as well as bulma-scss.

The output is not exact byte-for-byte, and the remaining differences in output are tracked [here](https://github.com/connorskees/grass/issues/4).

## Web Assembly

`grass` experimentally releases a
[WASM version of the library to npm](https://www.npmjs.com/package/@connorskees/grass),
compiled using wasm-bindgen. To use `grass` in your JavaScript projects, just run
`npm install @connorskees/grass` to your package.json. Better documentation
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

These numbers come from a default run of the Sass specification as shown above.

```
2020-08-12
PASSING: 3387
FAILING: 1706
TOTAL: 5093
```

```
2020-07-24
PASSING: 2935
FAILING: 2158
TOTAL: 5093
```

```
2020-06-07
PASSING: 2442
FAILING: 2651
TOTAL: 5093
```

```
2020-05-01
PASSING: 2193
FAILING: 2900
TOTAL: 5093
```

```
2020-04-01
PASSING: 1711
FAILING: 3382
TOTAL: 5093
```


```
2020-03-22
PASSING: 1442
FAILING: 3651
TOTAL: 5093
```


```
2020-02-03
PASSING: 242
FAILING: 4851
TOTAL: 5093
```

```
2020-01-20
PASSING: 143
FAILING: 4950
TOTAL: 5093
```
