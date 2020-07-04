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

## Web Assembly

`grass` experimentally releases a
[WASM version of the library to npm](https://www.npmjs.com/package/@connorskees/grass),
compiled using wasm-bindgen. To use `grass` in your JavaScript projects, just run
`npm install @connorskees/grass` to your package.json. Better documentation
for this version will be provided when the library becomes more stable.

## Status

The large features remaining are

```
indented syntax
css imports
@use, @forward, and the module system
@keyframes
```

This is in addition to dozens of smaller features, edge cases, and miscompilations.
Features currently blocking Bootstrap are tracked [here](https://github.com/connorskees/grass/issues/4).

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
2020-07-04
PASSING: 2874
FAILING: 2219
TOTAL: 5093
```

```
2020-06-28
PASSING: 2821
FAILING: 2272
TOTAL: 5093
```

```
2020-06-23
PASSING: 2800
FAILING: 2293
TOTAL: 5093
```

```
2020-06-16
PASSING: 2489
FAILING: 2604
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
2020-04-21
PASSING: 2150
FAILING: 2943
TOTAL: 5093
```

```
2020-04-07
PASSING: 2031
FAILING: 3062
TOTAL: 5093
```

```
2020-04-01
PASSING: 1711
FAILING: 3382
TOTAL: 5093
```

```
2020-03-30
PASSING: 1685
FAILING: 3408
TOTAL: 5093
```

```
2020-03-23
PASSING: 1547
FAILING: 3546
TOTAL: 5093
```

```
2020-03-22
PASSING: 1442
FAILING: 3651
TOTAL: 5093
```

```
2020-02-24
PASSING: 1192
FAILING: 3901
TOTAL: 5093
```

```
2020-02-17
PASSING: 1115
FAILING: 3978
TOTAL: 5093
```

```
2020-02-10
PASSING: 475
FAILING: 4618
TOTAL: 5093
```

```
2020-02-03
PASSING: 242
FAILING: 4851
TOTAL: 5093
```

```
2020-01-27
PASSING: 186
FAILING: 4907
TOTAL: 5093
```

```
2020-01-20
PASSING: 143
FAILING: 4950
TOTAL: 5093
```
