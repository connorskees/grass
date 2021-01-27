# 0.11.0

- `fs` option added to allow interception and reimplementation of all file system operations (such as imports)

# 0.10.8

- bugfix: properly emit the number `0` in compressed mode (#53)

# 0.10.7

- special case plain CSS fn `clamp`
- support more uses of plain CSS fns inside `rgb`/`rgba`/`hsl`/`hsla`
- better support for `@at-root` at the toplevel and inside media queries
- bugfixes for the module system
- more robust handling of load paths that are directories

# 0.10.6

- **feature complete, byte-for-byte support for bootstrap**
  - add bootstrap v5.0.2 to ci
  - run script to verify output against the last 2,500 commits to bootstrap
- feature complete `min`/`max` support -- special functions and `min`/`max` are now allowed as arguments
- removed dependency on `peekmore`, which sped up parsing and simplified lookahead
- emit comments inside the `@if` rule body
- fix bug in `hue(...)` function in which the value would be incorrect when the `red` channel was the highest and the green channel was lower than the blue channel
- no longer round output from `saturation(...)` function
- improve handling of newlines for `@media`, `@supports`, `@at-root`, placeholder selectors, unrelated style rules, and unknown @-rules
- arglists can be equal to comma separated lists
- throw error for invalid uses of `@charset`
- more robustly parse `@else if`, allowing escaped and uppercase characters
- resolve two `@extend` bugs -- one in which we would incorrectly emit `a b, a > b` as a selector, even though `a b` is a superselector of `a > b`, and a feature called "three-level extend loop", in which a stylesheet where `a` extends `b`, `b` extends `c`, and `c` extends `a` would fail to include all 3 selectors in certain places
- support compressed values for comma separated lists and numbers
- more robustly parse unknown @-rules

# 0.10.5

- support compressed output
- support new builtin functions `math.div`, `map.set`
- support the HWB colorspace and builtin functions `color.hwb`, `color.blackness`, `color.whiteness`
- `:is` pseudo selector is now considered an alias of `:matches` in `@extend`
- support `$keys...` argument in `map.merge`
- `%` now implements the modulo operation, rather than finding the remainder. this largely affects negative numbers
- fix parsing bug in which `/***/` in a selector would miss the closing `/`

# 0.10.4

- plain css `invert(..)` accepts numbers with any unit
- plain css imports (e.g. `@import url(foo)` or `@import "foo.css"`) are now emitted at the top of documents

# 0.10.3

- hyphen followed by interpolation is not treated as subtraction, e.g. `10-#{10}` => `10 -10` rather than `0`
- function arguments do not affect variables in outer scopes (fixes [#37](https://github.com/connorskees/grass/issues/37))
- improve error messages for NaN with units passed to builtin functions

# 0.10.2

- use `std::fs::OpenOptions` to open files ([#35](https://github.com/connorskees/grass/pull/35) by [@MidasLamb](https://github.com/MidasLamb))

# 0.10.1

- **implement `@use` and the module system**
- support the filter syntax for function arguments, e.g. `alpha(opacity=1)`
- disallow certain at-rules in functions, resolving several panics
- allow vendor-prefixed special CSS functions, e.g. `-webkit-calc(...)`
- allow decimal percent selectors inside `@keyframes`
- allow vendor-prefixed `@keyframes`
- resolve parsing bug for maps involving silent comments
- allow escaped `!` in selectors
- allow multiline comments in functions
- resolve several panics on malformed input when parsing bracketed lists
- support NaN in all contexts
- add support for unicode ranges
- recognize plain CSS imports beginning with `//`, e.g. `@import "//fonts.googleapis.com/css?family=Droid+Sans";`
- resolve integer overflows in `@for` when bounds were equal to `i32::MIN` and `i32::MAX`
- allow quoted strings in default function arguments

# 0.10.0

- bugfixes for `@media` query regressions
- bugfixes for maps, arglists, and `@each`
- implement string interning for identifiers and style properties
- implement spec-compliant variable scoping
- emit `@import` when importing `url(...)` or `*.css`
- resolve all panics for malformed `@import`
- various optimizations that now allow us to compile bootstrap 10% faster than `libsass`
- errors inside builtin functions use `inspect` to print values
- bugfixes for color and map equality (e.g. `red` == `#ff0000`)
- hide unimplemented command line flags
- implement CLI options for `--quiet`, `--load-path` ([#22](https://github.com/connorskees/grass/pull/22) by @JosephLing), `--no-charset`, `--stdin`, and `--no-unicode`
- use unicode characters in error messages by default
- allow comma separated `@import` statements ([#23](https://github.com/connorskees/grass/pull/23) by @JosephLing)
- implement and correctly parse `!optional` in `@extend`
- lazily evaluate `!default` variable values
- disallow interpolation in mixin and function names
- improve parsing for `@supports` and unknown at-rules

## Breaking

- functions now take an `Options` struct

# 0.9.5

A small release fixing potential build issues and improving documentation.

This release is not published to NPM due to [a bug](https://github.com/rustwasm/wasm-pack/issues/837)
in `wasm-pack`.

# 0.9.4

- implement `@keyframes`
- don't strip newlines following comments in selectors

# 0.9.3

- fix parsing bugs for empty bracketed lists
- partially implement inverse units
- remove all remaining `todo!()`s from binary and unary ops
- parse keywords case sensitively
- various optimizations that make bulma about _6x faster_ to compile

# 0.9.2

- implement builtin functions `min` and `max`
- bugfixes for `@extend` and `selector-unify`
- allow `@content` to take arguments
- bugfixes for `@content`, for example it will no longer infinitely recurse for chained mixins
- better support queries in `@media`
- bugfixes for `@media`
- add support for splats, e.g. `rgba([1, 2, 3, 4]...)`
- resolve a number of parsing bugs for `@for`, variable declarations, selectors, and maps
- completely rewrite how styles are evaluated, allowing short circuiting of values like `false and unit(foo)` and `if(true, foo, unit(foo)`

# 0.9.1

This release is largely focused on `@extend`, but it also resolves some regressions resulting from the new parser.

- **implement `@extend`**
- properly document new API
- MVP implementation of `@supports`
- fix regression in which `@at-root` would panic when placed after a ruleset
- fix regression related to `@mixin` and `@function` scoping when combined with outer, local variables
- remove most remaining `unwrap`s that could result in a panic

# 0.9.0

This release is focused on setting up the groundwork for implementing `@extend` as well
as being able to compile Bootstrap.

- implement all builtin selector functions
  - `selector-append`
  - `selector-extend`
  - `selector-nest`
  - `selector-parse`
  - `selector-replace`
  - `selector-unify`
  - `simple-selectors`
  - `is-superselector`
- implement builtin function `content-exists`
- allow `@import`, `@warn`, and `@debug` in all contexts, such as inside `@mixin`
- refactor control flow evaluation, resolving some issues blocking Bootstrap

#### Breaking Changes

- remove the `StyleSheet` struct in favor of freestanding functions, `from_string` and `from_path`

# 0.8.3

This release is largely focused on performance and robustness

- implement smallint optimization for numbers, making some benchmarks 50% faster
- remove `bimap` as a dependency for storing named colors in favor of an ad hoc, more specialized data structure
- remove _dozens_ of panics on malformed input
- use `beef::Cow` instead of `std::borrow::Cow`
- increase code coverage to 80%

# 0.8.2

This release contains significant (>10x) improvements for WASM speed.
Performance is now comparable to libsass bindings with `node-sass` as
well as `dart-sass` with dart2js. It is, however, roughly 4x slower than
native `grass`.
