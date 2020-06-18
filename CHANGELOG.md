# 0.9.1
 - fix regression in which `@at-root` would panic when placed after a ruleset
 - fix regression related to `@mixin` scoping and outer, local variables

# 0.9.0

This release is focused on setting up the groundwork for implementing `@extend` as well
as being able to compile Bootstrap.
 - Implement all builtin selector functions
   - `selector-append`
   - `selector-extend`
   - `selector-nest`
   - `selector-parse`
   - `selector-replace`
   - `selector-unify`
   - `simple-selectors`
   - `is-superselector`
 - Implement builtin function `content-exists`
 - Allow `@import`, `@warn`, and `@debug` in all contexts, such as inside `@mixin`
 - Refactor control flow evaluation, resolving some issues blocking Bootstrap

#### Breaking Changes
 - remove the `StyleSheet` struct in favor of freestanding functions, `from_string` and `from_path`

# 0.8.3

This release is largely focused on performance and robustness
 - implement smallint optimization for numbers, making some benchmarks 50% faster
 - remove `bimap` as a dependency for storing named colors in favor of an ad hoc, more specialized data structure
 - remove *dozens* of panics on malformed input
 - use `beef::Cow` instead of `std::borrow::Cow`
 - increase code coverage to 80%


# 0.8.2

This release contains significant (>10x) improvements for WASM speed.
Performance is now comparable to libsass bindings with `node-sass` as
well as `dart-sass` with dart2js. It is, however, roughly 4x slower than
native `grass`.
