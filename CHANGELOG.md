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
