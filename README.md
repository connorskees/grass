# grass

An implementation of the SASS spec in rust with 0 dependencies

To run the official test suite,

```bash
git clone https://github.com/ConnorSkees/grass
cd grass
git submodule init
git submodule update
cargo b --release
./sass-spec/sass-spec.rb -c './target/release/grass'
```

2020-01-20
PASSING: 143
FAILING: 4950
TOTAL: 5093

## Features

`grass` is far from being feature complete! Below you can see what SCSS features are currently supported.
My MVP will include `values`, `@mixin`, `@include`, `@media`, `styles`, `operators`, `css functions` and `css at rules`.

- [ ] Variables
  - [x] Scoping
  - [x] Shadowing
  - [ ] Built-in variables
  - [ ] !default
  - [ ] !global
- [x] @import
- [x] @error
- [x] @warn
- [x] @debug
- [ ] Styles
  - [x] !important
  - [x] Interpolation
  - [ ] Nesting
  - [ ] Custom properties
  - [ ] Hidden declarations
- [ ] Selectors
  - [x] Attributes
  - [x] Parent selector &
  - [x] All other selectors
  - [x] Nesting
  - [ ] Placeholder selector %
- [x] Comments
  - [x] Removes single line comments
  - [x] Preserves toplevel multiline comments
  - [x] Removes inline multiline comments
- [ ] @mixin
  - [x] Keyword args
  - [x] Default arg values
  - [ ] Variadic args
  - [ ] @content
- [ ] @include
  - [x] Keyword args
  - [x] Default arg values
  - [ ] Content blocks
- [ ] Functions
  - [ ] @return
- [ ] Control flow
  - [ ] @if
  - [ ] @else
  - [ ] @for
  - [ ] @while
  - [ ] @each
- [ ] Constant folding
- [ ] Unit arithmetic
- [ ] CSS at rules
- [ ] CSS functions
  - [ ] calc()
  - [ ] url()
  - [ ] element()
  - [ ] progid:...()
  - [ ] expression()
  - [ ] min()
  - [ ] max()
- [ ] @media
- [ ] @use
- [ ] @extend
- [ ] @at-root
- [ ] @forward
- [ ] Values
  - [ ] Numbers
  - [ ] Strings
  - [ ] Colors
  - [ ] Lists
  - [ ] Maps
  - [ ] Booleans
  - [ ] Null
- [ ] Operators
- [ ] Built-in modules
- [ ] Content encoding
- [ ] WASM bindings
