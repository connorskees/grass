# grass

An implementation of the SASS spec in pure Rust

To run the official test suite,

```bash
git clone https://github.com/connorskees/grass
cd grass
git submodule init
git submodule update
cargo b --release
./sass-spec/sass-spec.rb -c './target/release/grass'
```

The large features remaining are
```
@while
maps
order of operations
@each
special case certain functions (min, max, calc, element, expression, progid, url)
variadic arguments
many builtin list functions
many builtin string functions
all builtin selector functions
all builtin map functions
@extend
indented syntax
dedicated global scope
a special parser for plain css
@use and module system
@forward
```
in addition to some smaller features like `\` in selectors


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
