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

The focus right now is just getting the most basic tests to pass.

```
2020-03-20
PASSING: 1394
FAILING: 3699
TOTAL: 5093
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
