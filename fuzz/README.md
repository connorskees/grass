# Fuzz
Fuzzing targets for the grass library.

## Installing
You'll need `cargo-fuzz` for this to work, simply do:
```
cargo install cargo-fuzz
```

## Running
Get a list of available targets with:
```
cargo fuzz list
```

And run a available target simply with:
```
cargo fuzz run <target>
```
You might have to use nightly:
```
cargo +nightly fuzz run <target>
```



## More info about fuzzing
Consult the [fuzzing book](https://rust-fuzz.github.io/book/introduction.html).