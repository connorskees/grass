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

2020-01-27  
PASSING: 186  
FAILING: 4907  
TOTAL: 5093

2020-01-20  
PASSING: 143  
FAILING: 4950  
TOTAL: 5093  

## Features

`grass` is far from being feature complete!

Right now, I am focusing on just getting the most basic tests to pass. This is actually very close to being done!
Major tasks (not basic) remaining include builtin functions, control flow, and values.

I would eventually like to focus heavily on performance and WASM bindings -- two areas that a Rust implementation has an advantage over Dart
