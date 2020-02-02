#![cfg(test)]

#[macro_use]
mod macros;

test!(
    removes_double_quotes,
    "a {\n  color: #{\"red\"};\n}\n",
    "a {\n  color: red;\n}\n"
);
test!(
    removes_single_quotes,
    "a {\n  color: #{'red'};\n}\n",
    "a {\n  color: red;\n}\n"
);
