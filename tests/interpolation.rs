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
test!(
    number_after_interpolation,
    "a {\n  color: a#{foo}1;\n}\n",
    "a {\n  color: afoo1;\n}\n"
);
test!(
    double_hyphen_before_interpolation,
    "a {\n  --#{foo}: red;\n}\n",
    "a {\n  --foo: red;\n}\n"
);
