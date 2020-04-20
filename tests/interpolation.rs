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
test!(
    preserves_inner_single_quotes,
    "a {\n  color: #{\"''\"};\n}\n",
    "a {\n  color: '';\n}\n"
);
test!(
    single_quotes_converted_to_double_when_interpolated,
    "a {\n  color: '#{foo}';\n}\n",
    "a {\n  color: \"foo\";\n}\n"
);
test!(
    double_quotes_inside_double_quoted_string,
    "a {\n  color: #{\"#{'\"'}\"};\n}\n",
    "a {\n  color: \";\n}\n"
);
