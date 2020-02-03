#![cfg(test)]

#[macro_use]
mod macros;

test!(
    if_true,
    "a {\n  color: if(true, 1, 2)\n}\n",
    "a {\n  color: 1;\n}\n"
);
test!(
    if_named_args,
    "a {\n  color: if($condition: true, $if-true: 1, $if-false: 2)\n}\n",
    "a {\n  color: 1;\n}\n"
);
test!(
    if_false,
    "a {\n  color: if(false, 1, 2);\n}\n",
    "a {\n  color: 2;\n}\n"
);
