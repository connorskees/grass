#[macro_use]
mod macros;

test!(
    arg_is_binop,
    "@use \"sass:meta\";

    a {
        color: meta.calc-args(calc(1vh + 1px));
    }",
    "a {\n  color: 1vh + 1px;\n}\n"
);
