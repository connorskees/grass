#![cfg(test)]

#[macro_use]
mod macros;

error!(
    nothing_after_decimal,
    "a {\n  color: 1.;\n}\n", "Error: Expected digit."
);
