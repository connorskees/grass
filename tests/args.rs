#![cfg(test)]

#[macro_use]
mod macros;

error!(
    variable_after_varargs,
    "@function foo($a..., $b) {\n  @return $a;\n}\n", "Error: expected \")\"."
);
error!(
    varargs_one_period,
    "@function foo($a.) {\n  @return $a;\n}\n", "Error: expected \".\"."
);
error!(
    varargs_two_periods,
    "@function foo($a..) {\n  @return $a;\n}\n", "Error: expected \".\"."
);
