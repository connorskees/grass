#![cfg(test)]

#[macro_use]
mod macros;

test!(
    basic_unknown_at_rule,
    "@foo {\n  a {\n    color: red;\n  }\n}\n"
);
test!(unknown_at_rule_no_selector, "@foo {\n  color: red;\n}\n");
test!(unknown_at_rule_no_body, "@foo;\n");
test!(unknown_at_rule_no_body_eof, "@foo", "@foo;\n");
test!(
    unknown_at_rule_interpolated_eof_no_body,
    "@#{()if(0,0<0,0)}",
    "@false;\n"
);
