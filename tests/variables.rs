#![cfg(test)]

#[macro_use]
mod macros;

test!(
    basic_variable,
    "$height: 1px;\na {\n  height: $height;\n}\n",
    "a {\n  height: 1px;\n}\n"
);
test!(
    variable_redeclaration,
    "$a: 1px;\n$a: 2px;\na {\n  height: $a;\n}\n",
    "a {\n  height: 2px;\n}\n"
);
test!(
    variable_shadowing,
    "$a: 1px;\n$b: $a;\na {\n  height: $b;\n}\n",
    "a {\n  height: 1px;\n}\n"
);
test!(
    variable_shadowing_val_does_not_change,
    "$a: 1px;\n$b: $a; $a: 2px;\na {\n  height: $b;\n}\n",
    "a {\n  height: 1px;\n}\n"
);
test!(
    variable_shadowing_val_does_not_change_complex,
    "a {\n  color: red;\n}\n$y: before;\n$x: 1 2 $y;\n$y: after;\nfoo {\n  a: $x;\n}",
    "a {\n  color: red;\n}\n\nfoo {\n  a: 1 2 before;\n}\n"
);
test!(
    variable_whitespace,
    "$a   :    1px   ;\na {\n  height: $a;\n}\n",
    "a {\n  height: 1px;\n}\n"
);
test!(
    style_after_variable,
    "$a: 1px;\na {\n  height: $a;\n  color: red;\n}\n",
    "a {\n  height: 1px;\n  color: red;\n}\n"
);
test!(
    literal_and_variable_as_val,
    "$a: 1px;\na {\n  height: 1 $a;\n}\n",
    "a {\n  height: 1 1px;\n}\n"
);
test!(
    literal_and_variable_as_var,
    "$a: 1px;\n$b: 1 $a;\na {\n  height: $b;\n}\n",
    "a {\n  height: 1 1px;\n}\n"
);
test!(
    eats_whitespace_after_variable_value,
    "a {\n  b {\n    $c: red;\n  }\n  color: red;\n}\n",
    "a {\n  color: red;\n}\n"
);
test!(
    variable_changes_through_new_ruleset,
    "a {\n  $c: red;\nb {\n    $c: blue;\n  }\n  color: $c;\n}\n",
    "a {\n  color: blue;\n}\n"
);
test!(
    nested_interpolation,
    "$a: red; a {\n  color: #{#{$a}};\n}\n",
    "a {\n  color: red;\n}\n"
);
test!(
    numbers_in_variable,
    "$var1: red; a {\n  color: $var1;\n}\n",
    "a {\n  color: red;\n}\n"
);
test!(
    default_var_after,
    "$a: red;\n$a: blue !default;\na {\n  color: $a;\n}",
    "a {\n  color: red;\n}\n"
);
test!(
    default_var_before,
    "$a: red !default;\n$a: blue;\na {\n  color: $a;\n}",
    "a {\n  color: blue;\n}\n"
);
test!(
    default_var_whitespace,
    "$a: red     !default          ;\na {\n  color: $a;\n}",
    "a {\n  color: red;\n}\n"
);
test!(
    default_var_inside_rule,
    "a {\n  $a: red;\n  $a: blue !default;\n  color: $a;\n}",
    "a {\n  color: red;\n}\n"
);
test!(
    interpolation_in_variable,
    "$a: #{red};\na {\n  color: $a\n}\n",
    "a {\n  color: red;\n}\n"
);
test!(
    variable_decl_doesnt_end_in_semicolon,
    "a {\n  $a: red\n}\n\nb {\n  color: blue;\n}\n",
    "b {\n  color: blue;\n}\n"
);
// TODO: blocked on properly emitting @charset
// right now, we emit @charset if a utf-8 character
// is found *anywhere*, but ideally we would only emit
// it if a utf-8 character is actually in the output
// test!(
//     unicode_in_variables,
//     "$vär: foo;\na {\n  color: $vär;\n}\n",
//     "a {\n  color: foo;\n}\n"
// );
test!(
    variable_does_not_include_interpolation,
    "$input: foo;\na {\n  color: $input#{\"literal\"};\n}\n",
    "a {\n  color: foo literal;\n}\n"
);
