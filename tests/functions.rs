#![cfg(test)]

#[macro_use]
mod macros;

test!(
    return_num,
    "@function a() {\n  @return 1;\n}\n\nb {\ncolor: a();\n}\n",
    "b {\n  color: 1;\n}\n"
);
test!(
    return_spaced_list,
    "@function a() {\n  @return a b;\n}\n\nb {\ncolor: a();\n}\n",
    "b {\n  color: a b;\n}\n"
);
test!(
    single_arg,
    "@function a($c) {\n  @return $c;\n}\n\nb {\ncolor: a(1);\n}\n",
    "b {\n  color: 1;\n}\n"
);
test!(
    return_variable,
    "@function a($a) {\n  @return $a;\n}\n\nb {\ncolor: a(1);\n}\n",
    "b {\n  color: 1;\n}\n"
);
// test!(
//     return_no_semicolon,
//     "@function a() {\n  @return 1\n}\n\nb {\ncolor: a();\n}\n",
//     "b {\n  color: 1;\n}\n"
// );