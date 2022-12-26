use grass::InputSyntax;

#[macro_use]
mod macros;

test!(
    function_call,
    "a {
        color: rotate(-45deg);
    }",
    "a {\n  color: rotate(-45deg);\n}\n",
    grass::Options::default().input_syntax(InputSyntax::Css)
);
test!(
    retains_null,
    "a {
        color: null;
    }",
    "a {\n  color: null;\n}\n",
    grass::Options::default().input_syntax(InputSyntax::Css)
);
test!(
    does_not_evaluate_and,
    "a {
        color: 1 and 2;
    }",
    "a {\n  color: 1 and 2;\n}\n",
    grass::Options::default().input_syntax(InputSyntax::Css)
);
test!(
    does_not_evaluate_or,
    "a {
        color: 1 or 2;
    }",
    "a {\n  color: 1 or 2;\n}\n",
    grass::Options::default().input_syntax(InputSyntax::Css)
);
test!(
    does_not_evaluate_not,
    "a {
        color: not 2;
        color: not true;
        color: not false;
    }",
    "a {\n  color: not 2;\n  color: not true;\n  color: not false;\n}\n",
    grass::Options::default().input_syntax(InputSyntax::Css)
);
