#[macro_use]
mod macros;

test!(
    basic_unknown_at_rule,
    "@foo {\n  a {\n    color: red;\n  }\n}\n",
    "@foo {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    unknown_at_rule_no_selector,
    "@foo {\n  color: red;\n}\n",
    "@foo {\n  color: red;\n}\n"
);
test!(unknown_at_rule_no_body, "@foo;\n", "@foo;\n");
test!(unknown_at_rule_empty_body, "@foo {}\n", "@foo {}\n");
test!(unknown_at_rule_no_body_eof, "@foo", "@foo;\n");
test!(
    unknown_at_rule_interpolated_eof_no_body,
    "@#{()if(0,0<0,0)}",
    "@false;\n"
);
test!(nothing_after_hash, "@foo #", "@foo #;\n");
test!(
    style_following,
    "@foo (a: b) {
        a {
            color: red;
        }
    }

    a {
        color: green;
    }",
    "@foo (a: b) {\n  a {\n    color: red;\n  }\n}\na {\n  color: green;\n}\n"
);
test!(
    no_semicolon_no_params_no_body,
    "a {
      @b
    }
    
    a {
      color: red;
    }",
    "a {\n  @b;\n}\n\na {\n  color: red;\n}\n"
);
test!(
    no_semicolon_has_params_no_body,
    "a {
      @foo bar
    }

    a {
      color: red;
    }",
    "a {\n  @foo bar;\n}\n\na {\n  color: red;\n}\n"
);
test!(
    no_body_remains_inside_style_rule,
    "a {
      @box-shadow: $btn-focus-box-shadow, $btn-active-box-shadow;
    }
    
    a {
      color: red;
    }",
    "a {\n  @box-shadow : $btn-focus-box-shadow, $btn-active-box-shadow;\n}\n\na {\n  color: red;\n}\n"
);
test!(
    empty_body_moves_outside_style_rule,
    "a {
      @b {}
    }
    
    a {
      color: red;
    }",
    "@b {}\n\na {\n  color: red;\n}\n"
);
test!(
    parent_selector_moves_inside_rule,
    "a {
     @foo {
       b: c
     }
    }",
    "@foo {\n  a {\n    b: c;\n  }\n}\n"
);
test!(
    parent_selector_moves_inside_rule_and_is_parent_to_inner_selector,
    "a {
     @foo {
       f {
         b: c
       }
     }
    }",
    "@foo {\n  a f {\n    b: c;\n  }\n}\n"
);
test!(
    params_contain_silent_comment_and_semicolon,
    "a {
      @box-shadow: $btn-focus-box-shadow, // $btn-active-box-shadow;
    }",
    "a {\n  @box-shadow : $btn-focus-box-shadow, // $btn-active-box-shadow;;\n}\n"
);
test!(contains_multiline_comment, "@foo /**/;\n", "@foo;\n");
error!(
    unknown_at_rule_inside_declaration_body,
    "@mixin foo {
        @foo;
    }

    a {
        color: {
            @include foo;
        }
    }",
    "Error: At-rules may not be used within nested declarations."
);

// todo: test scoping in rule
