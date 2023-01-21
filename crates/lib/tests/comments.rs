#[macro_use]
mod macros;

test!(
    removes_inner_comments,
    "a {\n  color: red/* hi */;\n}\n",
    "a {\n  color: red;\n}\n"
);
test!(
    removes_inner_comments_whitespace,
    "a {\n  color: red    /* hi */;\n}\n",
    "a {\n  color: red;\n}\n"
);
test!(
    removes_comment_before_style,
    "a {\n  color: /**/red;\n}\n",
    "a {\n  color: red;\n}\n"
);
test!(
    preserves_outer_comments_before,
    "a {\n  /* hi */\n  color: red;\n}\n",
    "a {\n  /* hi */\n  color: red;\n}\n"
);
test!(
    preserves_outer_comments_after,
    "a {\n  color: red;\n  /* hi */\n}\n",
    "a {\n  color: red;\n  /* hi */\n}\n"
);
test!(
    preserves_outer_comments_two,
    "a {\n  /* foo */\n  /* bar */\n  color: red;\n}\n",
    "a {\n  /* foo */\n  /* bar */\n  color: red;\n}\n"
);
test!(two_silent_comments_followed_by_eof, "//\n//\n", "");
test!(
    preserves_toplevel_comment_before,
    "/* foo */\na {\n  color: red;\n}\n",
    "/* foo */\na {\n  color: red;\n}\n"
);
test!(
    preserves_toplevel_comment_after,
    "a {\n  color: red;\n}\n/* foo */\n",
    "a {\n  color: red;\n}\n\n/* foo */\n"
);
test!(
    #[ignore = "we use the old form of comment writing"]
    preserves_trailing_comments,
    "a { /**/
      color: red; /**/
    } /**/",
    "a { /**/\n  color: red; /**/\n} /**/\n"
);
test!(
    removes_single_line_comment,
    "// a { color: red }\na {\n  height: 1 1px;\n}\n",
    "a {\n  height: 1 1px;\n}\n"
);
test!(
    converts_form_feed_in_comment,
    "a {\n  /*  \x0C*/ color: red;\n}\n",
    "a {\n  /*  \n*/\n  color: red;\n}\n"
);
test!(
    converts_crlf_in_comment,
    "a {\n  /*  \r\n*/ color: red;\n}\n",
    "a {\n  /*  \n*/\n  color: red;\n}\n"
);
test!(
    closing_curly_brace_in_comment,
    "a {\n  color: 1 + // flang }\n  blang }",
    "a {\n  color: 1blang;\n}\n"
);
test!(
    converts_cr_in_comment,
    "a {\n  /*  \r*/ color: red;\n}\n",
    "a {\n  /*  \n*/\n  color: red;\n}\n"
);
test!(
    interpolation_in_multiline_comment,
    "$a: foo;/* interpolation #{1 + 1} in #{$a} comments */",
    "/* interpolation 2 in foo comments */\n"
);
test!(
    preserves_relative_whitespace,
    "  /*!\n    * a\n    */\n",
    "/*!\n  * a\n  */\n"
);
test!(
    preserves_relative_whitespace_for_each_line,
    "  /*!\n      * a\n    */\n",
    "/*!\n    * a\n  */\n"
);
test!(
    triple_star_in_selector,
    "a/***/ {x: y} b { color: red; }",
    "a {\n  x: y;\n}\n\nb {\n  color: red;\n}\n"
);
test!(
    sass_spec__css_comments_multiple_stars,
    "a /***/ b {x: y}
    a /****/ b {x: y}
    a /* **/ b {x: y}
    a /** */ b {x: y}",
    "a b {\n  x: y;\n}\n\na b {\n  x: y;\n}\n\na b {\n  x: y;\n}\n\na b {\n  x: y;\n}\n"
);
test!(
    comment_has_newline_above_and_not_below_when_between_two_rulesets,
    r"a {
      color: red;
    }
    
    /**/
    
    a {
      color: green;
    }",
    "a {\n  color: red;\n}\n\n/**/\na {\n  color: green;\n}\n"
);
test!(
    no_extra_newline_between_two_comments,
    r"/**/
      /**/",
    "/**/\n/**/\n"
);
test!(
    no_extra_newline_when_after_placeholder_selector,
    r"a {
      color: red;
    
      %b {
        color: red;
      }
    }
    
    /**/
    
    c {
      color: red;
    }
    
    /**/",
    "a {\n  color: red;\n}\n/**/\nc {\n  color: red;\n}\n\n/**/\n"
);
test!(
    extra_newline_when_after_regular_selector_after_placeholder_selector,
    r"a {
      color: red;
    
      %b {
        color: red;
      }
    
      d {
        color: red;
      }
    }
    
    /**/
    
    c {
      color: red;
    }
    
    /**/",
    "a {\n  color: red;\n}\na d {\n  color: red;\n}\n\n/**/\nc {\n  color: red;\n}\n\n/**/\n"
);
test!(
    #[ignore = "we use the old form of comment writing"]
    same_line_loud_comments_are_emitted_on_same_line_of_ruleset_brackets,
    "a {/**/}",
    "a { /**/ }\n"
);
test!(silent_comment_as_child, "a {\n// silent\n}\n", "");
test!(single_hash_in_loud_comment, "/*#*/", "/*#*/\n");
error!(unclosed_loud_comment, "/*", "Error: expected more input.");
