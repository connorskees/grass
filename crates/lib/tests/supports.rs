#[macro_use]
mod macros;

test!(
    style_following,
    "@supports (a: b) {
        a {
            color: red;
        }
    }

    a {
        color: green;
    }",
    "@supports (a: b) {\n  a {\n    color: red;\n  }\n}\na {\n  color: green;\n}\n"
);
test!(
    no_newline_between_styles_inside,
    "@supports (-ms-ime-align: auto) {
      a {
        color: red;
      }
    
      b {
        color: green;
      }
    }",
    "@supports (-ms-ime-align: auto) {\n  a {\n    color: red;\n  }\n  b {\n    color: green;\n  }\n}\n"
);
test!(
    no_newline_after_media,
    "@supports (position: sticky) {
      a {
        color: red;
      }
    
      @media (min-width: 576px) {
        a {
          color: red;
        }
    
        a {
          color: red;
        }
      }
    
      a {
        color: red;
      }
    }",
    "@supports (position: sticky) {\n  a {\n    color: red;\n  }\n  @media (min-width: 576px) {\n    a {\n      color: red;\n    }\n    a {\n      color: red;\n    }\n  }\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    newline_after_supports_when_inside_style_rule,
    "a {
      @supports (position: sticky) {
        color: red;
      }
    }
    
    a {
      color: red;
    }",
    "@supports (position: sticky) {\n  a {\n    color: red;\n  }\n}\n\na {\n  color: red;\n}\n"
);
test!(
    supports_nested_inside_media,
    "@media foo {
      @supports (a: b) {
          a {
              color: red;
          }
      }
  }",
    "@media foo {\n  @supports (a: b) {\n    a {\n      color: red;\n    }\n  }\n}\n"
);
test!(
    supports_nested_inside_style_rule,
    "a {
    @supports (a: b) {
        b {
            color: red;
        }
    }
  }",
    "@supports (a: b) {\n  a b {\n    color: red;\n  }\n}\n"
);
test!(
    supports_nested_inside_media_nested_inside_style_rule,
    "a {
    @media foo {
        @supports (a: b) {
            b {
                color: red;
            }
        }
    }
  }",
    "@media foo {\n  @supports (a: b) {\n    a b {\n      color: red;\n    }\n  }\n}\n"
);
test!(
    media_nested_inside_supports,
    "@supports (a: b) {
    @media foo {
        a {
            color: red;
        }
    }
  }",
    "@supports (a: b) {\n  @media foo {\n    a {\n      color: red;\n    }\n  }\n}\n"
);
test!(
    supports_nested_inside_supports,
    "@supports (a: b) {
     @supports (c: d) {
         a {
             color: red;
         }
     }
   }",
    "@supports (a: b) {\n  @supports (c: d) {\n    a {\n      color: red;\n    }\n  }\n}\n"
);
test!(
    supports_different_operation_is_in_parens,
    "@supports (a: b) and ((c: d) or (e: f)) {
      a {
          color: red;
      }
    }",
    "@supports (a: b) and ((c: d) or (e: f)) {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    supports_removed_if_all_children_invisible,
    "@supports (a: b) {
      %a {}
    }",
    ""
);
test!(supports_empty_body, "@supports (a: b) {}", "");
test!(
    calculation_not_in_declaration,
    "@supports (calc(1 + 1)) {
        a {
            color: red;
        }
    }",
    "@supports (calc(1 + 1)) {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    ident_addition_on_rhs_of_declaration,
    "@supports (a: a + b) {
        a {
            color: red;
        }
    }",
    "@supports (a: ab) {\n  a {\n    color: red;\n  }\n}\n"
);
test!(
    calculation_on_rhs_of_declaration,
    "@supports (a: calc(1px + 1px)) {
        a {
            color: red;
        }
    }",
    "@supports (a: calc(1px + 1px)) {\n  a {\n    color: red;\n  }\n}\n"
);
error!(
    supports_inside_declaration_body,
    "@mixin foo() {
        @supports (foo) {}
    }

    a {
        color: {
            @include foo();
        }
    }",
    "Error: Supports rules may not be used within nested declarations."
);
