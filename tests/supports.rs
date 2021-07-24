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
    newline_between_styles_inside,
    "@supports (-ms-ime-align: auto) {
      a {
        color: red;
      }
    
      b {
        color: green;
      }
    }",
    "@supports (-ms-ime-align: auto) {\n  a {\n    color: red;\n  }\n\n  b {\n    color: green;\n  }\n}\n"
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
    "@supports (position: sticky) {\n  a {\n    color: red;\n  }\n\n  @media (min-width: 576px) {\n    a {\n      color: red;\n    }\n\n    a {\n      color: red;\n    }\n  }\n  a {\n    color: red;\n  }\n}\n"
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
