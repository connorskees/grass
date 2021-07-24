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
