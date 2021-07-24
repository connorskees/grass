//! Tests that exist only to verify the printing of whitespace (largely, newlines)

#[macro_use]
mod macros;

test!(
    // this is a bug in dart-sass that we must emulate.
    no_newline_between_ruleset_when_last_ruleset_is_empty,
    "a {
      color: red;
    
      b {
        color: red;
      }
    
      c {
      }
    }
    
    d {
      color: red;
    }",
    "a {\n  color: red;\n}\na b {\n  color: red;\n}\nd {\n  color: red;\n}\n"
);
test!(
    // this is a bug in dart-sass that we must emulate.
    no_newline_between_ruleset_when_last_ruleset_is_empty_from_extend,
    "a {
      color: red;
    
      %b {
        color: red;
      }
    
      c {
        @extend %b;
      }
    }
    
    d {
      color: red;
    }",
    "a {\n  color: red;\n}\na c {\n  color: red;\n}\nd {\n  color: red;\n}\n"
);
