use std::io::{self, Write};

use crate::{RuleSet, Stmt, StyleSheet};

pub(crate) struct PrettyPrinter<W: Write> {
    buf: W,
    scope: usize,
}

impl<W: Write> PrettyPrinter<W> {
    pub(crate) fn new(buf: W) -> Self {
        PrettyPrinter { buf, scope: 0 }
    }

    /// Pretty print `crate::Stmt`
    /// Throws away super selectors and variables
    fn pretty_print_stmt(&mut self, stmt: &Stmt) -> io::Result<()> {
        let padding = vec![' '; self.scope * 2].iter().collect::<String>();
        match stmt {
            Stmt::MultilineComment(s) => writeln!(self.buf, "{}/*{}*/", padding, s)?,
            Stmt::RuleSet(RuleSet {
                selector, rules, ..
            }) => {
                writeln!(self.buf, "{}{} {{", padding, selector)?;
                self.scope += 1;
                for rule in rules {
                    self.pretty_print_stmt(rule)?;
                }
                writeln!(self.buf, "{}}}", padding)?;
                self.scope -= 1;
            }
            Stmt::Style(s) => {
                writeln!(self.buf, "{}{}", padding, s)?;
            }
        }
        Ok(())
    }

    /// Pretty print SCSS
    ///
    /// The result should be an exact copy of the SCSS input
    /// Empty rules are included
    pub fn pretty_print(&mut self, s: &StyleSheet) -> io::Result<()> {
        for rule in &s.0 {
            self.pretty_print_stmt(rule)?;
        }
        Ok(())
    }

    /// Pretty print `crate::Stmt`
    /// Keeps super selectors
    fn pretty_print_stmt_preserve_super_selectors(&mut self, stmt: &Stmt) -> io::Result<()> {
        let padding = vec![' '; self.scope * 2].iter().collect::<String>();
        match stmt {
            Stmt::MultilineComment(s) => writeln!(self.buf, "/*{}*/", s)?,
            Stmt::RuleSet(RuleSet {
                selector,
                rules,
                super_selector,
            }) => {
                writeln!(
                    self.buf,
                    "{}{} {{",
                    padding,
                    super_selector.clone().zip(selector.clone())
                )?;
                self.scope += 1;
                for rule in rules {
                    self.pretty_print_stmt(rule)?;
                }
                writeln!(self.buf, "{}}}", padding)?;
                self.scope -= 1;
            }
            Stmt::Style(s) => {
                writeln!(self.buf, "{}{}", padding, s)?;
            }
        }
        Ok(())
    }

    /// Pretty print a special form of SCSS that shows what the full selectors are for children
    /// Meant for debugging
    /// Empty rules are included
    pub(crate) fn pretty_print_preserve_super_selectors(
        &mut self,
        s: &StyleSheet,
    ) -> io::Result<()> {
        for rule in &s.0 {
            self.pretty_print_stmt_preserve_super_selectors(rule)?;
        }
        writeln!(self.buf)?;
        Ok(())
    }
}

#[cfg(test)]
mod test_scss {
    use super::StyleSheet;
    macro_rules! test {
        ($func:ident, $input:literal) => {
            #[test]
            fn $func() {
                let mut buf = Vec::new();
                StyleSheet::new($input)
                    .expect(concat!("failed to parse on ", $input))
                    .pretty_print(&mut buf)
                    .expect(concat!("failed to pretty print on ", $input));
                assert_eq!(
                    String::from($input),
                    String::from_utf8(buf).expect("produced invalid utf8")
                );
            }
        };
        ($func:ident, $input:literal, $output:literal) => {
            #[test]
            fn $func() {
                let mut buf = Vec::new();
                StyleSheet::new($input)
                    .expect(concat!("failed to parse on ", $input))
                    .pretty_print(&mut buf)
                    .expect(concat!("failed to pretty print on ", $input));
                assert_eq!(
                    String::from($output),
                    String::from_utf8(buf).expect("produced invalid utf8")
                );
            }
        };
    }

    test!(empty, "");
    test!(basic_nesting, "a {\n  b {\n  }\n}\n");
    test!(mul_nesting, "a, b {\n  a, b {\n  }\n}\n");
    test!(basic_style, "a {\n  color: red;\n}\n");
    test!(two_styles, "a {\n  color: red;\n  color: blue;\n}\n");
    test!(
        nested_style_in_parent,
        "a {\n  color: red;\n  b {\n  }\n}\n"
    );
    test!(
        nested_style_in_child,
        "a {\n  b {\n    color: red;\n  }\n}\n"
    );
    test!(
        nested_style_in_both,
        "a {\n  color: red;\n  b {\n    color: red;\n  }\n}\n"
    );
    test!(
        deeply_nested_selector,
        "\
a {
  b {
    c {
      d {
        e {
          f {
            g {
              h {
                i {
                }
              }
            }
          }
        }
      }
    }
  }
}
"
    );
}
