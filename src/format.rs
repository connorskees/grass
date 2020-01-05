use std::io::{self, Write};

use crate::{RuleSet, Stmt, Style, StyleSheet};

pub(crate) struct PrettyPrinter<W: Write> {
    buf: W,
    scope: usize,
}

impl<W: Write> PrettyPrinter<W> {
    pub(crate) fn new(buf: W) -> Self {
        PrettyPrinter { buf, scope: 0 }
    }

    /// Pretty print `crate::Stmt`
    /// Throws away super selectors
    fn pretty_print_stmt(&mut self, stmt: &Stmt) -> io::Result<()> {
        let padding = vec![' '; self.scope * 2].iter().collect::<String>();
        match stmt {
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
            Stmt::Style(Style { property, value }) => {
                writeln!(
                    self.buf,
                    "{}{}: {};",
                    padding,
                    property,
                    value
                        .iter()
                        .map(ToString::to_string)
                        .collect::<Vec<String>>()
                        .join(" ")
                )?;
            }
        }
        Ok(())
    }

    /// Pretty print SCSS
    ///
    /// The result should be an exact copy of the SCSS input
    /// Empty rules are included
    pub fn pretty_print(&mut self, s: &StyleSheet) -> io::Result<()> {
        for rule in &s.rules {
            self.pretty_print_stmt(rule)?;
        }
        Ok(())
    }

    /// Pretty print `crate::Stmt`
    /// Keeps super selectors
    fn pretty_print_stmt_preserve_super_selectors(&mut self, stmt: &Stmt) -> io::Result<()> {
        let padding = vec![' '; self.scope * 2].iter().collect::<String>();
        match stmt {
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
            Stmt::Style(Style { property, value }) => {
                writeln!(
                    self.buf,
                    "{}{}: {};",
                    padding,
                    property,
                    value
                        .iter()
                        .map(ToString::to_string)
                        .collect::<Vec<String>>()
                        .join(" ")
                )?;
            }
        }
        Ok(())
    }

    /// Pretty print a special form of SCSS that shows what the full selectors are for children
    /// Meant for debugging
    /// Empty rules are included
    pub(crate) fn pretty_print_preserve_super_selectors(&mut self, s: &StyleSheet) -> io::Result<()> {
        for rule in &s.rules {
            self.pretty_print_stmt(rule)?;
        }
        writeln!(self.buf)?;
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::StyleSheet;
    macro_rules! test {
        ($func:ident, $input:literal) => {
            #[test]
            fn $func() {
                let mut buf = Vec::new();
                StyleSheet::new($input)
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
    test!(ident_with_num, "el1 {\n}\n");

    test!(selector_element, "a {\n}\n");
    test!(selector_id, "#id {\n}\n");
    test!(selector_class, ".class {\n}\n");
    test!(selector_el_descendant, "a a {\n}\n");
    test!(selector_universal, "* {\n}\n");
    test!(selector_el_class_and, "a.class {\n}\n");
    test!(selector_el_id_and, "a#class {\n}\n");
    test!(selector_el_class_descendant, "a .class {\n}\n");
    test!(selector_el_id_descendant, "a #class {\n}\n");
    test!(selector_el_universal_descendant, "a * {\n}\n");
    test!(selector_universal_el_descendant, "* a {\n}\n");
    test!(selector_attribute_any, "[attr] {\n}\n");
    test!(selector_attribute_equals, "[attr=val] {\n}\n");
    test!(selector_attribute_in, "[attr~=val] {\n}\n");
    test!(
        selector_attribute_begins_hyphen_or_exact,
        "[attr|=val] {\n}\n"
    );
    test!(selector_attribute_starts_with, "[attr^=val] {\n}\n");
    test!(selector_attribute_ends_with, "[attr$=val] {\n}\n");
    test!(selector_attribute_contains, "[attr*=val] {\n}\n");
    test!(selector_el_attribute_and, "a[attr] {\n}\n");
    test!(selector_el_attribute_descendant, "a [attr] {\n}\n");
    test!(selector_el_mul_el, "a, b {\n}\n");
    test!(selector_el_immediate_child_el, "a > b {\n}\n");
    test!(selector_el_following_el, "a + b {\n}\n");
    test!(selector_el_preceding_el, "a ~ b {\n}\n");
    test!(selector_pseudo, ":pseudo {\n}\n");
    test!(selector_el_pseudo_and, "a:pseudo {\n}\n");
    test!(selector_el_pseudo_descendant, "a :pseudo {\n}\n");

    test!(basic_style, "a {\n  color: red;\n}\n");
    test!(two_styles, "a {\n  color: red;\n  color: blue;\n}\n");
    test!(
        multiline_style,
        "a {\n  color: red\n  blue;\n}\n",
        "a {\n  color: red blue;\n}\n"
    );
    test!(hyphenated_style_property, "a {\n  font-family: Arial;\n}\n");
    test!(hyphenated_style_value, "a {\n  color: Open-Sans;\n}\n");
    test!(
        space_separated_style_value,
        "a {\n  border: solid red;\n}\n"
    );
    test!(
        single_quoted_style_value,
        "a {\n  font: 'Open-Sans';\n}\n",
        "a {\n  font: Open-Sans;\n}\n"
    );
    test!(
        double_quoted_style_value,
        "a {\n  font: \"Open-Sans\";\n}\n",
        "a {\n  font: Open-Sans;\n}\n"
    );
    // test!(comma_style_value, "a {\n  font: Open-Sans, sans-serif;\n}\n");
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

    test!(unit_none, "a {\n  height: 1;\n}\n");
    test!(unit_not_attached, "a {\n  height: 1 px;\n}\n");
    test!(unit_px, "a {\n  height: 1px;\n}\n");
    test!(unit_em, "a {\n  height: 1em;\n}\n");
    test!(unit_rem, "a {\n  height: 1rem;\n}\n");
    test!(unit_percent, "a {\n  height: 1%;\n}\n");
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
