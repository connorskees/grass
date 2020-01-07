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
        for rule in &s.rules {
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
    test!(selector_attribute_single_quotes, "[attr='val'] {\n}\n");
    test!(selector_attribute_double_quotes, "[attr=\"val\"] {\n}\n");
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
    test!(single_quoted_style_value, "a {\n  font: 'Open-Sans';\n}\n");
    test!(
        double_quoted_style_value,
        "a {\n  font: \"Open-Sans\";\n}\n"
    );
    test!(
        comma_style_value,
        "a {\n  font: Open-Sans, sans-serif;\n}\n"
    );
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
        basic_variable,
        "$height: 1px;\na {\n  height: $height;\n}\n",
        "a {\n  height: 1px;\n}\n"
    );
    test!(
        variable_redeclaration,
        "$a: 1px;\n$a: 2px;\na {\n  height: $a;\n}\n",
        "a {\n  height: 2px;\n}\n"
    );
    test!(
        variable_shadowing,
        "$a: 1px;\n$b: $a;\na {\n  height: $b;\n}\n",
        "a {\n  height: 1px;\n}\n"
    );
    test!(
        variable_shadowing_val_does_not_change,
        "$a: 1px;\n$b: $a; $a: 2px;\na {\n  height: $b;\n}\n",
        "a {\n  height: 1px;\n}\n"
    );
    test!(
        variable_shadowing_val_does_not_change_complex,
        "a {\n}\n$y: before;\n$x: 1 2 $y;\n$y: after;\nfoo {\n  a: $x;\n}",
        "a {\n}\nfoo {\n  a: 1 2 before;\n}\n"
    );
    test!(
        variable_whitespace,
        "$a   :    1px   ;\na {\n  height: $a;\n}\n",
        "a {\n  height: 1px;\n}\n"
    );
    test!(
        style_after_variable,
        "$a: 1px;\na {\n  height: $a;\n  color: red;\n}\n",
        "a {\n  height: 1px;\n  color: red;\n}\n"
    );
    test!(
        literal_and_variable_as_val,
        "$a: 1px;\na {\n  height: 1 $a;\n}\n",
        "a {\n  height: 1 1px;\n}\n"
    );
    test!(
        literal_and_variable_as_var,
        "$a: 1px;\n$b: 1 $a;\na {\n  height: $b;\n}\n",
        "a {\n  height: 1 1px;\n}\n"
    );

    test!(
        removes_single_line_comment,
        "// a { color: red }\na {\n  height: 1 1px;\n}\n",
        "a {\n  height: 1 1px;\n}\n"
    );

    test!(keyword_important, "a {\n  height: 1 !important;\n}\n");
    test!(
        keyword_important_uppercase,
        "a {\n  height: 1 !IMPORTANT;\n}\n",
        "a {\n  height: 1 !important;\n}\n"
    );
    test!(
        keyword_important_not_at_end,
        "a {\n  height: !important 1;\n}\n"
    );

    test!(
        combines_hyphens,
        "a {\n  foo: bar - baz;\n}\n",
        "a {\n  foo: bar-baz;\n}\n"
    );
    test!(does_not_combine_hyphens, "a {\n  foo: bar -baz;\n}\n");
    test!(
        ident_starts_with_hyphen,
        "a {\n  foo: -webkit-bar-baz;\n}\n"
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
