use grass::StyleSheet;

#[cfg(test)]
macro_rules! test {
    ($func:ident, $input:literal) => {
        #[test]
        fn $func() {
            let mut buf = Vec::new();
            StyleSheet::new($input)
                .expect(concat!("failed to parse on ", $input))
                .print_as_css(&mut buf)
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
                .print_as_css(&mut buf)
                .expect(concat!("failed to pretty print on ", $input));
            assert_eq!(
                String::from($output),
                String::from_utf8(buf).expect("produced invalid utf8")
            );
        }
    };
}

#[cfg(test)]
mod test_variables {
    use super::*;
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
        "a {\n  color: red;\n}\n$y: before;\n$x: 1 2 $y;\n$y: after;\nfoo {\n  a: $x;\n}",
        "a {\n  color: red;\n}\nfoo {\n  a: 1 2 before;\n}\n"
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
        eats_whitespace_after_variable_value,
        "a {\n  b {\n    $c: red;\n  }\n  color: red;\n}\n",
        "a {\n  color: red;\n}\n"
    );
    test!(
        variable_changes_through_new_ruleset,
        "a {\n  $c: red;\nb {\n    $c: blue;\n  }\n  color: $c;\n}\n",
        "a {\n  color: blue;\n}\n"
    );
    test!(
        nested_interpolation,
        "$a: red; a {\n  color: #{#{$a}};\n}\n",
        "a {\n  color: red;\n}\n"
    );
    test!(
        numbers_in_variable,
        "$var1: red; a {\n  color: $var1;\n}\n",
        "a {\n  color: red;\n}\n"
    );
    test!(
        default_var_after,
        "$a: red;\n$a: blue !default;\na {\n  color: $a;\n}",
        "a {\n  color: red;\n}\n"
    );
    test!(
        default_var_before,
        "$a: red !default;\n$a: blue;\na {\n  color: $a;\n}",
        "a {\n  color: blue;\n}\n"
    );
    test!(
        default_var_whitespace,
        "$a: red     !default          ;\na {\n  color: $a;\n}",
        "a {\n  color: red;\n}\n"
    );
    test!(
        default_var_inside_rule,
        "a {\n  $a: red;\n  $a: blue !default;\n  color: $a;\n}",
        "a {\n  color: red;\n}\n"
    );
    test!(
        interpolation_in_variable,
        "$a: #{red};\na {\n  color: $a\n}\n",
        "a {\n  color: red;\n}\n"
    );
    test!(
        variable_decl_doesnt_end_in_semicolon,
        "a {\n  $a: red\n}\n\nb {\n  color: blue;\n}\n",
        "b {\n  color: blue;\n}\n"
    );
}

#[cfg(test)]
mod test_selectors {
    use super::StyleSheet;
    test!(
        selector_nesting_el_mul_el,
        "a, b {\n  a, b {\n  color: red\n}\n}\n",
        "a a, a b, b a, b b {\n  color: red;\n}\n"
    );
    test!(selector_element, "a {\n  color: red;\n}\n");
    test!(selector_id, "#id {\n  color: red;\n}\n");
    test!(selector_class, ".class {\n  color: red;\n}\n");
    test!(selector_el_descendant, "a a {\n  color: red;\n}\n");
    test!(selector_universal, "* {\n  color: red;\n}\n");
    test!(selector_el_class_and, "a.class {\n  color: red;\n}\n");
    test!(selector_el_id_and, "a#class {\n  color: red;\n}\n");
    test!(
        selector_el_class_descendant,
        "a .class {\n  color: red;\n}\n"
    );
    test!(selector_el_id_descendant, "a #class {\n  color: red;\n}\n");
    test!(
        selector_el_universal_descendant,
        "a * {\n  color: red;\n}\n"
    );
    test!(
        selector_universal_el_descendant,
        "* a {\n  color: red;\n}\n"
    );

    test!(selector_attribute_any, "[attr] {\n  color: red;\n}\n");
    test!(
        selector_attribute_any_lower_case_insensitive,
        "[attr=val i] {\n  color: red;\n}\n"
    );
    test!(
        selector_attribute_any_upper_case_insensitive,
        "[attr=val I] {\n  color: red;\n}\n"
    );
    test!(
        selector_attribute_arbitrary_modifier,
        "[attr=val c] {\n  color: red;\n}\n"
    );
    test!(
        selector_attribute_i_in_attr,
        "[atitr=val] {\n  color: red;\n}\n"
    );
    test!(
        selector_attribute_i_in_val,
        "[attr=vail] {\n  color: red;\n}\n"
    );
    test!(
        selector_attribute_whitespace,
        "[attr   *=   val      ] {\n  color: red;\n}\n",
        "[attr*=val] {\n  color: red;\n}\n"
    );
    test!(
        selector_attribute_equals,
        "[attr=val] {\n  color: red;\n}\n"
    );
    test!(
        selector_attribute_single_quotes,
        "[attr='val'] {\n  color: red;\n}\n"
    );
    test!(
        selector_attribute_double_quotes,
        "[attr=\"val\"] {\n  color: red;\n}\n"
    );
    test!(selector_attribute_in, "[attr~=val] {\n  color: red;\n}\n");
    test!(
        selector_attribute_begins_hyphen_or_exact,
        "[attr|=val] {\n  color: red;\n}\n"
    );
    test!(
        selector_attribute_starts_with,
        "[attr^=val] {\n  color: red;\n}\n"
    );
    test!(
        selector_attribute_ends_with,
        "[attr$=val] {\n  color: red;\n}\n"
    );
    test!(
        selector_attribute_contains,
        "[attr*=val] {\n  color: red;\n}\n"
    );
    test!(selector_el_attribute_and, "a[attr] {\n  color: red;\n}\n");
    test!(
        selector_el_attribute_descendant,
        "a [attr] {\n  color: red;\n}\n"
    );
    test!(selector_el_mul_el, "a, b {\n  color: red;\n}\n");
    test!(
        selector_el_immediate_child_el,
        "a > b {\n  color: red;\n}\n"
    );
    test!(selector_el_following_el, "a + b {\n  color: red;\n}\n");
    test!(selector_el_preceding_el, "a ~ b {\n  color: red;\n}\n");
    test!(selector_pseudo, ":pseudo {\n  color: red;\n}\n");
    test!(selector_el_and_pseudo, "a:pseudo {\n  color: red;\n}\n");
    test!(
        selector_el_pseudo_descendant,
        "a :pseudo {\n  color: red;\n}\n"
    );
    test!(
        selector_pseudo_el_descendant,
        ":pseudo a {\n  color: red;\n}\n"
    );
    test!(
        selector_pseudo_paren_comma,
        ":pseudo(a, b, c) {\n  color: red;\n}\n"
    );
    test!(
        selector_pseudo_paren_space,
        ":pseudo(a b c) {\n  color: red;\n}\n"
    );
    test!(
        selector_el_pseudo_paren_and,
        "a:pseudo(a, b, c) {\n  color: red;\n}\n"
    );
    test!(
        selector_el_pseudo_paren_descendant,
        "a :pseudo(a, b, c) {\n  color: red;\n}\n"
    );
    test!(
        selector_pseudo_paren_el_descendant,
        ":pseudo(a, b, c) a {\n  color: red;\n}\n"
    );
    test!(
        selector_pseudo_paren_el_nested,
        "a {\n  :pseudo(a, b, c) {\n  color: red;\n  }\n}\n",
        "a :pseudo(a, b, c) {\n  color: red;\n}\n"
    );
    test!(selector_mul, "a, b {\n  color: red;\n}\n");
    test!(
        outer_ampersand,
        "a, b {\n& c {\n  color: red;\n}\n}\n",
        "a c, b c {\n  color: red;\n}\n"
    );
    test!(
        inner_ampersand,
        "a, b {\na & c {\n  color: red;\n}\n}\n",
        "a a c, a b c {\n  color: red;\n}\n"
    );
    test!(
        ampersand_multiple_whitespace,
        " a  ,  b   {\n&c {\n  color: red;\n}\n}\n",
        "ac, bc {\n  color: red;\n}\n"
    );
    test!(
        ampersand_alone,
        "a, b {\n& {\n  color: red;\n}\n}\n",
        "a, b {\n  color: red;\n}\n"
    );
    test!(
        bem_dash_dash_selector,
        "a {\n&--b {\n  color: red;\n}\n}\n",
        "a--b {\n  color: red;\n}\n"
    );
    // test!(
    //     bem_underscore_selector,
    //     "a {\n&__b {\n  color: red;\n}\n}\n",
    //     "a__b {\n  color: red;\n}\n"
    // );
    test!(
        selector_interpolation_start,
        "#{a}bc {\n  color: red;\n}\n",
        "abc {\n  color: red;\n}\n"
    );
    test!(
        selector_interpolation_middle,
        "a#{b}c {\n  color: red;\n}\n",
        "abc {\n  color: red;\n}\n"
    );
    test!(
        selector_interpolation_end,
        "ab#{c} {\n  color: red;\n}\n",
        "abc {\n  color: red;\n}\n"
    );
    test!(
        selector_interpolation_variable,
        "$a: foo;\nab#{$a} {\n  color: red;\n}\n",
        "abfoo {\n  color: red;\n}\n"
    );
    test!(
        selector_interpolation_super_selector,
        "a {\nb #{&} { color: red; }}",
        "a b a {\n  color: red;\n}\n"
    );
    test!(
        selector_interpolation_super_selector_root_postfix,
        "a#{&} {\nb { color: red; }}",
        "a b {\n  color: red;\n}\n"
    );
    test!(
        selector_interpolation_super_selector_root_prefix,
        "#{&}a {\nb { color: red; }}",
        "a b {\n  color: red;\n}\n"
    );
    test!(
        selector_whitespace,
        "  a  >  b  ,  c  ~  d  e  .f  #g  :h  i.j  [  k  ]  { color: red }",
        "a > b, c ~ d e .f #g :h i.j [k] {\n  color: red;\n}\n"
    );
    test!(
        comment_between_selectors,
        "a /* foo */ b {\n  color: red;\n}\n",
        "a b {\n  color: red;\n}\n"
    );
    test!(
        interpolates_comma,
        "$x: oo, ba;\nf#{$x}r {\n  baz {\n    color: red;\n  }\n}\n",
        "foo baz, bar baz {\n  color: red;\n}\n"
    );
    test!(
        extra_commas,
        "div,, , span, ,, {\n  color: red;\n}\n",
        "div, span {\n  color: red;\n}\n"
    );
}

#[cfg(test)]
mod test_units {
    use super::StyleSheet;
    test!(unit_none, "a {\n  height: 1;\n}\n");
    test!(unit_not_attached, "a {\n  height: 1 px;\n}\n");
    test!(unit_px, "a {\n  height: 1px;\n}\n");
    test!(unit_em, "a {\n  height: 1em;\n}\n");
    test!(unit_rem, "a {\n  height: 1rem;\n}\n");
    test!(unit_percent, "a {\n  height: 1%;\n}\n");
}

#[cfg(test)]
mod test_comments {
    use super::StyleSheet;
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
        preserves_outer_comments_before,
        "a {\n  /* hi */\n  color: red;\n}\n"
    );
    test!(
        preserves_outer_comments_after,
        "a {\n  color: red;\n  /* hi */\n}\n"
    );
    test!(
        preserves_outer_comments_two,
        "a {\n  /* foo */\n  /* bar */\n  color: red;\n}\n"
    );
    test!(
        preserves_toplevel_comment_before,
        "/* foo */\na {\n  color: red;\n}\n"
    );
    test!(
        preserves_toplevel_comment_after,
        "a {\n  color: red;\n}\n/* foo */\n"
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
        converts_cr_in_comment,
        "a {\n  /*  \r*/ color: red;\n}\n",
        "a {\n  /*  \n*/\n  color: red;\n}\n"
    );
}

#[cfg(test)]
mod test_styles {
    use super::StyleSheet;
    test!(basic_style, "a {\n  color: red;\n}\n");
    test!(two_styles, "a {\n  color: red;\n  color: blue;\n}\n");
    test!(
        two_inner_rulesets,
        "a {\n  b {\n  color: red;\n}\n  c {\n  color: white;\n}\n}\n",
        "a b {\n  color: red;\n}\na c {\n  color: white;\n}\n"
    );
    test!(
        two_rulesets,
        "a {\n  color: red;\n}\nc {\n  color: white;\n}\n"
    );
    test!(
        two_rulesets_first_no_semicolon,
        "a {\n  color: red\n}\nc {\n  color: white;\n}\n",
        "a {\n  color: red;\n}\nc {\n  color: white;\n}\n"
    );
    test!(
        two_inner_outer_rulesets,
        "a {\n  b {\n  color: red;\n}\n  c {\n  color: white;\n}\n}\na {\n  b {\n  color: red;\n}\n  c {\n  color: white;\n}\n}\n",
        "a b {\n  color: red;\n}\na c {\n  color: white;\n}\na b {\n  color: red;\n}\na c {\n  color: white;\n}\n"
    );
    test!(
        removes_empty_outer_styles,
        "a {\n  b {\n    color: red;\n  }\n",
        "a b {\n  color: red;\n}\n"
    );
    test!(removes_empty_styles, "a {}\n", "");
    test!(
        doesnt_eat_style_after_ruleset,
        "a {\n  b {\n  color: red;\n}\n  color: blue;\n}\n",
        "a {\n  color: blue;\n}\na b {\n  color: red;\n}\n"
    );
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
        style_interpolation_start,
        "a {\n  #{c}olor: red;\n}\n",
        "a {\n  color: red;\n}\n"
    );
    test!(
        style_interpolation_middle,
        "a {\n  co#{l}or: red;\n}\n",
        "a {\n  color: red;\n}\n"
    );
    test!(
        style_interpolation_end,
        "a {\n  colo#{r}: red;\n}\n",
        "a {\n  color: red;\n}\n"
    );
    test!(
        style_interpolation_variable,
        "$a: foo;\na {\n  co#{$a}lor: red;\n}\n",
        "a {\n  cofoolor: red;\n}\n"
    );
    test!(
        style_val_interpolation_start,
        "a {\n  color: #{r}ed;\n}\n",
        "a {\n  color: red;\n}\n"
    );
    test!(
        style_val_interpolation_middle,
        "a {\n  color: r#{e}d;\n}\n",
        "a {\n  color: red;\n}\n"
    );
    test!(
        style_val_interpolation_end,
        "a {\n  color: re#{d};\n}\n",
        "a {\n  color: red;\n}\n"
    );
    test!(
        style_val_interpolation_variable,
        "$a: foo;\na {\n  color: r#{$a}ed;\n}\n",
        "a {\n  color: rfooed;\n}\n"
    );
    test!(
        style_whitespace,
        "a {\n     color      :       red    ;    \n}\n",
        "a {\n  color: red;\n}\n"
    );
    test!(
        triple_nested_preceding_ruleset,
        "a {\n  b {\n    foo: bar;\n    c {}\n  }\n}\n",
        "a b {\n  foo: bar;\n}\n"
    );
    test!(
        triple_nested_following_ruleset,
        "a {\n  b {\n    c {}\n    foo: bar;\n  }\n}\n",
        "a b {\n  foo: bar;\n}\n"
    );
}

#[cfg(test)]
mod test_misc {
    use super::*;
    // test!(
    //     combines_hyphens,
    //     "a {\n  foo: bar - baz;\n}\n",
    //     "a {\n  foo: bar-baz;\n}\n"
    // );
    test!(does_not_combine_hyphens, "a {\n  foo: bar -baz;\n}\n");
    test!(
        ident_starts_with_hyphen,
        "a {\n  foo: -webkit-bar-baz;\n}\n"
    );
    test!(ident_with_num, "el1 {\n  a: b;\n}\n");
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
}

#[cfg(test)]
mod test_interpolation {
    use super::*;
    test!(
        removes_double_quotes,
        "a {\n  color: #{\"red\"};\n}\n",
        "a {\n  color: red;\n}\n"
    );
    test!(
        removes_single_quotes,
        "a {\n  color: #{'red'};\n}\n",
        "a {\n  color: red;\n}\n"
    );
}

#[cfg(test)]
mod test_mixins {
    use super::*;
    test!(
        basic_mixin,
        "@mixin a {\n  color: red;\n}\n\nb {\n  @include a;\n}\n",
        "b {\n  color: red;\n}\n"
    );
    test!(empty_mixin, "@mixin a {}\n\nb {\n  @include a;\n}\n", "");
    test!(
        mixin_two_styles,
        "@mixin a {\n  color: red;\n  color: blue;\n}\n\nb {\n  @include a;\n}\n",
        "b {\n  color: red;\n  color: blue;\n}\n"
    );
    test!(
        mixin_ruleset,
        "@mixin a {\n  b {\n    color: red;\n  }\n}\nb {\n  @include a;\n}\n",
        "b b {\n  color: red;\n}\n"
    );
    test!(
        mixin_two_rulesets,
        "@mixin a {\n  b {\n    color: red;\n  }\n  c {\n    color: blue;\n  }\n}\nd {\n  @include a;\n}\n",
        "d b {\n  color: red;\n}\nd c {\n  color: blue;\n}\n"
    );
    test!(
        mixin_ruleset_and_style,
        "@mixin a {\n  b {\n    color: red;\n  }\n  color: blue;\n}\nd {\n  @include a;\n}\n",
        "d {\n  color: blue;\n}\nd b {\n  color: red;\n}\n"
    );
    test!(
        mixin_style_and_ruleset,
        "@mixin a {\n  color: blue;\n  b {\n    color: red;\n}\n}\nd {\n  @include a;\n}\n",
        "d {\n  color: blue;\n}\nd b {\n  color: red;\n}\n"
    );
    test!(
        mixin_nested_rulesets,
        "@mixin a {\n  b {\n    c {\n      color: red;\n}\n}\n}\nd {\n  @include a;\n}\n",
        "d b c {\n  color: red;\n}\n"
    );
    test!(
        mixin_removes_empty_ruleset,
        "@mixin a {\n  color:red; b {\n}\n}\nd {\n  @include a;\n}\n",
        "d {\n  color: red;\n}\n"
    );
    test!(
        mixin_variable_scope_one_ruleset,
        "@mixin a {\n  $a: blue;\nb {\n  $a: red;\n}  color: $a\n}\nd {\n  @include a;\n}\n",
        "d {\n  color: red;\n}\n"
    );
    test!(
        mixin_no_args,
        "@mixin a {\n  color: red;\n}\nd {\n  @include a();\n}\n",
        "d {\n  color: red;\n}\n"
    );
    test!(
        mixin_single_arg,
        "@mixin a($b) {\n  color: $b;\n}\nd {\n  @include a(red);\n}\n",
        "d {\n  color: red;\n}\n"
    );
    test!(
        mixin_two_args,
        "@mixin a($b, $c) {\n  color: $b;\n  color: $c\n}\nd {\n  @include a(red, blue);\n}\n",
        "d {\n  color: red;\n  color: blue;\n}\n"
    );
    test!(
        mixin_arg_trailing_comma,
        "@mixin a($b, $c,) {\n  color: $b;\n  color: $c\n}\nd {\n  @include a(red, blue);\n}\n",
        "d {\n  color: red;\n  color: blue;\n}\n"
    );
    test!(
        mixin_property_interpolation,
        "@mixin a($b) {\n  #{$b}: red;\n}\nd {\n  @include a(color);\n}\n",
        "d {\n  color: red;\n}\n"
    );
    test!(
        mixin_style_interpolation,
        "@mixin a($b) {\n  color: #{$b};\n}\nd {\n  @include a(red);\n}\n",
        "d {\n  color: red;\n}\n"
    );
    test!(
        mixin_simple_default_value,
        "@mixin a($b: red) {\n  color: $b;\n}\nd {\n  @include a;\n}\n",
        "d {\n  color: red;\n}\n"
    );
    test!(
        mixin_second_value_default,
        "@mixin a($a, $b: blue) {\n  color: $a $b;\n}\nd {\n  @include a(red);\n}\n",
        "d {\n  color: red blue;\n}\n"
    );
    test!(
        mixin_two_default_values,
        "@mixin a($a: red, $b: blue) {\n  color: $a $b;\n}\nd {\n  @include a;\n}\n",
        "d {\n  color: red blue;\n}\n"
    );
    test!(
        mixin_override_default_value_positionally,
        "@mixin a($a: red) {\n  color: $a;\n}\nd {\n  @include a(blue);\n}\n",
        "d {\n  color: blue;\n}\n"
    );
    test!(
        mixin_keyword_arg,
        "@mixin a($a) {\n  color: $a;\n}\nd {\n  @include a($a: blue);\n}\n",
        "d {\n  color: blue;\n}\n"
    );
    test!(
        mixin_keyword_arg_override_default,
        "@mixin a($a: red) {\n  color: $a;\n}\nd {\n  @include a($a: blue);\n}\n",
        "d {\n  color: blue;\n}\n"
    );
    test!(
        mixin_keyword_applies_to_second_arg,
        "@mixin a($a: red, $b) {\n  color: $a $b;\n}\nd {\n  @include a($b: blue);\n}\n",
        "d {\n  color: red blue;\n}\n"
    );
    test!(
        mixin_two_keywords,
        "@mixin a($a, $b) {\n  color: $a $b;\n}\nd {\n  @include a($a: red, $b: blue);\n}\n",
        "d {\n  color: red blue;\n}\n"
    );
    test!(
        mixin_two_keywords_wrong_direction,
        "@mixin a($a, $b) {\n  color: $a $b;\n}\nd {\n  @include a($b: blue, $a: red);\n}\n",
        "d {\n  color: red blue;\n}\n"
    );
    test!(
        variable_in_call_args,
        "@mixin a($a) {\n  color: $a;\n}\nd {\n  $c: red;\n  @include a($c);\n}\n",
        "d {\n  color: red;\n}\n"
    );
    test!(
        comment_before_positional_call_arg,
        "@mixin a($a) {\n  color: $a;\n}\nd {\n  @include a(/*foo*/red);\n}\n",
        "d {\n  color: red;\n}\n"
    );
    test!(
        comment_after_positional_call_arg,
        "@mixin a($a) {\n  color: $a;\n}\nd {\n  @include a(red/*foo*/);\n}\n",
        "d {\n  color: red;\n}\n"
    );
    test!(
        comment_before_keyword_call_arg_val,
        "@mixin a($a) {\n  color: $a;\n}\nd {\n  @include a($a: /*foo*/red);\n}\n",
        "d {\n  color: red;\n}\n"
    );
    test!(
        comment_after_keyword_call_arg_val,
        "@mixin a($a) {\n  color: $a;\n}\nd {\n  @include a($a: red/*foo*/);\n}\n",
        "d {\n  color: red;\n}\n"
    );
    test!(
        comment_before_keyword_call_arg_name,
        "@mixin a($a) {\n  color: $a;\n}\nd {\n  @include a(/*foo*/$a: red);\n}\n",
        "d {\n  color: red;\n}\n"
    );
    test!(
        comment_after_keyword_call_arg_name,
        "@mixin a($a) {\n  color: $a;\n}\nd {\n  @include a($a/*foo*/: red);\n}\n",
        "d {\n  color: red;\n}\n"
    );
    test!(
        toplevel_include,
        "@mixin a {\n  a {\n    color: red;\n  }\n}\n\n@include a;\n",
        "a {\n  color: red;\n}\n"
    );
}

#[cfg(test)]
mod test_imports {
    use super::*;
    use std::io::Write;
    use tempfile::Builder;

    macro_rules! test_import {
        ($func:ident, $input:literal => $output:literal | $( $name:literal($content:literal) ),*) => {
            #[test]
            fn $func() {
                $(
                    let mut f = Builder::new().rand_bytes(0).prefix("").suffix($name).tempfile_in("").unwrap();
                    write!(f, $content).unwrap();
                )*
                let mut buf = Vec::new();
                StyleSheet::new($input)
                .expect(concat!("failed to parse in "))
                .print_as_css(&mut buf)
                .expect(concat!("failed to pretty print on ", $input));
                assert_eq!(
                    String::from($output),
                    String::from_utf8(buf).expect("produced invalid utf8")
                );
            }
        }
    }

    // we have to use test name as filename because tests are run multithreaded in the same directory, so some names may conflict
    test_import!(imports_variable, "@import \"imports_variable\";\na {\n color: $a;\n}" => "a {\n  color: red;\n}\n" | "imports_variable"("$a: red;"));
    test_import!(single_quotes_import, "@import 'single_quotes_import';\na {\n color: $a;\n}" => "a {\n  color: red;\n}\n" | "single_quotes_import"("$a: red;"));
    test_import!(finds_name_scss, "@import \"finds_name_scss\";\na {\n color: $a;\n}" => "a {\n  color: red;\n}\n" | "finds_name_scss.scss"("$a: red;"));
    test_import!(finds_underscore_name_scss, "@import \"finds_underscore_name_scss\";\na {\n color: $a;\n}" => "a {\n  color: red;\n}\n" | "_finds_underscore_name_scss.scss"("$a: red;"));
}

#[cfg(test)]
mod test_values {
    use super::*;
    test!(comma_list_ident, "a {\n  color: red, white, blue;\n}\n");
    test!(space_list_ident, "a {\n  color: red white blue;\n}\n");
    test!(comma_list_number, "a {\n  color: 1, 2, 3;\n}\n");
    test!(space_list_number, "a {\n  color: 1 2 3;\n}\n");
    test!(comma_space_list_number, "a {\n  color: 1 1, 2 2, 3 3;\n}\n");
    test!(preserves_keyword_true, "a {\n  color: true;\n}\n");
    test!(preserves_keyword_false, "a {\n  color: false;\n}\n");
    test!(preserves_keyword_null, "a {\n  color: null;\n}\n");
    test!(preserves_keyword_auto, "a {\n  color: auto;\n}\n");
    test!(preserves_keyword_initial, "a {\n  color: initial;\n}\n");
    test!(preserves_keyword_infinity, "a {\n  color: infinity;\n}\n");
    test!(preserves_keyword_not, "a {\n  color: not;\n}\n");
    test!(preserves_keyword_and, "a {\n  color: and;\n}\n");
    test!(preserves_keyword_or, "a {\n  color: or;\n}\n");
    test!(preserves_keyword_unset, "a {\n  color: unset;\n}\n");
    test!(preserves_keyword_nan, "a {\n  color: NaN;\n}\n");
    test!(
        preserves_quotes,
        "a {\n  color: \"'red' \\\"blue\\\"\";\n}\n"
    );
    test!(
        whitespace_space_list_number,
        "a {\n  color:  1  2  3  ;\n}\n",
        "a {\n  color: 1 2 3;\n}\n"
    );
    test!(
        whitespace_comma_list_number,
        "a {\n  color:  1 ,  2 ,  3  ;\n}\n",
        "a {\n  color: 1, 2, 3;\n}\n"
    );
    test!(number, "a {\n  color: 1;\n}\n");
    test!(
        removes_paren_around_single_value,
        "a {\n  color: (red);\n}\n",
        "a {\n  color: red;\n}\n"
    );
    test!(
        removes_paren_around_space_list,
        "a {\n  color: (red blue);\n}\n",
        "a {\n  color: red blue;\n}\n"
    );
    test!(
        removes_paren_around_item_in_list,
        "a {\n  color: 1 (red blue);\n}\n",
        "a {\n  color: 1 red blue;\n}\n"
    );
}

#[cfg(test)]
mod test_functions {
    use super::*;
    test!(
        return_num,
        "@function a() {\n  @return 1;\n}\n\nb {\ncolor: a();\n}\n",
        "b {\n  color: 1;\n}\n"
    );
    test!(
        return_spaced_list,
        "@function a() {\n  @return a b;\n}\n\nb {\ncolor: a();\n}\n",
        "b {\n  color: a b;\n}\n"
    );
    test!(
        single_arg,
        "@function a($c) {\n  @return $c;\n}\n\nb {\ncolor: a(1);\n}\n",
        "b {\n  color: 1;\n}\n"
    );
    test!(
        return_variable,
        "@function a($a) {\n  @return $a;\n}\n\nb {\ncolor: a(1);\n}\n",
        "b {\n  color: 1;\n}\n"
    );
    // test!(
    //     return_no_semicolon,
    //     "@function a() {\n  @return 1\n}\n\nb {\ncolor: a();\n}\n",
    //     "b {\n  color: 1;\n}\n"
    // );
}
