#![cfg(test)]

#[macro_use]
mod macros;

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
    selector_attribute_interpolate,
    "[a#{tt}r=v#{a}l] {\n  color: red;\n}\n",
    "[attr=val] {\n  color: red;\n}\n"
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
// test!(
//     selector_attribute_single_quotes,
//     "[attr='val'] {\n  color: red;\n}\n",
//     "[attr=val] {\n  color: red;\n}\n"
// );
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
test!(
    selector_attribute_interpolated,
    "a {\n  [#{&}] {\n    color: red;\n  }\n}\n",
    "a [a] {\n  color: red;\n}\n"
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
test!(selector_pseudoelement, "::before {\n  color: red;\n}\n");
test!(
    selector_el_and_pseudoelement,
    "a::before {\n  color: red;\n}\n"
);
test!(
    selector_el_pseudoelement_descendant,
    "a ::before {\n  color: red;\n}\n"
);
test!(
    selector_pseudoelement_el_descendant,
    "::before a {\n  color: red;\n}\n"
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
    selector_pseudo_paren_whitespacespace,
    ":pseudo(  -2n+1 ) {\n  color: red;\n}\n",
    ":pseudo(-2n+1) {\n  color: red;\n}\n"
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
test!(
    bem_underscore_selector,
    "a {\n&__b {\n  color: red;\n}\n}\n",
    "a__b {\n  color: red;\n}\n"
);
test!(
    selector_interpolation_addition,
    "#{\"foo\" + \" bar\"}baz {color: red;}",
    "foo barbaz {\n  color: red;\n}\n"
);
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
test!(
    combinator_following,
    "a + {\n  b {\n    color: red;\n  }\n}\n",
    "a + b {\n  color: red;\n}\n"
);
test!(
    combinator_preceding,
    "a {\n  + b {\n    color: red;\n  }\n}\n",
    "a + b {\n  color: red;\n}\n"
);
test!(
    combinator_alone,
    "a {\n  + {\n    b {\n      color: red;\n  }\n}\n",
    "a + b {\n  color: red;\n}\n"
);
test!(simple_multiple_newline, "a,\nb {\n  color: red;\n}\n");
test!(
    nested_multiple_newline,
    "a,\nb {\n  c {\n    color: blue;\n  }\n  color: red;\n}\n",
    "a,\nb {\n  color: red;\n}\na c,\nb c {\n  color: blue;\n}\n"
);
test!(
    trailing_comma_newline,
    "#foo #bar,,\n,#baz #boom, {a: b}",
    "#foo #bar,\n#baz #boom {\n  a: b;\n}\n"
);
// test!(
//     trailing_comma_children,
//     "a,, {\n  b {\n    color: /**/red;\n  }\n}\n",
//     "a b {\n  color: red;\n}\n"
// );
test!(simple_placeholder, "%a {\n  color: red;\n}\n", "");
test!(
    placeholder_first,
    "%a, b {\n  color: red;\n}\n",
    "b {\n  color: red;\n}\n"
);
test!(
    placeholder_last,
    "a, %b {\n  color: red;\n}\n",
    "a {\n  color: red;\n}\n"
);
test!(
    placeholder_middle,
    "a, %b, c {\n  color: red;\n}\n",
    "a, c {\n  color: red;\n}\n"
);
test!(
    removes_leading_space,
    "#{&} a {\n  color: red;\n}\n",
    "a {\n  color: red;\n}\n"
);
test!(allows_id_start_with_number, "#2foo {\n  color: red;\n}\n");
test!(allows_id_only_number, "#2 {\n  color: red;\n}\n");
test!(
    id_interpolation,
    "$zzz: zzz;\n##{$zzz} {\n  a: b;\n}\n",
    "#zzz {\n  a: b;\n}\n"
);
test!(
    interpolate_id_selector,
    "$bar: \"#foo\";\nul li#{$bar} {\n  foo: bar;\n}\n",
    "ul li#foo {\n  foo: bar;\n}\n"
);
test!(escaped_space, "a\\ b {\n  color: foo;\n}\n");
// blocked on whitespace
// test!(multiple_consecutive_immediate_child,"> > foo {\n  color: foo;\n}\n",);
