#![cfg(test)]

#[macro_use]
mod macros;

test!(preserves_named_color_case, "a {\n  color: OrAnGe;\n}\n");
test!(preserves_hex_color_case, "a {\n  color: #FfFfFf;\n}\n");
test!(
    preserves_hex_8_val_10000000,
    "a {\n  color: #10000000;\n}\n"
);
test!(
    preserves_hex_8_val_12312312,
    "a {\n  color: #12312312;\n}\n"
);
test!(
    preserves_hex_8_val_ab234cff,
    "a {\n  color: #ab234cff;\n}\n"
);
test!(preserves_hex_6_val_000000, "a {\n  color: #000000;\n}\n");
test!(preserves_hex_6_val_123123, "a {\n  color: #123123;\n}\n");
test!(preserves_hex_6_val_ab234c, "a {\n  color: #ab234c;\n}\n");
test!(preserves_hex_4_val_0000, "a {\n  color: #0000;\n}\n");
test!(preserves_hex_4_val_123a, "a {\n  color: #123a;\n}\n");
test!(preserves_hex_4_val_ab2f, "a {\n  color: #ab2f;\n}\n");
test!(preserves_hex_3_val_000, "a {\n  color: #000;\n}\n");
test!(preserves_hex_3_val_123, "a {\n  color: #123;\n}\n");
test!(preserves_hex_3_val_ab2, "a {\n  color: #ab2;\n}\n");
test!(
    converts_rgb_to_named_color,
    "a {\n  color: rgb(0, 0, 0);\n}\n",
    "a {\n  color: black;\n}\n"
);
test!(
    converts_rgba_to_named_color_red,
    "a {\n  color: rgb(255, 0, 0, 255);\n}\n",
    "a {\n  color: red;\n}\n"
);
test!(
    rgb_binop,
    "a {\n  color: rgb(1, 2, 1+2);\n}\n",
    "a {\n  color: #010203;\n}\n"
);
test!(
    rgb_pads_0,
    "a {\n  color: rgb(1, 2, 3);\n}\n",
    "a {\n  color: #010203;\n}\n"
);
test!(
    rgba_percent,
    "a {\n  color: rgba(159%, 169, 169%, 50%);\n}\n",
    "a {\n  color: rgba(255, 169, 255, 0.5);\n}\n"
);
test!(
    rgba_percent_round_up,
    "a {\n  color: rgba(59%, 169, 69%, 50%);\n}\n",
    "a {\n  color: rgba(150, 169, 176, 0.5);\n}\n"
);
test!(
    rgb_double_digits,
    "a {\n  color: rgb(254, 255, 255);\n}\n",
    "a {\n  color: #feffff;\n}\n"
);
test!(
    rgb_double_digits_white,
    "a {\n  color: rgb(255, 255, 255);\n}\n",
    "a {\n  color: white;\n}\n"
);
test!(
    alpha_function_4_hex,
    "a {\n  color: alpha(#0123);\n}\n",
    "a {\n  color: 0.2;\n}\n"
);
test!(
    alpha_function_named_color,
    "a {\n  color: alpha(red);\n}\n",
    "a {\n  color: 1;\n}\n"
);
test!(opacity_function_number, "a {\n  color: opacity(1);\n}\n");
test!(
    opacity_function_number_unit,
    "a {\n  color: opacity(1px);\n}\n"
);
// blocked on better function-call argument parsing
// specifically, passing lists as values
// test!(
//     rgba_one_arg,
//     "a {\n  color: rgba(1 2 3);;\n}\n",
//     "a {\n  color: #010203;\n}\n"
// );
test!(
    rgba_two_args,
    "a {\n  color: rgba(red, 0.5);\n}\n",
    "a {\n  color: rgba(255, 0, 0, 0.5);\n}\n"
);
test!(
    rgba_opacity_over_1,
    "a {\n  color: rgba(1, 2, 3, 3);\n}\n",
    "a {\n  color: #010203;\n}\n"
);
test!(
    rgba_negative_alpha,
    "a {\n  color: rgba(1, 2, 3, -10%);\n}\n",
    "a {\n  color: rgba(1, 2, 3, 0);\n}\n"
);
test!(
    rgba_opacity_decimal,
    "a {\n  color: rgba(1, 2, 3, .6);\n}\n",
    "a {\n  color: rgba(1, 2, 3, 0.6);\n}\n"
);
test!(
    rgba_opacity_percent,
    "a {\n  color: rgba(1, 2, 3, 50%);\n}\n",
    "a {\n  color: rgba(1, 2, 3, 0.5);\n}\n"
);
test!(
    hsl_basic,
    "a {\n  color: hsl(193, 67%, 99);\n}\n",
    "a {\n  color: #fbfdfe;\n}\n"
);
test!(
    hsla_basic,
    "a {\n  color: hsla(193, 67%, 99, .6);\n}\n",
    "a {\n  color: rgba(251, 253, 254, 0.6);\n}\n"
);
test!(
    hsl_named,
    "a {\n  color: hsl($hue: 193, $saturation: 67%, $luminance: 99);\n}\n",
    "a {\n  color: #fbfdfe;\n}\n"
);
test!(
    hsla_named,
    "a {\n  color: hsla($hue: 193, $saturation: 67%, $luminance: 99, $alpha: .6);\n}\n",
    "a {\n  color: rgba(251, 253, 254, 0.6);\n}\n"
);
test!(
    color_plus_ident,
    "a {\n  color: red + foo;\n}\n",
    "a {\n  color: redfoo;\n}\n"
);
test!(
    ident_plus_color,
    "a {\n  color: foo + red;\n}\n",
    "a {\n  color: foored;\n}\n"
);
test!(
    color_minus_ident,
    "a {\n  color: red - foo;\n}\n",
    "a {\n  color: red-foo;\n}\n"
);
test!(
    color_minus_dbl_quote_ident,
    "a {\n  color: red - \"foo\";\n}\n",
    "a {\n  color: red-\"foo\";\n}\n"
);
test!(
    color_minus_sgl_quote_ident,
    "a {\n  color: red - 'foo';\n}\n",
    "a {\n  color: red-\"foo\";\n}\n"
);
test!(
    color_minus_important,
    "a {\n  color: red - !important;\n}\n",
    "a {\n  color: red-!important;\n}\n"
);
test!(
    color_minus_null,
    "a {\n  color: red - null;\n}\n",
    "a {\n  color: red-;\n}\n"
);
test!(
    ident_minus_color,
    "a {\n  color: foo - red;\n}\n",
    "a {\n  color: foo-red;\n}\n"
);
test!(
    hue,
    "a {\n  color: hue(hsl(193, 67%, 28%));\n}\n",
    "a {\n  color: 193deg;\n}\n"
);
test!(
    hue_red_equals_blue,
    "a {\n  color: hue(rgb(1, 0, 1));\n}\n",
    "a {\n  color: 300deg;\n}\n"
);
test!(
    hue_green_equals_blue,
    "a {\n  color: hue(rgb(0, 1, 1));\n}\n",
    "a {\n  color: 180deg;\n}\n"
);
test!(
    hue_green_is_1,
    "a {\n  color: hue(rgb(0, 1, 0));\n}\n",
    "a {\n  color: 120deg;\n}\n"
);
test!(
    hue_rgb_all_equal,
    "a {\n  color: hue(rgb(1, 1, 1));\n}\n",
    "a {\n  color: 0deg;\n}\n"
);
test!(
    saturation,
    "a {\n  color: saturation(hsl(193, 67%, 28%));\n}\n",
    "a {\n  color: 67%;\n}\n"
);
test!(
    saturation_2,
    "$a: hsl(1, 1, 10);\n\na {\n  color: saturation($a);\n}\n",
    "a {\n  color: 1%;\n}\n"
);
test!(
    lightness,
    "a {\n  color: lightness(hsl(193, 67%, 28%));\n}\n",
    "a {\n  color: 28%;\n}\n"
);
test!(
    invert_no_weight,
    "a {\n  color: invert(white);\n}\n",
    "a {\n  color: black;\n}\n"
);
test!(
    invert_weight_percent,
    "a {\n  color: invert(white, 20%);\n}\n",
    "a {\n  color: #cccccc;\n}\n"
);
test!(
    invert_weight_no_unit,
    "a {\n  color: invert(white, 20);\n}\n",
    "a {\n  color: #cccccc;\n}\n"
);
test!(
    adjust_hue_positive,
    "a {\n  color: adjust-hue(hsl(120, 30%, 90%), 60deg);\n}\n",
    "a {\n  color: #deeded;\n}\n"
);
test!(
    adjust_hue_negative,
    "a {\n  color: adjust-hue(hsl(120, 30%, 90%), -60deg);\n}\n",
    "a {\n  color: #ededde;\n}\n"
);
test!(
    adjust_hue_3_hex,
    "a {\n  color: adjust-hue(#811, 45deg);\n}\n",
    "a {\n  color: #886a11;\n}\n"
);
// blocked on better parsing of call args
// test!(
//     adjust_hue_named_args,
//     "a {\n  color: adjust-hue($color: hsl(120, 30%, 90%), $degrees: 60deg);\n}\n",
//     "a {\n  color: #deeded;\n}\n"
// );
// test!(
//     lighten_named_args,
//     "a {\n  color: lighten($color: hsl(0, 0%, 0%), $amount: 30%);\n}\n",
//     "a {\n  color: #deeded;\n}\n"
// );
test!(
    lighten_basic,
    "a {\n  color: lighten(hsl(0, 0%, 0%), 30%);\n}\n",
    "a {\n  color: #4d4d4d;\n}\n"
);
test!(
    lighten_3_hex,
    "a {\n  color: lighten(#800, 20%);\n}\n",
    // eventually, this should become `#e00`
    // blocked on recognizing when to use 3-hex over 6-hex
    "a {\n  color: #ee0000;\n}\n"
);
// blocked on better parsing of call args
// test!(
//     darken_named_args,
//     "a {\n  color: darken($color: hsl(25, 100%, 80%), $amount: 30%);\n}\n",
//     "a {\n  color: #ff6a00;\n}\n"
// );
test!(
    darken_basic,
    "a {\n  color: darken(hsl(25, 100%, 80%), 30%);\n}\n",
    "a {\n  color: #ff6a00;\n}\n"
);
test!(
    darken_3_hex,
    "a {\n  color: darken(#800, 20%);\n}\n",
    // eventually, this should become `#200`
    // blocked on recognizing when to use 3-hex over 6-hex
    "a {\n  color: #220000;\n}\n"
);
// blocked on better parsing of call args
// test!(
//     saturate_named_args,
//     "a {\n  color: saturate($color: hsl(25, 100%, 80%), $amount: 30%);\n}\n",
//     "a {\n  color: #ff6a00;\n}\n"
// );
test!(
    saturate_basic,
    "a {\n  color: saturate(hsl(120, 30%, 90%), 20%);\n}\n",
    "a {\n  color: #d9f2d9;\n}\n"
);
test!(
    saturate_3_hex,
    "a {\n  color: saturate(#855, 20%);\n}\n",
    "a {\n  color: #9e3f3f;\n}\n"
);
// blocked on better parsing of call args
// test!(
//     desaturate_named_args,
//     "a {\n  color: desaturate($color: hsl(25, 100%, 80%), $amount: 30%);\n}\n",
//     "a {\n  color: #ff6a00;\n}\n"
// );
test!(
    desaturate_basic,
    "a {\n  color: desaturate(hsl(120, 30%, 90%), 20%);\n}\n",
    "a {\n  color: #e3e8e3;\n}\n"
);
test!(
    desaturate_3_hex,
    "a {\n  color: desaturate(#855, 20%);\n}\n",
    "a {\n  color: #726b6b;\n}\n"
);
test!(
    desaturate_correctly_calculates_hue,
    "a {\n  color: desaturate(plum, 14%);\n}\n",
    "a {\n  color: #d4a9d4;\n}\n"
);
test!(
    transparentize,
    "a {\n  color: transparentize(rgba(0, 0, 0, 0.5), 0.1);\n}\n",
    "a {\n  color: rgba(0, 0, 0, 0.4);\n}\n"
);
test!(
    fade_out,
    "a {\n  color: fade-out(rgba(0, 0, 0, 0.8), 0.2);\n}\n",
    "a {\n  color: rgba(0, 0, 0, 0.6);\n}\n"
);
test!(
    opacify,
    "a {\n  color: opacify(rgba(0, 0, 0, 0.5), 0.1);\n}\n",
    "a {\n  color: rgba(0, 0, 0, 0.6);\n}\n"
);
test!(
    fade_in,
    "a {\n  color: opacify(rgba(0, 0, 17, 0.8), 0.2);\n}\n",
    "a {\n  color: #000011;\n}\n"
);
test!(
    grayscale_1,
    "a {\n  color: grayscale(plum);\n}\n",
    "a {\n  color: #bfbfbf;\n}\n"
);
test!(
    grayscale_2,
    "a {\n  color: grayscale(red);\n}\n",
    "a {\n  color: gray;\n}\n"
);
