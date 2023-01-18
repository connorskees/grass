#[macro_use]
mod macros;

error!(
    hsl_no_args,
    "a {\n  color: hsl();\n}\n", "Error: Missing argument $channels."
);
error!(
    hsla_no_args,
    "a {\n  color: hsla();\n}\n", "Error: Missing argument $channels."
);
test!(
    hsl_basic,
    "a {\n  color: hsl(193, 67%, 99);\n}\n",
    "a {\n  color: hsl(193deg, 67%, 99%);\n}\n"
);
test!(
    hsla_basic,
    "a {\n  color: hsla(193, 67%, 99, .6);\n}\n",
    "a {\n  color: hsla(193deg, 67%, 99%, 0.6);\n}\n"
);
test!(
    hsl_doesnt_care_about_units,
    "a {\n  color: hsl(193deg, 67foo, 99%);\n}\n",
    "a {\n  color: hsl(193deg, 67%, 99%);\n}\n"
);
test!(
    hsl_named,
    "a {\n  color: hsl($hue: 193, $saturation: 67%, $lightness: 99);\n}\n",
    "a {\n  color: hsl(193deg, 67%, 99%);\n}\n"
);
test!(
    hsl_four_args,
    "a {\n  color: hsl(0, 0, 0, 0.456);\n}\n",
    "a {\n  color: hsla(0deg, 0%, 0%, 0.456);\n}\n"
);
test!(
    hsl_negative_hue,
    "a {\n  color: hsl(-60deg, 100%, 50%);\n}\n",
    "a {\n  color: hsl(300deg, 100%, 50%);\n}\n"
);
test!(
    hsl_hue_above_max,
    "a {\n  color: hsl(540, 100%, 50%);\n}\n",
    "a {\n  color: hsl(180deg, 100%, 50%);\n}\n"
);
test!(
    hsl_hue_below_min,
    "a {\n  color: hsl(-540, 100%, 50%);\n}\n",
    "a {\n  color: hsl(180deg, 100%, 50%);\n}\n"
);
test!(
    hsla_named,
    "a {\n  color: hsla($hue: 193, $saturation: 67%, $lightness: 99, $alpha: .6);\n}\n",
    "a {\n  color: hsla(193deg, 67%, 99%, 0.6);\n}\n"
);
test!(
    hue,
    "a {\n  color: hue(hsl(193, 67%, 28%));\n}\n",
    "a {\n  color: 193deg;\n}\n"
);
test!(
    hue_maintains_value_when_created_through_hsl,
    "a {\n  color: hue(hsl(0.544, 100%, 100%));\n}\n",
    "a {\n  color: 0.544deg;\n}\n"
);
test!(
    hue_red_equals_blue,
    "a {\n  color: hue(rgb(1, 0, 1));\n}\n",
    "a {\n  color: 300deg;\n}\n"
);
test!(
    hue_of_360_becomes_0,
    "a {\n  color: hue(hsl(360, 10%, 20%));\n}\n",
    "a {\n  color: 0deg;\n}\n"
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
test!(
    adjust_hue_named_args,
    "a {\n  color: adjust-hue($color: hsl(120, 30%, 90%), $degrees: 60deg);\n}\n",
    "a {\n  color: #deeded;\n}\n"
);
test!(
    lighten_named_args,
    "a {\n  color: lighten($color: hsl(0, 0%, 0%), $amount: 30%);\n}\n",
    "a {\n  color: #4d4d4d;\n}\n"
);
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
test!(
    lighten_percent,
    "a {
        color: lighten(crimson, 10%);
    }",
    "a {\n  color: #ed365b;\n}\n"
);
test!(
    lighten_no_percent,
    "a {
        color: lighten(crimson, 10);
    }",
    "a {\n  color: #ed365b;\n}\n"
);
test!(
    channels_after_lighten,
    "a {
        color: red(lighten(crimson, 10));
        color: green(lighten(crimson, 10));
        color: blue(lighten(crimson, 10));
        color: hue(lighten(crimson, 10));
        color: hue(crimson);
        color: saturation(lighten(crimson, 10));
        color: lightness(lighten(crimson, 10));
    }",
    "a {\n  color: 237;\n  color: 54;\n  color: 91;\n  color: 348deg;\n  color: 348deg;\n  color: 83.3333333333%;\n  color: 57.0588235294%;\n}\n"
);
error!(
    lighten_nan,
    "a {
        color: lighten(crimson, (0/0));
    }",
    "Error: $amount: Expected NaN to be within 0 and 100."
);
test!(
    darken_named_args,
    "a {\n  color: darken($color: hsl(25, 100%, 80%), $amount: 30%);\n}\n",
    "a {\n  color: #ff6a00;\n}\n"
);
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
test!(
    saturate_named_args,
    "a {\n  color: saturate($color: hsl(25, 100%, 80%), $amount: 30%);\n}\n",
    "a {\n  color: #ffc499;\n}\n"
);
test!(
    saturation_cannot_go_above_100,
    "a {\n  color: saturation(saturate($color: hsl(25, 100%, 80%), $amount: 30%));\n}\n",
    "a {\n  color: 100%;\n}\n"
);
test!(
    saturate_one_named_arg,
    "a {\n  color: saturate($amount: 50%);\n}\n",
    "a {\n  color: saturate(50%);\n}\n"
);
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
test!(
    desaturate_named_args,
    "a {\n  color: desaturate($color: hsl(25, 100%, 80%), $amount: 30%);\n}\n",
    "a {\n  color: #f0c6a8;\n}\n"
);
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
    negative_values_in_hsl,
    "a {\n  color: hsl(-1 -1 -1);\n}\n",
    "a {\n  color: hsl(359deg, 0%, 0%);\n}\n"
);
test!(
    hsla_becomes_named_color,
    "a {\n  color: hsla(0deg, 100%, 50%);\n}\n",
    "a {\n  color: hsl(0deg, 100%, 50%);\n}\n"
);
test!(
    hsl_special_fn_4_arg_maintains_units,
    "a {\n  color: hsl(1, 0.02, 3%, max(0.4));\n}\n",
    "a {\n  color: hsla(1deg, 0.02%, 3%, 0.4);\n}\n"
);
test!(
    hsl_special_fn_3_arg_maintains_units,
    "a {\n  color: hsl(1, 0.02, max(0.4));\n}\n",
    "a {\n  color: hsl(1deg, 0.02%, 0.4%);\n}\n"
);
test!(
    hsla_special_fn_1_arg_is_not_list,
    "a {\n  color: hsla(var(--foo));\n}\n",
    "a {\n  color: hsla(var(--foo));\n}\n"
);
test!(
    hue_of_rgb_is_negative,
    "a {\n  color: hue(rgb(255, 0, 1));\n}\n",
    "a {\n  color: 359.7647058824deg;\n}\n"
);
test!(
    saturation_of_rgb_all_channels_equal,
    "a {\n  color: saturation(rgb(125, 125, 125));\n}\n",
    "a {\n  color: 0%;\n}\n"
);
test!(
    saturation_of_rgb_min_and_max_above_1,
    "a {\n  color: saturation(rgb(255, 248, 248));\n}\n",
    "a {\n  color: 100%;\n}\n"
);
test!(
    saturation_of_rgb_min_and_max_below_1,
    "a {\n  color: saturation(rgb(88, 88, 87));\n}\n",
    "a {\n  color: 0.5714285714%;\n}\n"
);
test!(
    hsl_with_turn_unit,
    "a {\n  color: hsl(8turn, 25%, 50%);\n}\n",
    "a {\n  color: hsl(0deg, 25%, 50%);\n}\n"
);
test!(
    hsl_with_rad_unit,
    "a {\n  color: hsl(8rad, 25%, 50%);\n}\n",
    "a {\n  color: hsl(98.3662361047deg, 25%, 50%);\n}\n"
);
test!(
    hsl_with_grad_unit,
    "a {\n  color: hsl(8grad, 25%, 50%);\n}\n",
    "a {\n  color: hsl(7.2deg, 25%, 50%);\n}\n"
);
test!(
    adjust_hue_nan,
    "a {\n  color: adjust-hue(hsla(200, 50%, 50%), (0/0));\n}\n",
    "a {\n  color: #404040;\n}\n"
);
test!(
    adjust_hue_nan_get_hue,
    "a {\n  color: hue(adjust-hue(hsla(200, 50%, 50%), (0/0)));\n}\n",
    "a {\n  color: NaNdeg;\n}\n"
);
test!(
    hsl_special_two_arg_var_first,
    "a {\n  color: hsl(var(--foo), --bar);\n}\n",
    "a {\n  color: hsl(var(--foo), --bar);\n}\n"
);
test!(
    hsl_special_two_arg_var_second,
    "a {\n  color: hsl(--foo, var(--bar));\n}\n",
    "a {\n  color: hsl(--foo, var(--bar));\n}\n"
);
error!(
    hsl_special_two_arg_neither_var,
    "a {\n  color: hsl(--foo, --bar);\n}\n", "Error: Missing argument $lightness."
);
test!(
    hsl_special_four_arg_var_last,
    "a {\n  color: hsl(a, b, c, var(--bar));\n}\n",
    "a {\n  color: hsl(a, b, c, var(--bar));\n}\n"
);
test!(
    hsl_special_three_arg_var_last,
    "a {\n  color: hsl(a, b, var(--bar));\n}\n",
    "a {\n  color: hsl(a, b, var(--bar));\n}\n"
);
test!(
    darken_all_channels_equal,
    "a {\n  color: darken(#fff, 10);\n}\n",
    "a {\n  color: #e6e6e6;\n}\n"
);
test!(
    darken_green_channel_max,
    "a {\n  color: darken(rgb(50, 200, 100), 10);\n}\n",
    "a {\n  color: #289f50;\n}\n"
);
test!(
    hue_adjust_color_over_360,
    "a {\n  color: hue(adjust-color(blue, $hue: 150));\n}\n",
    "a {\n  color: 30deg;\n}\n"
);
test!(
    adjust_hue_rad,
    "a {\n  color: adjust-hue(red, 60rad);\n}\n",
    "a {\n  color: #00b4ff;\n}\n"
);
test!(
    hsl_hue_rad,
    "a {\n  color: hsl(60rad, 100%, 50%);\n}\n",
    "a {\n  color: hsl(197.7467707849deg, 100%, 50%);\n}\n"
);
error!(
    hsl_one_arg_bracketed,
    "a {\n  color: hsl([1, 2, 3]);\n}\n",
    "Error: $channels must be an unbracketed, space-separated list."
);
error!(
    hue_arg_not_color,
    "a {\n  color: hue(1);\n}\n", "Error: $color: 1 is not a color."
);
error!(
    saturation_arg_not_color,
    "a {\n  color: saturation(1);\n}\n", "Error: $color: 1 is not a color."
);
error!(
    lightness_arg_not_color,
    "a {\n  color: lightness(1);\n}\n", "Error: $color: 1 is not a color."
);
error!(
    adjust_hue_first_arg_not_color,
    "a {\n  color: adjust-hue(1, 5deg);\n}\n", "Error: $color: 1 is not a color."
);
error!(
    hue_no_args,
    "a {\n  color: hue();\n}\n", "Error: Missing argument $color."
);
error!(
    saturation_no_args,
    "a {\n  color: saturation();\n}\n", "Error: Missing argument $color."
);
error!(
    lightness_no_args,
    "a {\n  color: lightness();\n}\n", "Error: Missing argument $color."
);
error!(
    adjust_hue_no_args,
    "a {\n  color: adjust-hue();\n}\n", "Error: Missing argument $color."
);
error!(
    adjust_hue_one_arg,
    "a {\n  color: adjust-hue(red);\n}\n", "Error: Missing argument $degrees."
);
error!(
    lighten_no_args,
    "a {\n  color: lighten();\n}\n", "Error: Missing argument $color."
);
error!(
    lighten_one_arg,
    "a {\n  color: lighten(red);\n}\n", "Error: Missing argument $amount."
);
error!(
    darken_no_args,
    "a {\n  color: darken();\n}\n", "Error: Missing argument $color."
);
error!(
    darken_one_arg,
    "a {\n  color: darken(red);\n}\n", "Error: Missing argument $amount."
);
error!(
    saturate_single_named_arg_not_amount,
    "a {\n  color: saturate($a: red);\n}\n", "Error: Missing argument $amount."
);
error!(
    saturate_no_args,
    "a {\n  color: saturate();\n}\n", "Error: Missing argument $amount."
);
error!(
    saturate_one_arg,
    "a {\n  color: saturate(red);\n}\n", "Error: $amount: red is not a number."
);
error!(
    desaturate_no_args,
    "a {\n  color: desaturate();\n}\n", "Error: Missing argument $color."
);
error!(
    desaturate_one_arg,
    "a {\n  color: desaturate(red);\n}\n", "Error: Missing argument $amount."
);
error!(
    complement_no_args,
    "a {\n  color: complement();\n}\n", "Error: Missing argument $color."
);
