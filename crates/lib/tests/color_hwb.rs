#[macro_use]
mod macros;

test!(
    blackness_black,
    "@use \"sass:color\";\na {\n  color: color.blackness(black);\n}\n",
    "a {\n  color: 100%;\n}\n"
);
test!(
    blackness_white,
    "@use \"sass:color\";\na {\n  color: color.blackness(white);\n}\n",
    "a {\n  color: 0%;\n}\n"
);

test!(
    whiteness_black,
    "@use \"sass:color\";\na {\n  color: color.whiteness(black);\n}\n",
    "a {\n  color: 0%;\n}\n"
);
test!(
    whiteness_white,
    "@use \"sass:color\";\na {\n  color: color.whiteness(white);\n}\n",
    "a {\n  color: 100%;\n}\n"
);
test!(
    blackness_approx_50_pct,
    "@use \"sass:color\";\na {\n  color: color.blackness(color.hwb(0, 0%, 50%));\n}\n",
    "a {\n  color: 49.8039215686%;\n}\n"
);
test!(
    blackness_approx_50_pct_and_whiteness,
    "@use \"sass:color\";\na {\n  color: color.blackness(color.hwb(0, 50%, 50%));\n}\n",
    "a {\n  color: 49.8039215686%;\n}\n"
);
test!(
    blackness_approx_70_pct_and_whiteness,
    "@use \"sass:color\";\na {\n  color: color.blackness(color.hwb(0, 70%, 70%));\n}\n",
    "a {\n  color: 49.8039215686%;\n}\n"
);
test!(
    blackness_approx_half_pct,
    "@use \"sass:color\";\na {\n  color: color.blackness(color.hwb(0, 0%, 0.5%));\n}\n",
    "a {\n  color: 0.3921568627%;\n}\n"
);
test!(
    hwb_half_blackness,
    "@use \"sass:color\";\na {\n  color: color.hwb(0, 0%, 50%);\n}\n",
    "a {\n  color: maroon;\n}\n"
);
test!(
    hwb_equal_white_black_50,
    "@use \"sass:color\";\na {\n  color: color.hwb(0, 50%, 50%);\n}\n",
    "a {\n  color: gray;\n}\n"
);
test!(
    hwb_equal_white_black_70,
    "@use \"sass:color\";\na {\n  color: color.hwb(0, 70%, 70%);\n}\n",
    "a {\n  color: gray;\n}\n"
);
test!(
    hwb_half_percent_black,
    "@use \"sass:color\";\na {\n  color: color.hwb(0, 0%, 0.5%);\n}\n",
    "a {\n  color: #fe0000;\n}\n"
);
test!(
    hwb_black_100,
    "@use \"sass:color\";\na {\n  color: color.hwb(0, 0%, 100%);\n}\n",
    "a {\n  color: black;\n}\n"
);
test!(
    blackness_named,
    "@use \"sass:color\";\na {\n  color: color.blackness($color: color.hwb(0, 0%, 42%));\n}\n",
    "a {\n  color: 41.9607843137%;\n}\n"
);
test!(
    hwb_alpha_unitless,
    "@use \"sass:color\";\na {\n  color: color.hwb(0, 0%, 100%, 0.04);\n}\n",
    "a {\n  color: rgba(0, 0, 0, 0.04);\n}\n"
);
test!(
    hwb_alpha_unit_percent,
    "@use \"sass:color\";\na {\n  color: color.hwb(0, 0%, 100%, 0.04%);\n}\n",
    "a {\n  color: rgba(0, 0, 0, 0.0004);\n}\n"
);
test!(
    hwb_negative_alpha,
    "@use \"sass:color\";\na {\n  color: color.hwb(0, 0%, 100%, -0.5);\n}\n",
    "a {\n  color: rgba(0, 0, 0, 0);\n}\n"
);
test!(
    hue_60_whiteness_20_blackness_100,
    "@use \"sass:color\";\na {\n  color: color.hwb(60, 20%, 100%);\n}\n",
    "a {\n  color: #2b2b2b;\n}\n"
);
test!(
    one_arg_with_slash,
    "@use \"sass:color\";\na {\n  color: color.hwb(180 30% 40% / 0);\n}\n",
    "a {\n  color: rgba(77, 153, 153, 0);\n}\n"
);
test!(
    hue_has_unit_rad,
    "@use \"sass:color\";\na {\n  color: color.hwb(1rad, 30%, 40%);\n}\n",
    "a {\n  color: #99964d;\n}\n"
);
test!(
    scale_whiteness,
    "a {\n  color: scale-color(#cc6666, $whiteness: 100%);\n}\n",
    "a {\n  color: #d5d5d5;\n}\n"
);
error!(
    hwb_whiteness_missing_pct,
    "@use \"sass:color\";\na {\n  color: color.hwb(0, 0, 100);\n}\n",
    "Error: $whiteness: Expected 0 to have unit \"%\"."
);
error!(
    hwb_two_args,
    "@use \"sass:color\";\na {\n  color: color.hwb(#123, 0.5);\n}\n",
    "Error: Only 1 argument allowed, but 2 were passed."
);
error!(
    hwb_blackness_too_high,
    "@use \"sass:color\";\na {\n  color: color.hwb(0, 30%, 101%, 0.5);\n}\n",
    "Error: $blackness: Expected 101% to be within 0% and 100%."
);
error!(
    blackness_no_args,
    "@use \"sass:color\";\na {\n  color: color.blackness();\n}\n",
    "Error: Missing argument $color."
);
error!(
    whiteness_no_args,
    "@use \"sass:color\";\na {\n  color: color.whiteness();\n}\n",
    "Error: Missing argument $color."
);
error!(
    hwb_var_channels,
    "@use \"sass:color\";\na {\n  color: color.hwb(var(--foo));\n}\n",
    "Error: Expected numeric channels, got \"hwb(var(--foo))\"."
);
