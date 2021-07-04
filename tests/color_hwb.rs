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
error!(
    hwb_whiteness_missing_pct,
    "@use \"sass:color\";\na {\n  color: color.hwb(0, 0, 100);\n}\n",
    "Error: $whiteness: Expected 0 to have unit \"%\"."
);
