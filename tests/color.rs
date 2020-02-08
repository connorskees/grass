#![cfg(test)]

#[macro_use]
mod macros;

test!(preserves_named_color_case, "a {\n  color: OrAnGe;\n}\n");
test!(preserves_hex_color_case, "a {\n  color: #FfFfFf;\n}\n");
test!(
    preserves_hex_8_val_00000000,
    "a {\n  color: #00000000;\n}\n"
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
// test!(
//     converts_rgb_to_named_color,
//     "a {\n  color: rgb(0, 0, 0);\n}\n",
//     "a {\n  color: black;\n}\n"
// );
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
    rgb_double_digits,
    "a {\n  color: rgb(255, 255, 255);\n}\n",
    "a {\n  color: #ffffff;\n}\n"
);
