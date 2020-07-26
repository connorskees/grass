use crate::{
    args::CallArgs,
    builtin::{
        color::{
            hsl::{complement, grayscale, hue, invert, lightness, saturation},
            opacity::alpha,
            other::{adjust_color, change_color, ie_hex_str, scale_color},
            rgb::{blue, green, mix, red},
        },
        modules::Module,
    },
    error::SassResult,
    parse::Parser,
    value::Value,
};

fn adjust_hue(mut args: CallArgs, parser: &mut Parser<'_>) -> SassResult<Value> {
    args.max_args(2)?;
    todo!()
}

pub(crate) fn declare(f: &mut Module) {
    f.insert_builtin("adjust", adjust_color);
    f.insert_builtin("alpha", alpha);
    f.insert_builtin("blue", blue);
    f.insert_builtin("change", change_color);
    f.insert_builtin("complement", complement);
    f.insert_builtin("grayscale", grayscale);
    f.insert_builtin("green", green);
    f.insert_builtin("hue", hue);
    f.insert_builtin("ie-hex-str", ie_hex_str);
    f.insert_builtin("invert", invert);
    f.insert_builtin("lightness", lightness);
    f.insert_builtin("mix", mix);
    f.insert_builtin("red", red);
    f.insert_builtin("saturation", saturation);
    f.insert_builtin("scale", scale_color);
}
