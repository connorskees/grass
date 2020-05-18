use super::{Builtin, GlobalFunctionMap};

mod hsl;
mod opacity;
mod other;
mod rgb;

pub(crate) fn declare(f: &mut GlobalFunctionMap) {
    hsl::declare(f);
    opacity::declare(f);
    other::declare(f);
    rgb::declare(f);
}
