use std::collections::HashMap;

use super::Builtin;

mod hsl;
mod opacity;
mod other;
mod rgb;

pub(crate) fn register(f: &mut HashMap<String, Builtin>) {
    hsl::register(f);
    opacity::register(f);
    other::register(f);
    rgb::register(f);
}
