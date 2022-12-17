use super::GlobalFunctionMap;

pub mod hsl;
pub mod hwb;
pub mod opacity;
pub mod other;
pub mod rgb;

pub(crate) fn declare(f: &mut GlobalFunctionMap) {
    hsl::declare(f);
    opacity::declare(f);
    other::declare(f);
    rgb::declare(f);
}
