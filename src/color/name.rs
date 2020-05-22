//! A big dictionary of named colors and their
//! corresponding RGBA values

use once_cell::sync::Lazy;
use std::collections::HashMap;

use crate::interner::InternedString;

pub(crate) struct NamedColorMap {
    name_to_rgba: HashMap<InternedString, [u8; 4]>,
    rgba_to_name: HashMap<[u8; 4], InternedString>,
}

impl NamedColorMap {
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            name_to_rgba: HashMap::with_capacity(capacity),
            rgba_to_name: HashMap::with_capacity(capacity),
        }
    }

    fn insert(&mut self, name: InternedString, rgba: [u8; 4]) {
        self.name_to_rgba.insert(name, rgba);
        self.rgba_to_name.insert(rgba, name);
    }

    pub fn get_by_name(&self, name: InternedString) -> Option<&[u8; 4]> {
        self.name_to_rgba.get(&name)
    }

    pub fn get_by_rgba(&self, rgba: [u8; 4]) -> Option<&InternedString> {
        self.rgba_to_name.get(&rgba)
    }
}

pub(crate) static NAMED_COLORS: Lazy<NamedColorMap> = Lazy::new(|| {
    let mut m = NamedColorMap::with_capacity(150);
    m.insert(
        InternedString::get_or_intern("aliceblue"),
        [0xF0, 0xF8, 0xFF, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("antiquewhite"),
        [0xFA, 0xEB, 0xD7, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("aqua"),
        [0x00, 0xFF, 0xFF, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("aquamarine"),
        [0x7F, 0xFF, 0xD4, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("azure"),
        [0xF0, 0xFF, 0xFF, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("beige"),
        [0xF5, 0xF5, 0xDC, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("bisque"),
        [0xFF, 0xE4, 0xC4, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("black"),
        [0x00, 0x00, 0x00, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("blanchedalmond"),
        [0xFF, 0xEB, 0xCD, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("blue"),
        [0x00, 0x00, 0xFF, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("blueviolet"),
        [0x8A, 0x2B, 0xE2, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("brown"),
        [0xA5, 0x2A, 0x2A, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("burlywood"),
        [0xDE, 0xB8, 0x87, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("cadetblue"),
        [0x5F, 0x9E, 0xA0, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("chartreuse"),
        [0x7F, 0xFF, 0x00, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("chocolate"),
        [0xD2, 0x69, 0x1E, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("coral"),
        [0xFF, 0x7F, 0x50, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("cornflowerblue"),
        [0x64, 0x95, 0xED, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("cornsilk"),
        [0xFF, 0xF8, 0xDC, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("crimson"),
        [0xDC, 0x14, 0x3C, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("darkblue"),
        [0x00, 0x00, 0x8B, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("darkcyan"),
        [0x00, 0x8B, 0x8B, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("darkgoldenrod"),
        [0xB8, 0x86, 0x0B, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("darkgray"),
        [0xA9, 0xA9, 0xA9, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("darkgreen"),
        [0x00, 0x64, 0x00, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("darkkhaki"),
        [0xBD, 0xB7, 0x6B, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("darkmagenta"),
        [0x8B, 0x00, 0x8B, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("darkolivegreen"),
        [0x55, 0x6B, 0x2F, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("darkorange"),
        [0xFF, 0x8C, 0x00, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("darkorchid"),
        [0x99, 0x32, 0xCC, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("darkred"),
        [0x8B, 0x00, 0x00, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("darksalmon"),
        [0xE9, 0x96, 0x7A, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("darkseagreen"),
        [0x8F, 0xBC, 0x8F, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("darkslateblue"),
        [0x48, 0x3D, 0x8B, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("darkslategray"),
        [0x2F, 0x4F, 0x4F, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("darkturquoise"),
        [0x00, 0xCE, 0xD1, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("darkviolet"),
        [0x94, 0x00, 0xD3, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("deeppink"),
        [0xFF, 0x14, 0x93, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("deepskyblue"),
        [0x00, 0xBF, 0xFF, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("dimgray"),
        [0x69, 0x69, 0x69, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("dodgerblue"),
        [0x1E, 0x90, 0xFF, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("firebrick"),
        [0xB2, 0x22, 0x22, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("floralwhite"),
        [0xFF, 0xFA, 0xF0, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("forestgreen"),
        [0x22, 0x8B, 0x22, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("fuchsia"),
        [0xFF, 0x00, 0xFF, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("gainsboro"),
        [0xDC, 0xDC, 0xDC, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("ghostwhite"),
        [0xF8, 0xF8, 0xFF, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("gold"),
        [0xFF, 0xD7, 0x00, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("goldenrod"),
        [0xDA, 0xA5, 0x20, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("gray"),
        [0x80, 0x80, 0x80, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("green"),
        [0x00, 0x80, 0x00, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("greenyellow"),
        [0xAD, 0xFF, 0x2F, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("honeydew"),
        [0xF0, 0xFF, 0xF0, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("hotpink"),
        [0xFF, 0x69, 0xB4, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("indianred"),
        [0xCD, 0x5C, 0x5C, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("indigo"),
        [0x4B, 0x00, 0x82, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("ivory"),
        [0xFF, 0xFF, 0xF0, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("khaki"),
        [0xF0, 0xE6, 0x8C, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("lavender"),
        [0xE6, 0xE6, 0xFA, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("lavenderblush"),
        [0xFF, 0xF0, 0xF5, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("lawngreen"),
        [0x7C, 0xFC, 0x00, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("lemonchiffon"),
        [0xFF, 0xFA, 0xCD, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("lightblue"),
        [0xAD, 0xD8, 0xE6, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("lightcoral"),
        [0xF0, 0x80, 0x80, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("lightcyan"),
        [0xE0, 0xFF, 0xFF, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("lightgoldenrodyellow"),
        [0xFA, 0xFA, 0xD2, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("lightgray"),
        [0xD3, 0xD3, 0xD3, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("lightgreen"),
        [0x90, 0xEE, 0x90, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("lightpink"),
        [0xFF, 0xB6, 0xC1, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("lightsalmon"),
        [0xFF, 0xA0, 0x7A, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("lightseagreen"),
        [0x20, 0xB2, 0xAA, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("lightskyblue"),
        [0x87, 0xCE, 0xFA, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("lightslategray"),
        [0x77, 0x88, 0x99, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("lightsteelblue"),
        [0xB0, 0xC4, 0xDE, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("lightyellow"),
        [0xFF, 0xFF, 0xE0, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("lime"),
        [0x00, 0xFF, 0x00, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("limegreen"),
        [0x32, 0xCD, 0x32, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("linen"),
        [0xFA, 0xF0, 0xE6, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("maroon"),
        [0x80, 0x00, 0x00, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("mediumaquamarine"),
        [0x66, 0xCD, 0xAA, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("mediumblue"),
        [0x00, 0x00, 0xCD, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("mediumorchid"),
        [0xBA, 0x55, 0xD3, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("mediumpurple"),
        [0x93, 0x70, 0xDB, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("mediumseagreen"),
        [0x3C, 0xB3, 0x71, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("mediumslateblue"),
        [0x7B, 0x68, 0xEE, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("mediumspringgreen"),
        [0x00, 0xFA, 0x9A, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("mediumturquoise"),
        [0x48, 0xD1, 0xCC, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("mediumvioletred"),
        [0xC7, 0x15, 0x85, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("midnightblue"),
        [0x19, 0x19, 0x70, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("mintcream"),
        [0xF5, 0xFF, 0xFA, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("mistyrose"),
        [0xFF, 0xE4, 0xE1, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("moccasin"),
        [0xFF, 0xE4, 0xB5, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("navajowhite"),
        [0xFF, 0xDE, 0xAD, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("navy"),
        [0x00, 0x00, 0x80, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("oldlace"),
        [0xFD, 0xF5, 0xE6, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("olive"),
        [0x80, 0x80, 0x00, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("olivedrab"),
        [0x6B, 0x8E, 0x23, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("orange"),
        [0xFF, 0xA5, 0x00, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("orangered"),
        [0xFF, 0x45, 0x00, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("orchid"),
        [0xDA, 0x70, 0xD6, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("palegoldenrod"),
        [0xEE, 0xE8, 0xAA, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("palegreen"),
        [0x98, 0xFB, 0x98, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("paleturquoise"),
        [0xAF, 0xEE, 0xEE, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("palevioletred"),
        [0xDB, 0x70, 0x93, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("papayawhip"),
        [0xFF, 0xEF, 0xD5, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("peachpuff"),
        [0xFF, 0xDA, 0xB9, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("peru"),
        [0xCD, 0x85, 0x3F, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("pink"),
        [0xFF, 0xC0, 0xCB, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("plum"),
        [0xDD, 0xA0, 0xDD, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("powderblue"),
        [0xB0, 0xE0, 0xE6, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("purple"),
        [0x80, 0x00, 0x80, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("rebeccapurple"),
        [0x66, 0x33, 0x99, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("red"),
        [0xFF, 0x00, 0x00, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("rosybrown"),
        [0xBC, 0x8F, 0x8F, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("royalblue"),
        [0x41, 0x69, 0xE1, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("saddlebrown"),
        [0x8B, 0x45, 0x13, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("salmon"),
        [0xFA, 0x80, 0x72, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("sandybrown"),
        [0xF4, 0xA4, 0x60, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("seagreen"),
        [0x2E, 0x8B, 0x57, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("seashell"),
        [0xFF, 0xF5, 0xEE, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("sienna"),
        [0xA0, 0x52, 0x2D, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("silver"),
        [0xC0, 0xC0, 0xC0, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("skyblue"),
        [0x87, 0xCE, 0xEB, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("slateblue"),
        [0x6A, 0x5A, 0xCD, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("slategray"),
        [0x70, 0x80, 0x90, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("snow"),
        [0xFF, 0xFA, 0xFA, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("springgreen"),
        [0x00, 0xFF, 0x7F, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("steelblue"),
        [0x46, 0x82, 0xB4, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("tan"),
        [0xD2, 0xB4, 0x8C, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("teal"),
        [0x00, 0x80, 0x80, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("thistle"),
        [0xD8, 0xBF, 0xD8, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("tomato"),
        [0xFF, 0x63, 0x47, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("turquoise"),
        [0x40, 0xE0, 0xD0, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("violet"),
        [0xEE, 0x82, 0xEE, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("wheat"),
        [0xF5, 0xDE, 0xB3, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("white"),
        [0xFF, 0xFF, 0xFF, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("whitesmoke"),
        [0xF5, 0xF5, 0xF5, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("yellow"),
        [0xFF, 0xFF, 0x00, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("yellowgreen"),
        [0x9A, 0xCD, 0x32, 0xFF],
    );
    m.insert(
        InternedString::get_or_intern("transparent"),
        [0x00, 0x00, 0x00, 0x00],
    );
    m
});
