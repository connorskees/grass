use std::fmt;

pub(crate) use conversion::UNIT_CONVERSION_TABLE;

mod conversion;

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum Unit {
    // Absolute units
    /// Pixels
    Px,
    /// Millimeters
    Mm,
    /// Inches
    In,
    /// Centimeters
    Cm,
    /// Quarter-millimeters
    Q,
    /// Points
    Pt,
    /// Picas
    Pc,

    // Font relative units
    /// Font size of the parent element
    Em,
    /// Font size of the root element
    Rem,
    /// Line height of the element
    Lh,
    /// x-height of the element's font
    Ex,
    /// The advance measure (width) of the glyph "0" of the element's font
    Ch,
    /// Represents the "cap height" (nominal height of capital letters) of the element's font
    Cap,
    /// Equal to the used advance measure of the "水" (CJK water ideograph, U+6C34) glyph
    /// found in the font used to render it
    Ic,
    /// Equal to the computed value of the line-height property on the root element
    /// (typically <html>), converted to an absolute length
    Rlh,

    // Viewport relative units
    /// 1% of the viewport's width
    Vw,
    /// 1% of the viewport's height
    Vh,
    /// 1% of the viewport's smaller dimension
    Vmin,
    /// 1% of the viewport's larger dimension
    Vmax,
    /// Equal to 1% of the size of the initial containing block, in the direction of the root
    /// element's inline axis
    Vi,
    /// Equal to 1% of the size of the initial containing block, in the direction of the root
    /// element's block axis
    Vb,

    // Angle units
    /// Represents an angle in degrees. One full circle is 360deg
    Deg,
    /// Represents an angle in gradians. One full circle is 400grad
    Grad,
    /// Represents an angle in radians. One full circle is 2π radians which approximates to 6.283rad
    Rad,
    /// Represents an angle in a number of turns. One full circle is 1turn
    Turn,

    // Time units
    /// Represents a time in seconds
    S,
    /// Represents a time in milliseconds
    Ms,

    // Frequency units
    /// Represents a frequency in hertz
    Hz,
    /// Represents a frequency in kilohertz
    Khz,

    // Resolution units
    /// Represents the number of dots per inch
    Dpi,
    /// Represents the number of dots per centimeter
    Dpcm,
    /// Represents the number of dots per px unit
    Dppx,
    /// Alias for dppx
    X,

    // Other units
    /// Represents a fraction of the available space in the grid container
    Fr,
    Percent,

    /// Unknown unit
    Unknown(String),
    /// Unspecified unit
    None,

    /// Units multiplied together
    Mul(Vec<Unit>),
}
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) enum UnitKind {
    Absolute,
    FontRelative,
    ViewportRelative,
    Angle,
    Time,
    Frequency,
    Resolution,
    Other,
    None,
}

impl Unit {
    pub fn comparable(&self, other: &Unit) -> bool {
        if other == &Unit::None {
            return true;
        }
        match self.kind() {
            UnitKind::FontRelative | UnitKind::ViewportRelative | UnitKind::Other => self == other,
            UnitKind::None => true,
            u => other.kind() == u,
        }
    }

    /// Used internally to determine if two units are comparable or not
    fn kind(&self) -> UnitKind {
        match self {
            Unit::Px | Unit::Mm | Unit::In | Unit::Cm | Unit::Q | Unit::Pt | Unit::Pc => {
                UnitKind::Absolute
            }
            Unit::Em
            | Unit::Rem
            | Unit::Lh
            | Unit::Ex
            | Unit::Ch
            | Unit::Cap
            | Unit::Ic
            | Unit::Rlh => UnitKind::FontRelative,
            Unit::Vw | Unit::Vh | Unit::Vmin | Unit::Vmax | Unit::Vi | Unit::Vb => {
                UnitKind::ViewportRelative
            }
            Unit::Deg | Unit::Grad | Unit::Rad | Unit::Turn => UnitKind::Angle,
            Unit::S | Unit::Ms => UnitKind::Time,
            Unit::Hz | Unit::Khz => UnitKind::Frequency,
            Unit::Dpi | Unit::Dpcm | Unit::Dppx | Unit::X => UnitKind::Resolution,
            Unit::None => UnitKind::None,
            Unit::Fr | Unit::Percent | Unit::Unknown(..) | Unit::Mul(..) => UnitKind::Other,
        }
    }
}

impl From<&String> for Unit {
    fn from(unit: &String) -> Self {
        match unit.to_ascii_lowercase().as_bytes() {
            b"px" => Unit::Px,
            b"mm" => Unit::Mm,
            b"in" => Unit::In,
            b"cm" => Unit::Cm,
            b"q" => Unit::Q,
            b"pt" => Unit::Pt,
            b"pc" => Unit::Pc,
            b"em" => Unit::Em,
            b"rem" => Unit::Rem,
            b"lh" => Unit::Lh,
            b"%" => Unit::Percent,
            b"ex" => Unit::Ex,
            b"ch" => Unit::Ch,
            b"cap" => Unit::Cap,
            b"ic" => Unit::Ic,
            b"rlh" => Unit::Rlh,
            b"vw" => Unit::Vw,
            b"vh" => Unit::Vh,
            b"vmin" => Unit::Vmin,
            b"vmax" => Unit::Vmax,
            b"vi" => Unit::Vi,
            b"vb" => Unit::Vb,
            b"deg" => Unit::Deg,
            b"grad" => Unit::Grad,
            b"rad" => Unit::Rad,
            b"turn" => Unit::Turn,
            b"s" => Unit::S,
            b"ms" => Unit::Ms,
            b"hz" => Unit::Hz,
            b"khz" => Unit::Khz,
            b"dpi" => Unit::Dpi,
            b"dpcm" => Unit::Dpcm,
            b"dppx" => Unit::Dppx,
            b"x" => Unit::X,
            b"fr" => Unit::Fr,
            _ => Unit::Unknown(String::from(unit)),
        }
    }
}

impl Into<String> for Unit {
    fn into(self) -> String {
        match self {
            Unit::Px => "px",
            Unit::Mm => "mm",
            Unit::In => "in",
            Unit::Cm => "cm",
            Unit::Q => "q",
            Unit::Pt => "pt",
            Unit::Pc => "pc",
            Unit::Em => "em",
            Unit::Rem => "rem",
            Unit::Lh => "lh",
            Unit::Percent => "%",
            Unit::Ex => "ex",
            Unit::Ch => "ch",
            Unit::Cap => "cap",
            Unit::Ic => "ic",
            Unit::Rlh => "rlh",
            Unit::Vw => "vw",
            Unit::Vh => "vh",
            Unit::Vmin => "vmin",
            Unit::Vmax => "vmax",
            Unit::Vi => "vi",
            Unit::Vb => "vb",
            Unit::Deg => "deg",
            Unit::Grad => "grad",
            Unit::Rad => "rad",
            Unit::Turn => "turn",
            Unit::S => "s",
            Unit::Ms => "ms",
            Unit::Hz => "Hz",
            Unit::Khz => "kHz",
            Unit::Dpi => "dpi",
            Unit::Dpcm => "dpcm",
            Unit::Dppx => "dppx",
            Unit::X => "x",
            Unit::Fr => "fr",
            Unit::None => "",
            Unit::Mul(u) => {
                return u
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<String>>()
                    .join("*")
            }
            Unit::Unknown(ref s) => s,
        }
        .into()
    }
}

impl fmt::Display for Unit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Unit::Px => write!(f, "px"),
            Unit::Mm => write!(f, "mm"),
            Unit::In => write!(f, "in"),
            Unit::Cm => write!(f, "cm"),
            Unit::Q => write!(f, "q"),
            Unit::Pt => write!(f, "pt"),
            Unit::Pc => write!(f, "pc"),
            Unit::Em => write!(f, "em"),
            Unit::Rem => write!(f, "rem"),
            Unit::Lh => write!(f, "lh"),
            Unit::Percent => write!(f, "%"),
            Unit::Ex => write!(f, "ex"),
            Unit::Ch => write!(f, "ch"),
            Unit::Cap => write!(f, "cap"),
            Unit::Ic => write!(f, "ic"),
            Unit::Rlh => write!(f, "rlh"),
            Unit::Vw => write!(f, "vw"),
            Unit::Vh => write!(f, "vh"),
            Unit::Vmin => write!(f, "vmin"),
            Unit::Vmax => write!(f, "vmax"),
            Unit::Vi => write!(f, "vi"),
            Unit::Vb => write!(f, "vb"),
            Unit::Deg => write!(f, "deg"),
            Unit::Grad => write!(f, "grad"),
            Unit::Rad => write!(f, "rad"),
            Unit::Turn => write!(f, "turn"),
            Unit::S => write!(f, "s"),
            Unit::Ms => write!(f, "ms"),
            Unit::Hz => write!(f, "Hz"),
            Unit::Khz => write!(f, "kHz"),
            Unit::Dpi => write!(f, "dpi"),
            Unit::Dpcm => write!(f, "dpcm"),
            Unit::Dppx => write!(f, "dppx"),
            Unit::X => write!(f, "x"),
            Unit::Fr => write!(f, "fr"),
            Unit::Unknown(s) => write!(f, "{}", s),
            Unit::None => write!(f, ""),
            Unit::Mul(u) => write!(
                f,
                "{}",
                u.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<String>>()
                    .join("*")
            ),
        }
    }
}
