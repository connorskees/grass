use std::convert::TryFrom;
use std::fmt;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Unit {
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
    Percent,
    /// x-height of the element's font
    Ex,
    /// The advance measure (width) of the glyph "0" of the element's font
    Ch,
    /// Represents the "cap height" (nominal height of capital letters) of the element’s font
    Cap,
    /// Equal to the used advance measure of the "水" (CJK water ideograph, U+6C34) glyph found in the font used to render it
    Ic,
    /// Equal to the computed value of the line-height property on the root element (typically <html>), converted to an absolute length
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
    /// Equal to 1% of the size of the initial containing block, in the direction of the root element’s inline axis
    Vi,
    /// Equal to 1% of the size of the initial containing block, in the direction of the root element’s block axis
    Vb,

    // Angle units
    /// Represents an angle in degrees. One full circle is 360deg
    Deg,
    /// Represents an angle in gradians. One full circle is 400grad
    Grad,
    /// Represents an angle in radians. One full circle is 2π radians which approximates to 6.2832rad
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

    /// Represents a fraction of the available space in the grid container
    Fr,
    /// Unspecified unit
    None,
}

impl TryFrom<&str> for Unit {
    type Error = &'static str;

    fn try_from(unit: &str) -> Result<Self, Self::Error> {
        match unit.to_ascii_lowercase().as_bytes() {
            b"px" => Ok(Unit::Px),
            b"mm" => Ok(Unit::Mm),
            b"in" => Ok(Unit::In),
            b"cm" => Ok(Unit::Cm),
            b"Q" => Ok(Unit::Q),
            b"pt" => Ok(Unit::Pt),
            b"pc" => Ok(Unit::Pc),
            b"em" => Ok(Unit::Em),
            b"rem" => Ok(Unit::Rem),
            b"lh" => Ok(Unit::Lh),
            b"%" => Ok(Unit::Percent),
            b"ex" => Ok(Unit::Ex),
            b"ch" => Ok(Unit::Ch),
            b"cap" => Ok(Unit::Cap),
            b"ic" => Ok(Unit::Ic),
            b"rlh" => Ok(Unit::Rlh),
            b"vw" => Ok(Unit::Vw),
            b"vh" => Ok(Unit::Vh),
            b"vmin" => Ok(Unit::Vmin),
            b"vmax" => Ok(Unit::Vmax),
            b"vi" => Ok(Unit::Vi),
            b"vb" => Ok(Unit::Vb),
            b"deg" => Ok(Unit::Deg),
            b"grad" => Ok(Unit::Grad),
            b"rad" => Ok(Unit::Rad),
            b"turn" => Ok(Unit::Turn),
            b"s" => Ok(Unit::S),
            b"ms" => Ok(Unit::Ms),
            b"Hz" => Ok(Unit::Hz),
            b"kHz" => Ok(Unit::Khz),
            b"dpi" => Ok(Unit::Dpi),
            b"dpcm" => Ok(Unit::Dpcm),
            b"dppx" => Ok(Unit::Dppx),
            b"x" => Ok(Unit::X),
            b"fr" => Ok(Unit::Fr),
            _ => Err("invalid unit"),
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
            Unit::Q => "Q",
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
        }
        .into()
    }
}

impl Into<&'static str> for Unit {
    fn into(self) -> &'static str {
        match self {
            Unit::Px => "px",
            Unit::Mm => "mm",
            Unit::In => "in",
            Unit::Cm => "cm",
            Unit::Q => "Q",
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
        }
    }
}

impl fmt::Display for Unit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Unit::Px => write!(f, "px"),
            Unit::Mm => write!(f, "mm"),
            Unit::In => write!(f, "in"),
            Unit::Cm => write!(f, "cm"),
            Unit::Q => write!(f, "Q"),
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
            Unit::None => write!(f, ""),
        }
    }
}
