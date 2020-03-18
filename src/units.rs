use std::collections::HashMap;
use std::f64::consts::PI;
use std::fmt;

use once_cell::sync::Lazy;

use crate::value::Number;

pub(crate) static UNIT_CONVERSION_TABLE: Lazy<HashMap<String, HashMap<String, Number>>> =
    Lazy::new(|| {
        let mut from_in = HashMap::new();
        from_in.insert("in".to_string(), Number::from(1));
        from_in.insert("cm".to_string(), Number::from(1) / Number::from(2.54));
        from_in.insert("pc".to_string(), Number::ratio(1, 6));
        from_in.insert("mm".to_string(), Number::from(1) / Number::from(25.4));
        from_in.insert("q".to_string(), Number::from(1) / Number::from(101.6));
        from_in.insert("pt".to_string(), Number::ratio(1, 72));
        from_in.insert("px".to_string(), Number::ratio(1, 96));

        let mut from_cm = HashMap::new();
        from_cm.insert("in".to_string(), Number::from(2.54));
        from_cm.insert("cm".to_string(), Number::from(1));
        from_cm.insert("pc".to_string(), Number::from(2.54) / Number::from(6));
        from_cm.insert("mm".to_string(), Number::ratio(1, 10));
        from_cm.insert("q".to_string(), Number::ratio(1, 40));
        from_cm.insert("pt".to_string(), Number::from(2.54) / Number::from(72));
        from_cm.insert("px".to_string(), Number::from(2.54) / Number::from(96));

        let mut from_pc = HashMap::new();
        from_pc.insert("in".to_string(), Number::from(6));
        from_pc.insert("cm".to_string(), Number::from(6) / Number::from(2.54));
        from_pc.insert("pc".to_string(), Number::from(1));
        from_pc.insert("mm".to_string(), Number::from(6) / Number::from(25.4));
        from_pc.insert("q".to_string(), Number::from(6) / Number::from(101.6));
        from_pc.insert("pt".to_string(), Number::ratio(1, 12));
        from_pc.insert("px".to_string(), Number::ratio(1, 16));

        let mut from_mm = HashMap::new();
        from_mm.insert("in".to_string(), Number::from(25.4));
        from_mm.insert("cm".to_string(), Number::from(10));
        from_mm.insert("pc".to_string(), Number::from(25.4) / Number::from(6));
        from_mm.insert("mm".to_string(), Number::from(1));
        from_mm.insert("q".to_string(), Number::ratio(1, 4));
        from_mm.insert("pt".to_string(), Number::from(25.4) / Number::from(72));
        from_mm.insert("px".to_string(), Number::from(25.4) / Number::from(96));

        let mut from_q = HashMap::new();
        from_q.insert("in".to_string(), Number::from(101.6));
        from_q.insert("cm".to_string(), Number::from(40));
        from_q.insert("pc".to_string(), Number::from(101.6) / Number::from(6));
        from_q.insert("mm".to_string(), Number::from(4));
        from_q.insert("q".to_string(), Number::from(1));
        from_q.insert("pt".to_string(), Number::from(101.6) / Number::from(72));
        from_q.insert("px".to_string(), Number::from(101.6) / Number::from(96));

        let mut from_pt = HashMap::new();
        from_pt.insert("in".to_string(), Number::from(72));
        from_pt.insert("cm".to_string(), Number::from(72) / Number::from(2.54));
        from_pt.insert("pc".to_string(), Number::from(12));
        from_pt.insert("mm".to_string(), Number::from(72) / Number::from(25.4));
        from_pt.insert("q".to_string(), Number::from(72) / Number::from(101.6));
        from_pt.insert("pt".to_string(), Number::from(1));
        from_pt.insert("px".to_string(), Number::ratio(3, 4));

        let mut from_px = HashMap::new();
        from_px.insert("in".to_string(), Number::from(96));
        from_px.insert("cm".to_string(), Number::from(96) / Number::from(2.54));
        from_px.insert("pc".to_string(), Number::from(16));
        from_px.insert("mm".to_string(), Number::from(96) / Number::from(25.4));
        from_px.insert("q".to_string(), Number::from(96) / Number::from(101.6));
        from_px.insert("pt".to_string(), Number::ratio(4, 3));
        from_px.insert("px".to_string(), Number::from(1));

        let mut from_deg = HashMap::new();
        from_deg.insert("deg".to_string(), Number::from(1));
        from_deg.insert("grad".to_string(), Number::ratio(9, 10));
        from_deg.insert("rad".to_string(), Number::from(180) / Number::from(PI));
        from_deg.insert("turn".to_string(), Number::from(360));

        let mut from_grad = HashMap::new();
        from_grad.insert("deg".to_string(), Number::ratio(10, 9));
        from_grad.insert("grad".to_string(), Number::from(1));
        from_grad.insert("rad".to_string(), Number::from(200) / Number::from(PI));
        from_grad.insert("turn".to_string(), Number::from(400));

        let mut from_rad = HashMap::new();
        from_rad.insert("deg".to_string(), Number::from(PI) / Number::from(180));
        from_rad.insert("grad".to_string(), Number::from(PI) / Number::from(200));
        from_rad.insert("rad".to_string(), Number::from(1));
        from_rad.insert("turn".to_string(), Number::from(2.0 * PI));

        let mut from_turn = HashMap::new();
        from_turn.insert("deg".to_string(), Number::ratio(1, 360));
        from_turn.insert("grad".to_string(), Number::ratio(1, 400));
        from_turn.insert("rad".to_string(), Number::from(1) / Number::from(2.0 * PI));
        from_turn.insert("turn".to_string(), Number::from(1));

        let mut from_s = HashMap::new();
        from_s.insert("s".to_string(), Number::from(1));
        from_s.insert("ms".to_string(), Number::ratio(1, 1000));

        let mut from_ms = HashMap::new();
        from_ms.insert("s".to_string(), Number::from(1000));
        from_ms.insert("ms".to_string(), Number::from(1));

        let mut from_hz = HashMap::new();
        from_hz.insert("Hz".to_string(), Number::from(1));
        from_hz.insert("kHz".to_string(), Number::from(1000));

        let mut from_khz = HashMap::new();
        from_khz.insert("Hz".to_string(), Number::ratio(1, 1000));
        from_khz.insert("kHz".to_string(), Number::from(1));

        let mut from_dpi = HashMap::new();
        from_dpi.insert("dpi".to_string(), Number::from(1));
        from_dpi.insert("dpcm".to_string(), Number::from(2.54));
        from_dpi.insert("dppx".to_string(), Number::from(96));

        let mut from_dpcm = HashMap::new();
        from_dpcm.insert("dpi".to_string(), Number::from(1) / Number::from(2.54));
        from_dpcm.insert("dpcm".to_string(), Number::from(1));
        from_dpcm.insert("dppx".to_string(), Number::from(96) / Number::from(2.54));

        let mut from_dppx = HashMap::new();
        from_dppx.insert("dpi".to_string(), Number::ratio(1, 96));
        from_dppx.insert("dpcm".to_string(), Number::from(2.54) / Number::from(96));
        from_dppx.insert("dppx".to_string(), Number::from(1));

        let mut m = HashMap::new();
        m.insert("in".to_string(), from_in);
        m.insert("cm".to_string(), from_cm);
        m.insert("pc".to_string(), from_pc);
        m.insert("mm".to_string(), from_mm);
        m.insert("q".to_string(), from_q);
        m.insert("pt".to_string(), from_pt);
        m.insert("px".to_string(), from_px);

        m.insert("deg".to_string(), from_deg);
        m.insert("grad".to_string(), from_grad);
        m.insert("rad".to_string(), from_rad);
        m.insert("turn".to_string(), from_turn);

        m.insert("s".to_string(), from_s);
        m.insert("ms".to_string(), from_ms);

        m.insert("Hz".to_string(), from_hz);
        m.insert("kHz".to_string(), from_khz);

        m.insert("dpi".to_string(), from_dpi);
        m.insert("dpcm".to_string(), from_dpcm);
        m.insert("dppx".to_string(), from_dppx);

        m
    });

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
    /// A unit divided by another
    Div(Box<Unit>, Box<Unit>),
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
        match self.kind() {
            UnitKind::FontRelative | UnitKind::ViewportRelative | UnitKind::Other => self == other,
            UnitKind::None => true,
            u => other.kind() == u || other.kind() == UnitKind::None,
        }
    }

    pub fn kind(&self) -> UnitKind {
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
            _ => UnitKind::Other,
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
                return format!(
                    "{}",
                    u.into_iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<String>>()
                        .join("*")
                )
            }
            Unit::Div(l, r) => return format!("{}/{}", l, r),
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
                u.into_iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join("*")
            ),
            Unit::Div(l, r) => write!(f, "{}/{}", l, r),
        }
    }
}
