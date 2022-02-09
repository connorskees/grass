use std::{
    fmt,
    ops::{Div, Mul},
};

use crate::interner::InternedString;

pub(crate) use conversion::UNIT_CONVERSION_TABLE;

mod conversion;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
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
    Unknown(InternedString),
    /// Unspecified unit
    None,

    /// Units multiplied together
    /// Boxed under the assumption that mul units are exceedingly rare
    #[allow(clippy::box_collection)]
    Mul(Box<Vec<Unit>>),

    /// Units divided by each other
    Div(Box<DivUnit>),
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

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) struct DivUnit {
    numer: Unit,
    denom: Unit,
}

impl DivUnit {
    pub const fn new(numer: Unit, denom: Unit) -> Self {
        Self { numer, denom }
    }
}

impl fmt::Display for DivUnit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.numer == Unit::None {
            write!(f, "{}^-1", self.denom)
        } else {
            write!(f, "{}/{}", self.numer, self.denom)
        }
    }
}

#[allow(clippy::match_same_arms)]
impl Mul<Unit> for DivUnit {
    type Output = Unit;
    fn mul(self, rhs: Unit) -> Self::Output {
        match rhs {
            Unit::Mul(..) => todo!(),
            Unit::Div(..) => todo!(),
            Unit::None => todo!(),
            _ => {
                if self.denom == rhs {
                    self.numer
                } else {
                    match self.denom {
                        Unit::Mul(..) => todo!(),
                        Unit::Div(..) => unreachable!(),
                        _ => match self.numer {
                            Unit::Mul(..) => todo!(),
                            Unit::Div(..) => unreachable!(),
                            Unit::None => {
                                let numer = Unit::Mul(Box::new(vec![rhs]));
                                Unit::Div(Box::new(DivUnit::new(numer, self.denom)))
                            }
                            _ => {
                                let numer = Unit::Mul(Box::new(vec![self.numer, rhs]));
                                Unit::Div(Box::new(DivUnit::new(numer, self.denom)))
                            }
                        },
                    }
                }
            }
        }
    }
}

// impl Div<Unit> for DivUnit {
//     type Output = Unit;
//     fn div(self, rhs: Unit) -> Self::Output {
//         todo!()
//     }
// }

impl Mul<Unit> for Unit {
    type Output = Unit;
    fn mul(self, rhs: Unit) -> Self::Output {
        match self {
            Unit::Mul(u) => match rhs {
                Unit::Mul(u2) => {
                    let mut unit1 = *u;
                    unit1.extend_from_slice(&*u2);
                    Unit::Mul(Box::new(unit1))
                }
                Unit::Div(..) => todo!(),
                _ => {
                    let mut unit1 = *u;
                    unit1.push(rhs);
                    Unit::Mul(Box::new(unit1))
                }
            },
            Unit::Div(div) => *div * rhs,
            _ => match rhs {
                Unit::Mul(u2) => {
                    let mut unit1 = vec![self];
                    unit1.extend_from_slice(&*u2);
                    Unit::Mul(Box::new(unit1))
                }
                Unit::Div(..) => todo!(),
                _ => Unit::Mul(Box::new(vec![self, rhs])),
            },
        }
    }
}

impl Div<Unit> for Unit {
    type Output = Unit;
    #[allow(clippy::if_same_then_else)]
    fn div(self, rhs: Unit) -> Self::Output {
        if let Unit::Div(..) = self {
            todo!()
        } else if let Unit::Div(..) = rhs {
            todo!()
        } else {
            Unit::Div(Box::new(DivUnit::new(self, rhs)))
        }
    }
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
            Unit::Fr | Unit::Percent | Unit::Unknown(..) | Unit::Mul(..) | Unit::Div(..) => {
                UnitKind::Other
            }
        }
    }
}

impl From<String> for Unit {
    fn from(unit: String) -> Self {
        match unit.to_ascii_lowercase().as_str() {
            "px" => Unit::Px,
            "mm" => Unit::Mm,
            "in" => Unit::In,
            "cm" => Unit::Cm,
            "q" => Unit::Q,
            "pt" => Unit::Pt,
            "pc" => Unit::Pc,
            "em" => Unit::Em,
            "rem" => Unit::Rem,
            "lh" => Unit::Lh,
            "%" => Unit::Percent,
            "ex" => Unit::Ex,
            "ch" => Unit::Ch,
            "cap" => Unit::Cap,
            "ic" => Unit::Ic,
            "rlh" => Unit::Rlh,
            "vw" => Unit::Vw,
            "vh" => Unit::Vh,
            "vmin" => Unit::Vmin,
            "vmax" => Unit::Vmax,
            "vi" => Unit::Vi,
            "vb" => Unit::Vb,
            "deg" => Unit::Deg,
            "grad" => Unit::Grad,
            "rad" => Unit::Rad,
            "turn" => Unit::Turn,
            "s" => Unit::S,
            "ms" => Unit::Ms,
            "hz" => Unit::Hz,
            "khz" => Unit::Khz,
            "dpi" => Unit::Dpi,
            "dpcm" => Unit::Dpcm,
            "dppx" => Unit::Dppx,
            "x" => Unit::X,
            "fr" => Unit::Fr,
            _ => Unit::Unknown(InternedString::get_or_intern(unit)),
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
            Unit::None => Ok(()),
            Unit::Mul(u) => write!(
                f,
                "{}",
                u.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<String>>()
                    .join("*")
            ),
            Unit::Div(u) => write!(f, "{}", u),
        }
    }
}
