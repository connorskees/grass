//! A big dictionary of units and their conversion ratios.
//!
//! Arbitrary precision is retained.

use std::collections::HashMap;
use std::f64::consts::PI;

use num_traits::One;
use once_cell::sync::Lazy;

use crate::value::Number;

pub(crate) static UNIT_CONVERSION_TABLE: Lazy<
    HashMap<&'static str, HashMap<&'static str, Number>>,
> = Lazy::new(|| {
    let mut from_in = HashMap::new();
    from_in.insert("in", Number::one());
    from_in.insert("cm", Number::one() / Number::from(2.54));
    from_in.insert("pc", Number::machine_ratio(1, 6));
    from_in.insert("mm", Number::one() / Number::from(25.4));
    from_in.insert("q", Number::one() / Number::from(101.6));
    from_in.insert("pt", Number::machine_ratio(1, 72));
    from_in.insert("px", Number::machine_ratio(1, 96));

    let mut from_cm = HashMap::new();
    from_cm.insert("in", Number::from(2.54));
    from_cm.insert("cm", Number::one());
    from_cm.insert("pc", Number::from(2.54) / Number::from(6));
    from_cm.insert("mm", Number::machine_ratio(1, 10));
    from_cm.insert("q", Number::machine_ratio(1, 40));
    from_cm.insert("pt", Number::from(2.54) / Number::from(72));
    from_cm.insert("px", Number::from(2.54) / Number::from(96));

    let mut from_pc = HashMap::new();
    from_pc.insert("in", Number::from(6));
    from_pc.insert("cm", Number::from(6) / Number::from(2.54));
    from_pc.insert("pc", Number::one());
    from_pc.insert("mm", Number::from(6) / Number::from(25.4));
    from_pc.insert("q", Number::from(6) / Number::from(101.6));
    from_pc.insert("pt", Number::machine_ratio(1, 12));
    from_pc.insert("px", Number::machine_ratio(1, 16));

    let mut from_mm = HashMap::new();
    from_mm.insert("in", Number::from(25.4));
    from_mm.insert("cm", Number::from(10));
    from_mm.insert("pc", Number::from(25.4) / Number::from(6));
    from_mm.insert("mm", Number::one());
    from_mm.insert("q", Number::machine_ratio(1, 4));
    from_mm.insert("pt", Number::from(25.4) / Number::from(72));
    from_mm.insert("px", Number::from(25.4) / Number::from(96));

    let mut from_q = HashMap::new();
    from_q.insert("in", Number::from(101.6));
    from_q.insert("cm", Number::from(40));
    from_q.insert("pc", Number::from(101.6) / Number::from(6));
    from_q.insert("mm", Number::from(4));
    from_q.insert("q", Number::one());
    from_q.insert("pt", Number::from(101.6) / Number::from(72));
    from_q.insert("px", Number::from(101.6) / Number::from(96));

    let mut from_pt = HashMap::new();
    from_pt.insert("in", Number::from(72));
    from_pt.insert("cm", Number::from(72) / Number::from(2.54));
    from_pt.insert("pc", Number::from(12));
    from_pt.insert("mm", Number::from(72) / Number::from(25.4));
    from_pt.insert("q", Number::from(72) / Number::from(101.6));
    from_pt.insert("pt", Number::one());
    from_pt.insert("px", Number::machine_ratio(3, 4));

    let mut from_px = HashMap::new();
    from_px.insert("in", Number::from(96));
    from_px.insert("cm", Number::from(96) / Number::from(2.54));
    from_px.insert("pc", Number::from(16));
    from_px.insert("mm", Number::from(96) / Number::from(25.4));
    from_px.insert("q", Number::from(96) / Number::from(101.6));
    from_px.insert("pt", Number::machine_ratio(4, 3));
    from_px.insert("px", Number::one());

    let mut from_deg = HashMap::new();
    from_deg.insert("deg", Number::one());
    from_deg.insert("grad", Number::machine_ratio(9, 10));
    from_deg.insert("rad", Number::from(180) / Number::from(PI));
    from_deg.insert("turn", Number::from(360));

    let mut from_grad = HashMap::new();
    from_grad.insert("deg", Number::machine_ratio(10, 9));
    from_grad.insert("grad", Number::one());
    from_grad.insert("rad", Number::from(200) / Number::from(PI));
    from_grad.insert("turn", Number::from(400));

    let mut from_rad = HashMap::new();
    from_rad.insert("deg", Number::from(PI) / Number::from(180));
    from_rad.insert("grad", Number::from(PI) / Number::from(200));
    from_rad.insert("rad", Number::one());
    from_rad.insert("turn", Number::from(2.0 * PI));

    let mut from_turn = HashMap::new();
    from_turn.insert("deg", Number::machine_ratio(1, 360));
    from_turn.insert("grad", Number::machine_ratio(1, 400));
    from_turn.insert("rad", Number::one() / Number::from(2.0 * PI));
    from_turn.insert("turn", Number::one());

    let mut from_s = HashMap::new();
    from_s.insert("s", Number::one());
    from_s.insert("ms", Number::machine_ratio(1, 1000));

    let mut from_ms = HashMap::new();
    from_ms.insert("s", Number::from(1000));
    from_ms.insert("ms", Number::one());

    let mut from_hz = HashMap::new();
    from_hz.insert("Hz", Number::one());
    from_hz.insert("kHz", Number::from(1000));

    let mut from_khz = HashMap::new();
    from_khz.insert("Hz", Number::machine_ratio(1, 1000));
    from_khz.insert("kHz", Number::one());

    let mut from_dpi = HashMap::new();
    from_dpi.insert("dpi", Number::one());
    from_dpi.insert("dpcm", Number::from(2.54));
    from_dpi.insert("dppx", Number::from(96));

    let mut from_dpcm = HashMap::new();
    from_dpcm.insert("dpi", Number::one() / Number::from(2.54));
    from_dpcm.insert("dpcm", Number::one());
    from_dpcm.insert("dppx", Number::from(96) / Number::from(2.54));

    let mut from_dppx = HashMap::new();
    from_dppx.insert("dpi", Number::machine_ratio(1, 96));
    from_dppx.insert("dpcm", Number::from(2.54) / Number::from(96));
    from_dppx.insert("dppx", Number::one());

    let mut m = HashMap::new();
    m.insert("in", from_in);
    m.insert("cm", from_cm);
    m.insert("pc", from_pc);
    m.insert("mm", from_mm);
    m.insert("q", from_q);
    m.insert("pt", from_pt);
    m.insert("px", from_px);

    m.insert("deg", from_deg);
    m.insert("grad", from_grad);
    m.insert("rad", from_rad);
    m.insert("turn", from_turn);

    m.insert("s", from_s);
    m.insert("ms", from_ms);

    m.insert("Hz", from_hz);
    m.insert("kHz", from_khz);

    m.insert("dpi", from_dpi);
    m.insert("dpcm", from_dpcm);
    m.insert("dppx", from_dppx);

    m
});
