//! A big dictionary of units and their conversion ratios.
//!
//! Arbitrary precision is retained.

use std::{collections::HashMap, f64::consts::PI};

use num_traits::One;
use once_cell::sync::Lazy;

use crate::{unit::Unit, value::Number};

pub(crate) static UNIT_CONVERSION_TABLE: Lazy<HashMap<Unit, HashMap<Unit, Number>>> =
    Lazy::new(|| {
        let mut from_in = HashMap::new();
        from_in.insert(Unit::In, Number::one());
        from_in.insert(Unit::Cm, Number::one() / Number::from(2.54));
        from_in.insert(Unit::Pc, Number::small_ratio(1, 6));
        from_in.insert(Unit::Mm, Number::one() / Number::from(25.4));
        from_in.insert(Unit::Q, Number::one() / Number::from(101.6));
        from_in.insert(Unit::Pt, Number::small_ratio(1, 72));
        from_in.insert(Unit::Px, Number::small_ratio(1, 96));

        let mut from_cm = HashMap::new();
        from_cm.insert(Unit::In, Number::from(2.54));
        from_cm.insert(Unit::Cm, Number::one());
        from_cm.insert(Unit::Pc, Number::from(2.54) / Number::from(6));
        from_cm.insert(Unit::Mm, Number::small_ratio(1, 10));
        from_cm.insert(Unit::Q, Number::small_ratio(1, 40));
        from_cm.insert(Unit::Pt, Number::from(2.54) / Number::from(72));
        from_cm.insert(Unit::Px, Number::from(2.54) / Number::from(96));

        let mut from_pc = HashMap::new();
        from_pc.insert(Unit::In, Number::from(6));
        from_pc.insert(Unit::Cm, Number::from(6) / Number::from(2.54));
        from_pc.insert(Unit::Pc, Number::one());
        from_pc.insert(Unit::Mm, Number::from(6) / Number::from(25.4));
        from_pc.insert(Unit::Q, Number::from(6) / Number::from(101.6));
        from_pc.insert(Unit::Pt, Number::small_ratio(1, 12));
        from_pc.insert(Unit::Px, Number::small_ratio(1, 16));

        let mut from_mm = HashMap::new();
        from_mm.insert(Unit::In, Number::from(25.4));
        from_mm.insert(Unit::Cm, Number::from(10));
        from_mm.insert(Unit::Pc, Number::from(25.4) / Number::from(6));
        from_mm.insert(Unit::Mm, Number::one());
        from_mm.insert(Unit::Q, Number::small_ratio(1, 4));
        from_mm.insert(Unit::Pt, Number::from(25.4) / Number::from(72));
        from_mm.insert(Unit::Px, Number::from(25.4) / Number::from(96));

        let mut from_q = HashMap::new();
        from_q.insert(Unit::In, Number::from(101.6));
        from_q.insert(Unit::Cm, Number::from(40));
        from_q.insert(Unit::Pc, Number::from(101.6) / Number::from(6));
        from_q.insert(Unit::Mm, Number::from(4));
        from_q.insert(Unit::Q, Number::one());
        from_q.insert(Unit::Pt, Number::from(101.6) / Number::from(72));
        from_q.insert(Unit::Px, Number::from(101.6) / Number::from(96));

        let mut from_pt = HashMap::new();
        from_pt.insert(Unit::In, Number::from(72));
        from_pt.insert(Unit::Cm, Number::from(72) / Number::from(2.54));
        from_pt.insert(Unit::Pc, Number::from(12));
        from_pt.insert(Unit::Mm, Number::from(72) / Number::from(25.4));
        from_pt.insert(Unit::Q, Number::from(72) / Number::from(101.6));
        from_pt.insert(Unit::Pt, Number::one());
        from_pt.insert(Unit::Px, Number::small_ratio(3, 4));

        let mut from_px = HashMap::new();
        from_px.insert(Unit::In, Number::from(96));
        from_px.insert(Unit::Cm, Number::from(96) / Number::from(2.54));
        from_px.insert(Unit::Pc, Number::from(16));
        from_px.insert(Unit::Mm, Number::from(96) / Number::from(25.4));
        from_px.insert(Unit::Q, Number::from(96) / Number::from(101.6));
        from_px.insert(Unit::Pt, Number::small_ratio(4, 3));
        from_px.insert(Unit::Px, Number::one());

        let mut from_deg = HashMap::new();
        from_deg.insert(Unit::Deg, Number::one());
        from_deg.insert(Unit::Grad, Number::small_ratio(9, 10));
        from_deg.insert(Unit::Rad, Number::from(180) / Number::from(PI));
        from_deg.insert(Unit::Turn, Number::from(360));

        let mut from_grad = HashMap::new();
        from_grad.insert(Unit::Deg, Number::small_ratio(10, 9));
        from_grad.insert(Unit::Grad, Number::one());
        from_grad.insert(Unit::Rad, Number::from(200) / Number::from(PI));
        from_grad.insert(Unit::Turn, Number::from(400));

        let mut from_rad = HashMap::new();
        from_rad.insert(Unit::Deg, Number::from(PI) / Number::from(180));
        from_rad.insert(Unit::Grad, Number::from(PI) / Number::from(200));
        from_rad.insert(Unit::Rad, Number::one());
        from_rad.insert(Unit::Turn, Number::from(2.0 * PI));

        let mut from_turn = HashMap::new();
        from_turn.insert(Unit::Deg, Number::small_ratio(1, 360));
        from_turn.insert(Unit::Grad, Number::small_ratio(1, 400));
        from_turn.insert(Unit::Rad, Number::one() / Number::from(2.0 * PI));
        from_turn.insert(Unit::Turn, Number::one());

        let mut from_s = HashMap::new();
        from_s.insert(Unit::S, Number::one());
        from_s.insert(Unit::Ms, Number::small_ratio(1, 1000));

        let mut from_ms = HashMap::new();
        from_ms.insert(Unit::S, Number::from(1000));
        from_ms.insert(Unit::Ms, Number::one());

        let mut from_hz = HashMap::new();
        from_hz.insert(Unit::Hz, Number::one());
        from_hz.insert(Unit::Khz, Number::from(1000));

        let mut from_khz = HashMap::new();
        from_khz.insert(Unit::Hz, Number::small_ratio(1, 1000));
        from_khz.insert(Unit::Khz, Number::one());

        let mut from_dpi = HashMap::new();
        from_dpi.insert(Unit::Dpi, Number::one());
        from_dpi.insert(Unit::Dpcm, Number::from(2.54));
        from_dpi.insert(Unit::Dppx, Number::from(96));

        let mut from_dpcm = HashMap::new();
        from_dpcm.insert(Unit::Dpi, Number::one() / Number::from(2.54));
        from_dpcm.insert(Unit::Dpcm, Number::one());
        from_dpcm.insert(Unit::Dppx, Number::from(96) / Number::from(2.54));

        let mut from_dppx = HashMap::new();
        from_dppx.insert(Unit::Dpi, Number::small_ratio(1, 96));
        from_dppx.insert(Unit::Dpcm, Number::from(2.54) / Number::from(96));
        from_dppx.insert(Unit::Dppx, Number::one());

        let mut m = HashMap::new();
        m.insert(Unit::In, from_in);
        m.insert(Unit::Cm, from_cm);
        m.insert(Unit::Pc, from_pc);
        m.insert(Unit::Mm, from_mm);
        m.insert(Unit::Q, from_q);
        m.insert(Unit::Pt, from_pt);
        m.insert(Unit::Px, from_px);

        m.insert(Unit::Deg, from_deg);
        m.insert(Unit::Grad, from_grad);
        m.insert(Unit::Rad, from_rad);
        m.insert(Unit::Turn, from_turn);

        m.insert(Unit::S, from_s);
        m.insert(Unit::Ms, from_ms);

        m.insert(Unit::Hz, from_hz);
        m.insert(Unit::Khz, from_khz);

        m.insert(Unit::Dpi, from_dpi);
        m.insert(Unit::Dpcm, from_dpcm);
        m.insert(Unit::Dppx, from_dppx);

        m
    });
