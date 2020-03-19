//! A big dictionary of units and their conversion ratios.
//!
//! Arbitrary precision is retained.

use std::collections::HashMap;
use std::f64::consts::PI;
use std::string::ToString;

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
