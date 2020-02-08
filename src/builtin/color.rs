use std::collections::BTreeMap;
use std::convert::TryInto;

use super::Builtin;
use crate::color::Color;
use crate::value::Value;

pub(crate) fn register(f: &mut BTreeMap<String, Builtin>) {
    decl!(f "rgb", |args| {
        let channels = args.get("channels").unwrap_or(&Value::Null);
        if channels.is_null() {
            let red: u16 = arg!(args, 0, "red").clone().try_into().unwrap();
            let green: u16 = arg!(args, 1, "green").clone().try_into().unwrap();
            let blue: u16 = arg!(args, 2, "blue").clone().try_into().unwrap();
            Some(Value::Color(Color::from_values(red, green, blue, 1)))
        } else {
            todo!("channels variable in `rgb`")
        }
    });
}
