use std::collections::BTreeMap;

use super::Builtin;
use crate::value::Value;
use crate::color::Color;

pub(crate) fn register(f: &mut BTreeMap<String, Builtin>) {
    f.insert(
        "rgb".to_owned(),
        Box::new(|args| {
            let channels = args.get("channels").unwrap_or(&Value::Null);
            if channels.is_null() {
                let _red: &Value = arg!(args, 0, "red");
                let _green: &Value = arg!(args, 1, "green");
                let _blue: &Value = arg!(args, 2, "blue");
            // Value::Color(Color::RGB(red, blue, green))
            } else {
                todo!("channels variable in `rgb`")
            };
            todo!()
        }),
    );
}
