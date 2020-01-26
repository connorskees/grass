use std::collections::BTreeMap;

use super::Builtin;
use crate::value::Value;

macro_rules! arg {
    ($args:ident, $idx:literal, $name:literal) => {
        match $args.get(stringify!($idx)) {
            Some(v) => v,
            None => match $args.get($name) {
                Some(v) => v,
                None => panic!("missing variable"),
            },
        };
    };
    ($args:ident, $idx:literal, $name:literal, $default:literal) => {
        match $args.get(stringify!($idx)) {
            Some(v) => v,
            None => match $args.get($name) {
                Some(v) => v,
                None => $default,
            },
        };
    };
}

pub(crate) fn register(f: &mut BTreeMap<String, Builtin>) {
    f.insert(
        "rgb".to_owned(),
        Box::new(|args| {
            let channels = args.get("channels").unwrap_or(&Value::Null);
            if channels.is_null() {
                let _red: &Value = arg!(args, 0, "red");
                let _green: &Value = arg!(args, 1, "green");
                let _blue: &Value = arg!(args, 2, "blue");
            // Value::Color()
            } else {
                todo!("channels variable in `rgb`")
            };
            todo!()
        }),
    );
}
