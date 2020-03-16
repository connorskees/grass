use std::collections::HashMap;

use super::{Builtin, GLOBAL_FUNCTIONS};
use crate::common::QuoteKind;
use crate::units::Unit;
use crate::value::Value;

pub(crate) fn register(f: &mut HashMap<String, Builtin>) {
    f.insert(
        "if".to_owned(),
        Box::new(|args, _| {
            max_args!(args, 3);
            if arg!(args, 0, "condition").is_true()? {
                Ok(arg!(args, 1, "if-true"))
            } else {
                Ok(arg!(args, 2, "if-false"))
            }
        }),
    );
    f.insert(
        "feature-exists".to_owned(),
        Box::new(|args, _| {
            max_args!(args, 1);
            match arg!(args, 0, "feature").unquote().to_string().as_str() {
                // A local variable will shadow a global variable unless
                // `!global` is used.
                "global-variable-shadowing" => Ok(Value::False),
                // the @extend rule will affect selectors nested in pseudo-classes
                // like :not()
                "extend-selector-pseudoclass" => Ok(Value::False),
                // Full support for unit arithmetic using units defined in the
                // [Values and Units Level 3][] spec.
                "units-level-3" => Ok(Value::False),
                // The Sass `@error` directive is supported.
                "at-error" => Ok(Value::True),
                // The "Custom Properties Level 1" spec is supported. This means
                // that custom properties are parsed statically, with only
                // interpolation treated as SassScript.
                "custom-property" => Ok(Value::False),
                _ => Ok(Value::False),
            }
        }),
    );
    f.insert(
        "unit".to_owned(),
        Box::new(|args, _| {
            max_args!(args, 1);
            let unit = match arg!(args, 0, "number") {
                Value::Dimension(_, u) => u.to_string(),
                _ => String::new(),
            };
            Ok(Value::Ident(unit, QuoteKind::Double))
        }),
    );
    f.insert(
        "type-of".to_owned(),
        Box::new(|args, _| {
            max_args!(args, 1);
            let value = arg!(args, 0, "value");
            Ok(Value::Ident(value.kind()?.to_owned(), QuoteKind::None))
        }),
    );
    f.insert(
        "unitless".to_owned(),
        Box::new(|args, _| {
            max_args!(args, 1);
            match arg!(args, 0, "number") {
                Value::Dimension(_, Unit::None) => Ok(Value::True),
                Value::Dimension(_, _) => Ok(Value::False),
                _ => Ok(Value::True),
            }
        }),
    );
    f.insert(
        "inspect".to_owned(),
        Box::new(|args, _| {
            max_args!(args, 1);
            let value = arg!(args, 0, "value");
            Ok(Value::Ident(value.to_string(), QuoteKind::None))
        }),
    );
    f.insert(
        "variable-exists".to_owned(),
        Box::new(|args, scope| {
            max_args!(args, 1);
            let value = arg!(args, 0, "name");
            Ok(Value::bool(scope.var_exists(&value.to_string())))
        }),
    );
    f.insert(
        "mixin-exists".to_owned(),
        Box::new(|args, scope| {
            max_args!(args, 1);
            let value = arg!(args, 0, "name");
            Ok(Value::bool(scope.mixin_exists(&value.to_string())))
        }),
    );
    f.insert(
        "function-exists".to_owned(),
        Box::new(|args, scope| {
            max_args!(args, 1);
            let value = arg!(args, 0, "name");
            let s = value.unquote().to_string();
            Ok(Value::bool(
                scope.fn_exists(&s) || GLOBAL_FUNCTIONS.contains_key(&s),
            ))
        }),
    );
    f.insert("call".to_owned(), Box::new(|_args, _scope| {
        todo!("builtin function `call()` is blocked on refactoring how call args are stored and parsed")
        // let func = arg!(args, 0, "function").to_string();
        // let func = match scope.get_fn(&func) {
        //     Ok(f) => f,
        //     Err(_) => match GLOBAL_FUNCTIONS.get(&func) {
        //         Some(f) => return f(&args, scope),
        //         None => todo!("called undefined function"),
        //     },
        // };
        // Some(func.clone().args(&args).call())
    }));
}
