use std::collections::BTreeMap;

use super::Builtin;
use crate::common::QuoteKind;
use crate::units::Unit;
use crate::value::Value;

pub(crate) fn register(f: &mut BTreeMap<String, Builtin>) {
    decl!(f "if", |args, _| {
        let cond: &Value = arg!(args, 0, "condition");
        let if_true = arg!(args, 1, "if-true").clone();
        let if_false = arg!(args, 2, "if-false").clone();
        if cond.is_true() {
            Some(if_true)
        } else {
            Some(if_false)
        }
    });
    decl!(f "feature-exists", |args, _| {
        let feature: &Value = arg!(args, 0, "feature");
        match feature.clone().unquote().to_string().as_str() {
            // A local variable will shadow a global variable unless
            // `!global` is used.
            "global-variable-shadowing" => Some(Value::False),
            // the @extend rule will affect selectors nested in pseudo-classes
            // like :not()
            "extend-selector-pseudoclass" => Some(Value::False),
            // Full support for unit arithmetic using units defined in the
            // [Values and Units Level 3][] spec.
            "units-level-3" => Some(Value::False),
            // The Sass `@error` directive is supported.
            "at-error" => Some(Value::True),
            // The "Custom Properties Level 1" spec is supported. This means
            // that custom properties are parsed statically, with only
            // interpolation treated as SassScript.
            "custom-property" => Some(Value::False),
            _ => Some(Value::False),
        }
    });
    decl!(f "unit", |args, _| {
        let number = arg!(args, 0, "number");
        let unit = match number {
            Value::Dimension(_, u) => u.to_string(),
            _ => String::new()
        };
        Some(Value::Ident(unit, QuoteKind::Double))
    });
    decl!(f "type-of", |args, _| {
        let value = arg!(args, 0, "value");
        Some(Value::Ident(value.kind().to_owned(), QuoteKind::None))
    });
    decl!(f "unitless", |args, _| {
        let number = arg!(args, 0, "number");
        match number {
            Value::Dimension(_, Unit::None) => Some(Value::True),
            Value::Dimension(_, _) => Some(Value::False),
            _ => Some(Value::True)
        }
    });
    decl!(f "inspect", |args, _| {
        let value = arg!(args, 0, "value");
        Some(Value::Ident(value.to_string(), QuoteKind::None))
    });
    decl!(f "variable-exists", |args, scope| {
        let value = arg!(args, 0, "name");
        Some(Value::bool(scope.var_exists(&value.to_string())))
    });
    decl!(f "mixin-exists", |args, scope| {
        let value = arg!(args, 0, "name");
        Some(Value::bool(scope.mixin_exists(&value.to_string())))
    });
    decl!(f "function-exists", |args, scope| {
        let value = arg!(args, 0, "name");
        Some(Value::bool(scope.fn_exists(&value.to_string())))
    });
}
